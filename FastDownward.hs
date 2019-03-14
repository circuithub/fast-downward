{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

{-| This module exposes a small DSL for building and solving planning problems
using <http://www.fast-downward.org Fast Downward> - an open source solver for
<https://en.wikipedia.org/wiki/Automated_planning_and_scheduling classical planning problems>.

Using this module, you model problems with a finite-domain representation
through state variables (see, 'Var', 'newVar'), and model their changes through
'Effect's (see 'readVar', and 'writeVar'). If you're familiar with software
transactional memory, an effect is like a transaction, except the process of
solving will choose the appropriate sequence for you.
-}

module FastDownward
  ( -- * Defining Problems
    Problem

    -- ** @Var@iables
  , Var
  , newVar
  , readVar
  , writeVar
  , modifyVar
  , resetInitial

    -- ** @Effect@s
  , Effect

    -- ** @Test@s
  , Test
  , (?=)
  , FastDownward.any
  , requiresAxioms

    -- * Solving Problems
  , solve
  , SolveResult(..)
  , Solution
  , runProblem

    -- ** Extracting Plans
  , totallyOrderedPlan
  , partiallyOrderedPlan
  )
  where

import Control.Applicative ( Alternative(..) )
import qualified Control.Monad.Fail
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader.Class ( local )
import Control.Monad.State.Class ( get, gets, modify )
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader ( ReaderT(..), runReaderT )
import Control.Monad.Trans.State.Strict ( StateT, evalStateT )
import qualified Data.Foldable
import qualified Data.Graph
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.List ( inits, intersect )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import Data.String ( fromString )
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.IO
import Data.Traversable ( for )
import qualified FastDownward.Exec as Exec
import qualified FastDownward.SAS
import qualified FastDownward.SAS.Axiom
import qualified FastDownward.SAS.Effect
import qualified FastDownward.SAS.Operator
import qualified FastDownward.SAS.Plan
import qualified FastDownward.SAS.Variable
import Prelude hiding ( reads )
import System.Exit
import System.IO.Temp


-- | A @Var@ is a state variable - a variable who's contents may change over
-- the execution of a plan. 'Effect's can read and write from variables in
-- order to change their state.
data Var a =
  Var
    { variableIndex :: {-# UNPACK #-} !FastDownward.SAS.VariableIndex
      -- ^ The SAS variable index of this variable.
    , values :: {-# UNPACK #-} !( IORef ( Map a ( Committed, FastDownward.SAS.DomainIndex ) ) )
      -- ^ Map Haskell values to the index in the domain in the SAS
      -- representation.
    , subscribed :: {-# UNPACK #-} !( IORef ( a -> FastDownward.SAS.DomainIndex -> IO () ) )
    , fromDomainIndex :: {-# UNPACK #-} !( IORef ( Map FastDownward.SAS.DomainIndex a ) )
      -- ^ Map back from a domain index to the Haskell value.
    }


data Committed =
  Committed | Uncommitted


-- | The @Problem@ monad is used to build a computation that describes a
-- particular planning problem. In this monad you can declare state variables
-- - 'Var's - using 'newVar', and you can solve planning problems using 'solve'.
newtype Problem a =
  Problem { unProblem :: StateT ProblemState IO a }
  deriving
    ( Functor, Applicative, Monad, MonadIO )


-- | Information needed to translate a 'Var' into its SAS equivalance.
data VariableDeclaration =
  VariableDeclaration
    { initial :: {-# UNPACK #-} !FastDownward.SAS.DomainIndex
      -- ^ The index of the initial (starting) value.
    , _enumerateDomain :: IO [ FastDownward.SAS.DomainIndex ]
      -- ^ List all values this variable can take.
    , _axiomLayer :: {-# UNPACK #-} !Int
      -- ^ The axiom layer of this variable. Most variables live at -1, derived
      -- variables at higher layers.
    }


-- | The state used to translate a 'Problem' its SAS equivalance.
data ProblemState =
  ProblemState
    { initialState :: !( Map FastDownward.SAS.VariableIndex VariableDeclaration )
      -- ^ A table of variables, indexed by their apperance in the SAS variable
      -- list.
    , axioms :: !( Seq FastDownward.SAS.Axiom )
      -- ^ A list of derived axioms.
    }


-- | Observe that a 'Var' can take a particular value.
observeValue :: ( Ord a, MonadIO m ) => Var a -> a -> m FastDownward.SAS.DomainIndex
observeValue var a = liftIO $ do
  vs <-
    readIORef ( values var )

  case Map.lookup a vs of
    Just ( _, i ) ->
      return i

    Nothing -> do
      let
        i =
          FastDownward.SAS.DomainIndex ( fromIntegral ( Map.size vs ) )

      modifyIORef' ( fromDomainIndex var ) ( Map.insert i a )

      i <$ modifyIORef' ( values var ) ( Map.insert a ( Uncommitted, i ) )


commit :: ( Ord a, MonadIO m ) => Var a -> a -> m ()
commit var a = liftIO $ do
  modifyIORef' ( values var ) ( Map.adjust ( \( _, x ) -> ( Committed, x ) ) a )


-- | Introduce a new state variable into a problem, and set it to an initial
-- starting value.
newVar :: Ord a => a -> Problem ( Var a )
newVar =
  newVarAt (-1)


-- | Introduce a new state variable into a problem at a particular axiom layer.
newVarAt :: Ord a => Int -> a -> Problem ( Var a )
newVarAt axiomLayer initialValue = do
  -- Allocate the domain IORef
  values <-
    liftIO ( newIORef mempty )

  -- Lookup an unused index for this variable.
  variableIndex <-
    freshIndex

  subscribed <-
    liftIO ( newIORef ( \_ _ -> return () ) )

  fromDomainIndex <-
    liftIO ( newIORef mempty )

  let
    enumerate =
      map snd . Map.elems <$> liftIO ( readIORef values )

    var =
      Var{..}

  -- Observe the initial value...
  initialI <-
    observeValue var initialValue

  commit var initialValue

  -- ... and record it in the ProblemState.
  Problem
    ( modify
        ( \ps ->
            ps
              { initialState =
                  Map.insert
                    variableIndex
                    ( VariableDeclaration initialI enumerate axiomLayer )
                    ( initialState ps )
              }
        )
    )

  return var


-- | Lookup the next unused variable index.
freshIndex :: Problem FastDownward.SAS.VariableIndex
freshIndex =
  FastDownward.SAS.VariableIndex <$> Problem ( gets ( fromIntegral . Map.size . initialState ) )


-- | Write a value into 'Var'. If the solver choses to use this particular
-- 'Effect', then the @Var@ will begin take this new value.
writeVar :: Ord a => Var a -> a -> Effect ()
writeVar var a = Effect $ do
  -- Writing a variable is fairly simple. First, we check what values the
  -- variable has already taken. If the value we're writing is in that set, then
  -- there's not much to do - we've already considered this assignment.
  --
  -- If the value is new, we record that this write invalidated a Var, which
  -- will cause exhaustEffects to run again.
  edomainIndex <- liftIO $ do
    currentValues <-
      readIORef ( values var )

    case Map.lookup a currentValues of
      Nothing -> do
        -- We've never seen this value before, first observe it to obtain
        -- its domain index.
        Left <$> observeValue var a

      Just ( _, domainIndex ) ->
        return ( Right domainIndex )

  callCC $ \k ->
    case edomainIndex of
      Left domainIndex -> do
        -- We just discovered a new value, so we'll broadcast this.
        laterRef <-
          liftIO $ newIORef $ do
            commit var a

            notify <-
              readIORef ( subscribed var )

            notify a domainIndex

        local
          ( \es ->
              es
                { writes =
                    Map.insert ( variableIndex var ) domainIndex ( writes es )
                , onCommit = do
                    action <-
                      readIORef laterRef

                    writeIORef laterRef ( return () )

                    action

                    onCommit es
                }
          )
          ( k () )

      Right domainIndex ->
        -- This is not a new value, so just record it and continue.
        local
          ( \es ->
              es
                { writes =
                    Map.insert ( variableIndex var ) domainIndex ( writes es )
                }
          )
          ( k () )


-- | Read the value of a 'Var' at the point the 'Effect' is invoked by the
-- solver.
readVar :: Ord a => Var a -> Effect a
readVar var = Effect $ ContT $ \k -> ReaderT $ \es -> do
  -- To "read" a variable is actually to read *all* of its values
  -- non-deterministically. The first time the Var v1 is read, we enumerate it
  -- and run the continuation with all values. However, in this continuation a
  -- subsquent read from v1 should be stable - that is, deterministic. This
  -- is done by consulting the 'reads' map of prior reads first.
  -- Furthermore, if this variable has been written too, we re-read the last
  -- write.
  let
    mPrevRead =
      Map.lookup ( variableIndex var ) ( reads es )

    mPrevWrite =
      Map.lookup ( variableIndex var ) ( writes es )

  case ( mPrevWrite, mPrevRead ) of
    ( Just prevWriteIndex, _ ) -> do
      -- We've written this variable, so continue with what we last wrote.
      prevWrite <-
        ( Map.! prevWriteIndex ) <$> readIORef ( fromDomainIndex var )

      runReaderT ( k prevWrite ) es

    ( Nothing, Just prevReadIndex ) -> do
      -- We've seen this variable before, so just continue with it.
      prevRead <-
        ( Map.! prevReadIndex ) <$> readIORef ( fromDomainIndex var )

      runReaderT ( k prevRead ) es

    ( Nothing, Nothing ) -> do
      -- We have never seen this variable before.
      let
        runRecordingRead a domainIndex =
          runReaderT
            ( k a )
            es
              { reads =
                  Map.insert ( variableIndex var ) domainIndex ( reads es )
              }

      currentValues <-
        readIORef ( values var )

      -- First, subscribe to any new writes. This has to be done first because
      -- yielding known values could immediately cause a write to happen
      -- (e.g., in the case of using modifyVar = readVar v >>= writeVar . f).
      modifyIORef'
        ( subscribed var )
        ( \io x y -> runRecordingRead x y >> io x y )

      -- Now enumerate all known reads.
      Map.foldrWithKey'
        ( \a ( committed, domainIndex ) r -> do
            case committed of
              Committed ->
                runRecordingRead a domainIndex

              _ ->
                return ()

            r
        )
        ( pure () )
        currentValues


-- | Modify the contents of a 'Var' by using a function.
--
-- @modifyVar v f = readVar v >>= writeVar v . f@
modifyVar :: Ord a => Var a -> ( a -> a ) -> Effect ()
modifyVar v f =
  readVar v >>= writeVar v . f


-- | An 'Effect' is a transition in a planning problem - a point where variables
-- can be inspected for their current values, and where they can take on new
-- values. For example, there might be an @Effect@ to instruct the robot to
-- move to a particular target location, if its current location is adjacent.
--
-- The @Effect@ monad supports failure, so you can 'guard' an @Effect@ to only
-- be applicable under particular circumstances. Continuing the above example,
-- we loosely mentioned the constraint that the robot must be adjacent to a
-- target location - something that could be modelled by using 'readVar' to
-- read the current location, and 'guard' to guard that this location is
-- adjacent to our goal.
newtype Effect a =
  Effect { runEffect :: ContT () ( ReaderT EffectState IO ) a }
  deriving
    ( Functor, Applicative )


instance Monad Effect where
  return a =
    Effect ( return a )
  {-# INLINE return #-}

  Effect a >>= f = Effect $
    a >>= runEffect . f
  {-# INLINE (>>=) #-}

  fail =
    Control.Monad.Fail.fail
  {-# INLINE fail #-}


instance Control.Monad.Fail.MonadFail Effect where
  fail _ =
    empty
  {-# INLINE fail #-}


instance Alternative Effect where
  empty =
    Effect ( ContT ( \_k -> return () ) )
  {-# INLINE empty #-}

  Effect a <|> Effect b =
    Effect $ ContT $ \k ->
      runContT a k <|> runContT b k
  {-# INLINE (<|>) #-}


-- | Used to track the evaluation of an 'Effect' as we enumerate all possible
-- outcomes.
data EffectState =
  EffectState
    { reads :: !( Map FastDownward.SAS.VariableIndex FastDownward.SAS.DomainIndex )
      -- ^ The variables and their exact values read to reach a certain outcome.
    , writes :: !( Map FastDownward.SAS.VariableIndex FastDownward.SAS.DomainIndex )
      -- ^ The changes made by this instance.
    , onCommit :: !( IO () )
    }


-- | The result from the solver on a call to 'solve'.
data SolveResult a
  = Unsolvable
    -- ^ The problem was to be unsolvable.
  | Crashed String String ExitCode
    -- ^ Fast Downward crashed (or otherwise rejected) the given problem.
  | Solved ( Solution a )
    -- ^ A solution was found.


-- | A successful solution to a planning problem. You can unpack a @Solution@
-- into a plan by using 'totallyOrderedPlan' and 'partiallyOrderedPlan'.
data Solution a =
  Solution
    { sas :: FastDownward.SAS.Plan
    , operators :: IntMap.IntMap a
    , stepIndices :: [ IntMap.Key ]
    }


-- | Extract a totally ordered plan from a solution.
totallyOrderedPlan :: Solution a -> [ a ]
totallyOrderedPlan Solution{..} =
  map ( operators IntMap.! ) stepIndices


-- | Given a particular 'Exec.SearchEngine', attempt to solve a planning
-- problem.
solve
  :: Show a
  => Exec.SearchEngine
  -> [ Effect a ]
     -- ^ The set of effects available to the planner. Each effect can return
     -- some domain-specific information of type @a@ which you can use to
     -- interpret the plan. This will usually be some kind of @Action@ type.
  -> [ Test ]
     -- ^ A conjunction of tests that must true for a solution to be considered
     -- acceptable.
  -> Problem ( SolveResult a )
     -- ^ The list of steps that will converge the initial state to a state that
     -- satisfies the given goal predicates.
solve cfg ops tests = do
  -- It's convenient to work in the 'Problem' monad, but we don't want to dirty
  -- the state. (E.g., maybe the user will want to call @solve@ again with
  -- something that doesn't require axioms derived to satisfy the first calls
  -- 'Test').
  s0 <-
    Problem get

  Problem $ liftIO $ flip evalStateT s0 $ do
    -- First, convert the given goal into a list of variable assignments. This
    -- will also introduce axioms for conjunctions, and will observe all test
    -- variable values.
    goal <-
      unProblem ( Prelude.traverse testToVariableAssignment tests )

    -- Now that we've observed every value we know up-front, find the fixed point
    -- of the set of operators.
    operators <-
      liftIO ( exhaustEffects ops ) <* liftIO ( putStrLn "Effects exhausted" )

    initialState <-
      gets initialState

    axioms <-
      gets axioms

    -- For all variables, convert them into a SAS-compatible list.
    variables <-
      for
        ( Map.toAscList initialState )
        ( \( FastDownward.SAS.VariableIndex i
           , VariableDeclaration _ enumerate axiomLayer
           ) -> do
            domain <-
              liftIO enumerate

            return
              FastDownward.SAS.Variable
                { name =
                    fromString ( "var-" <> show i )
                , domain =
                    Seq.fromList $
                    map
                      ( \( FastDownward.SAS.DomainIndex d ) ->
                          fromString
                            ( "Atom var-" <> show i <> "(" <> show d <> ")" )
                      )
                      domain
                      ++ [ "Atom dummy(dummy)" ]
                , axiomLayer = axiomLayer
                }
        )

    let
      plan =
        FastDownward.SAS.Plan
          { version =
              FastDownward.SAS.SAS3
          , useCosts =
              FastDownward.SAS.NoCosts
          , variables =
              Seq.fromList variables
          , mutexGroups =
              mempty
          , initialState =
              FastDownward.SAS.State
                ( Seq.fromList $ map ( initial . snd ) ( Map.toAscList initialState ) )
          , goal =
              FastDownward.SAS.Goal ( Seq.fromList goal )
          , operators =
              Seq.fromList $ zipWith
                ( \i ( _, EffectState{ reads, writes } ) ->
                    let
                      unchangedWrites =
                        Map.mapMaybe
                          ( \( a, b ) -> if a == b then Just a else Nothing )
                          ( Map.intersectionWith (,) writes reads )

                      actualWrites =
                        writes `Map.difference` unchangedWrites

                    in
                    FastDownward.SAS.Operator
                      { name = fromString ( "op" <> show i )
                      , prevail =
                          Seq.fromList $ map
                            ( uncurry FastDownward.SAS.VariableAssignment )
                            ( Map.toList
                                ( Map.difference reads writes
                                    <> unchangedWrites
                                )
                            )
                      , effects =
                          Seq.fromList $ map
                            ( \( v, post ) -> FastDownward.SAS.Effect v Nothing post )
                            ( Map.toList ( Map.difference writes reads ) )
                            ++
                              Map.elems
                                ( Map.intersectionWithKey
                                    ( \v pre post ->
                                        FastDownward.SAS.Effect v ( Just pre ) post
                                    )
                                    reads
                                    actualWrites
                                )
                      }
                )
                [ 0 :: Int .. ]
                operators
          , axioms =
              Seq.fromList $ Data.Foldable.toList axioms
          }

    planFilePath <-
      liftIO ( emptySystemTempFile "sas_plan" )

    -- liftIO $ Data.Text.Lazy.IO.writeFile "plan.sas" ( FastDownward.SAS.Plan.toSAS plan )

    ( exitCode, stdout, stderr ) <-
      liftIO
        ( Exec.callFastDownward
            Exec.Options
              { fastDownward = "downward"
              , problem = plan
              , planFilePath = planFilePath
              , search = cfg
              }
        )

    liftIO $ putStrLn stdout
    liftIO $ putStrLn stderr

    case exitCode of
      ExitFailure 11 ->
        return Unsolvable

      ExitFailure 12 ->
        return Unsolvable

      ExitFailure other ->
        return ( Crashed stdout stderr ( ExitFailure other ) )

      ExitSuccess -> liftIO $ do
        planText <-
          Data.Text.Lazy.IO.readFile planFilePath

        let
          stepIndices =
            map                           -- Read "(op42)" as 42
              ( read
                  . Data.Text.Lazy.unpack
                  . Data.Text.Lazy.init   -- keep everything up to ")"
                  . Data.Text.Lazy.drop 3 -- drop "(op"
              )
              ( takeWhile
                  ( "(" `Data.Text.Lazy.isPrefixOf` )
                  ( Data.Text.Lazy.lines planText )
              )

        return
          ( Solved
              Solution
                { sas = plan
                , operators = IntMap.fromList ( zip [0..] ( map fst operators ) )
                , ..
                }
          )


exhaustEffects
  :: Traversable t
  => t ( Effect a )
  -> IO [ ( a, EffectState ) ]
exhaustEffects ops = do
  out <-
    -- Every 'Effect' branch will eventually write it's output here.
    newIORef []

  Data.Foldable.for_
    ops
    ( \( Effect ( ContT k ) ) ->
        runReaderT
          ( k
              ( \a ->
                  ReaderT $ \es -> do
                    onCommit es

                    modifyIORef out ( ( a, es ) : )
              )
          )
          ( EffectState mempty mempty ( return () ) )
    )

  readIORef out


-- | Leave the 'Problem' monad by running the given computation to 'IO'.
runProblem :: MonadIO m => Problem a -> m a
runProblem p = liftIO $
  evalStateT
    ( unProblem p )
    ProblemState { initialState = mempty , axioms = mempty }


-- | Test that a 'Var' is set to a particular value.
(?=) :: Ord a => Var a -> a -> Test
(?=) =
  TestEq


-- | @Test@s are use to drive the solver in order to find a plan to the goal.
data Test where
  TestEq :: Ord a => {-# UNPACK #-} !( Var a ) -> !a -> Test
  Any :: ![ Test ] -> Test


requiresAxioms :: Test -> Bool
requiresAxioms =
  \case
    TestEq{} ->
      False

    Any{} ->
      True


-- | Reset the initial state of a variable (the value that the solver will begin
-- with).
resetInitial :: Ord a => Var a -> a -> Problem ()
resetInitial var a = do
  liftIO ( writeIORef ( values var ) mempty )

  liftIO ( writeIORef ( fromDomainIndex var ) mempty )

  i <-
    observeValue var a

  commit var a

  Problem $ modify $ \ps ->
    ps
      { initialState =
          Map.adjust
            ( \decl -> decl { initial = i } )
            ( variableIndex var )
            ( initialState ps )
      }


-- | Take the disjunction (or) of a list of 'Test's to a form new a @Test@ that
-- succeeds when at least one of the given tests is true.
--
-- __Caution!__ The use of @any@ introduces /axioms/ into the problem definition,
-- which is not compatible with many search engines.
any :: [ Test ] -> Test
any =
  Any


testToVariableAssignment :: Test -> Problem FastDownward.SAS.VariableAssignment
testToVariableAssignment ( TestEq var a ) =
  FastDownward.SAS.VariableAssignment ( variableIndex var )
    <$> observeValue var a

testToVariableAssignment ( Any tests ) = do
  axiom <-
    newVarAt 0 False

  falseI <-
    observeValue axiom False

  trueI <-
    observeValue axiom True

  assigns <-
    Prelude.traverse testToVariableAssignment tests

  Problem $ modify $ \ps ->
    ps
      { axioms =
          Seq.fromList
            [ FastDownward.SAS.Axiom
                { variable = variableIndex axiom
                , conditions = Seq.singleton va
                , pre = falseI
                , post = trueI
                }
            | va <- assigns
            ]
            <> axioms ps

      }

  return ( FastDownward.SAS.VariableAssignment ( variableIndex axiom ) trueI )


-- | Deorder a plan into a partially ordered plan. This attempts to recover some
-- concurrency when adjacent plan steps do not need to be totally ordered. The
-- result of this function is the same as the result of
-- 'Data.Graph.graphFromEdges'.
partiallyOrderedPlan
  :: Ord a
  => Solution a
  -> ( Data.Graph.Graph
     , Data.Graph.Vertex -> ( a, IntMap.Key, [ IntMap.Key ] )
     , IntMap.Key -> Maybe Data.Graph.Vertex
     )
partiallyOrderedPlan Solution{..} =
  let
    ops =
      IntMap.fromList ( zip [0..] ( Data.Foldable.toList ( FastDownward.SAS.Plan.operators sas ) ) )

    operation i =
      ops IntMap.! i

    g = do
      -- Here we consider every step in the list of steps. Using a combination
      -- of inits and reverse, given x1, x2, ..., xn, we end up with the
      -- following iterations:
      --
      -- 1. ( x1, [] )
      -- 2. ( x2, [ x1 ] )
      -- 3. ( xn, [ x1, x2, .., x(n-1) ] )
      --
      ( i, o ) : priorReversed <-
        map
          reverse
          ( tail ( inits ( map ( \i -> ( i, operation i ) ) stepIndices ) ) )

      let
        priors =
          reverse priorReversed

      -- For each step return it, and a list of any supporting operators.
      return
        ( operators IntMap.! i
        , i
        , mapMaybe
            ( \( j, x ) -> if o `after` x then Just j else Nothing )
            priors
        )

    ( gr, fromVertex, toVertex ) =
      Data.Graph.graphFromEdges g

  in
  ( Data.Graph.transposeG gr, fromVertex, toVertex )


-- | Given an 'Problem.Operator', return its effects as a list of variable
-- assignments.
assignments :: FastDownward.SAS.Operator -> [ FastDownward.SAS.VariableAssignment ]
assignments o =
  [ FastDownward.SAS.VariableAssignment
      ( FastDownward.SAS.Effect.variable e )
      ( FastDownward.SAS.Effect.post e )
  | e <- Data.Foldable.toList ( FastDownward.SAS.Operator.effects o )
  ]



-- | Given an 'Problem.Operator', return a list of variable assignments that
-- must be such in order for this operator to be applicable. This is the
-- combination of the prevailing conditions for the operator, and the original
-- state for all variables updated by effects.
requirements :: FastDownward.SAS.Operator -> Seq FastDownward.SAS.VariableAssignment
requirements o =
  FastDownward.SAS.Operator.prevail o <> Seq.fromList ( original o )


-- | Return all the original 'FastDownward.VariableAssignment's for an
-- 'Problem.Operator'. This is the set of original assignments before the
-- operator is applied.
original :: FastDownward.SAS.Operator -> [ FastDownward.SAS.VariableAssignment ]
original o =
  mapMaybe
    ( \e ->
        FastDownward.SAS.VariableAssignment ( FastDownward.SAS.Effect.variable e )
          <$> FastDownward.SAS.Effect.pre e
    )
    ( Data.Foldable.toList ( FastDownward.SAS.Operator.effects o ) )


-- | @o `after` x@ is true if:
--
-- 1. o requires an initial variable state produced by x.
-- 2. x requires an initial variable state that it is also required by
--    o. This is because x will be changing the state of a variable o
--    required by o.
after
  :: FastDownward.SAS.Operator.Operator
  -> FastDownward.SAS.Operator.Operator
  -> Bool
o `after` x =
  not ( null ( Data.Foldable.toList ( requirements o ) `intersect` assignments x ) ) ||
    not ( null ( Data.Foldable.toList ( requirements x ) `intersect` original o ) )
