{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module FastDownward.Exec
  ( Options(..)
  , callFastDownward

    -- * Search Engines
  , SearchEngine(..)
  , AStarConfiguration(..)
  , EagerBestFirstConfiguration(..)
  , EagerGreedyConfiguration(..)
  , EnforcedHillClimbingConfiguration(..)
  , IteratedConfiguration(..)
  , LazyBestFirstConfiguration(..)
  , LazyGreedyConfiguration(..)
  , LazyWeightedAStarConfiguration(..)

    -- ** Search Engine Options
  , CostType(..)
  , PreferredOperatorUsage(..)

    -- * Abstract Tasks
  , AbstractTask(..)

    -- * Constraint Generators
  , ConstraintGenerator(..)

    -- * Evaluators
  , Evaluator(..)
  , AddConfiguration(..)
  , AllStatesPotentialConfiguration(..)
  , BlindConfiguration(..)
  , CEAConfiguration(..)
  , CEGARConfiguration(..)
  , CEGARPick(..)
  , CGConfiguration(..)
  , DiversePotentialsConfiguration(..)
  , FFConfiguration(..)
  , GoalCountConfiguration(..)
  , HMConfiguration(..)
  , HMaxConfiguration(..)
  , InitialStatePotentialConfiguration(..)
  , LMCountConfiguration(..)
  , LMCutConfiguration(..)
  , MergeAndShrinkConfiguration(..)
  , Verbosity(..)
  , OperatorCountingConfiguration(..)
  , SampleBasedPotentialsConfiguration(..)
  , CanonicalPDBConfiguration(..)
  , IPDBConfiguration(..)
  , ZeroOnePDBConfiguration(..)

    -- ** Evaluator Configuration
  , LPSolver(..)

    -- * Label Reduction
  , LabelReduction(..)
  , ExactGeneralizedLabelReductionConfiguration(..)
  , LabelReductionMethod(..)
  , SystemOrder(..)

    -- * Landmark Factory
  , LandmarkFactory(..)
  , LMExhaustConfiguration(..)
  , LMHMConfiguration(..)
  , LMMergedConfiguration(..)
  , LMRHWConfiguration(..)
  , LMZGConfiguration(..)

    -- * Merge Scoring Function
  , MergeScoringFunction(..)
  , MIASMConfiguration(..)
  , TotalOrderConfiguration(..)
  , AtomicTsOrder(..)
  , ProductTsOrder(..)

    -- * Merge Selector
  , MergeSelector(..)

    -- * Merge Strategy
  , MergeStrategy(..)
  , MergeSCCsConfiguration(..)
  , OrderOfSCCs(..)

    -- * Merge Tree
  , MergeTree(..)
  , LinearMergeTreeConfiguration(..)
  , UpdateOption(..)
  , VariableOrder(..)

    -- * Open List
  , OpenList(..)
  , AltConfiguration(..)
  , EpsilonGreedyConfiguration(..)
  , ParetoConfiguration(..)
  , SingleConfiguration(..)
  , TiebreakingConfiguration(..)
  , TypeBasedConfiguration(..)

    -- * Pattern Collection Generators
  , PatternCollectionGenerator(..)
  , GeneticConfiguration(..)
  , HillclimbingConfiguration(..)
  , SystematicConfiguration(..)

    -- * Pruning Method
  , PruningMethod(..)
  , StubbornSetsConfiguration(..)

    -- * Shrink Strategy
  , ShrinkStrategy(..)
  , BisimulationConfiguration(..)
  , BisimulationLimitStrategy(..)
  , FPreservingConfiguration(..)
  , HighLow(..)

    -- * Subtask Generators
  , SubtaskGenerator(..)
  , GoalsConfiguration(..)
  , GoalsOrder(..)
  , LandmarksConfiguration(..)
  , OriginalConfiguration(..)
  )
  where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.Text.Lazy.IO
import qualified FastDownward.SAS
import qualified FastDownward.SAS.Plan
import System.Exit ( ExitCode )
import System.IO ( hClose )
import System.Process


data Expr
  = App String [ Expr ] [ ( String, Expr ) ]
  | Lit String
  | List [ Expr ]


infinity :: Expr
infinity =
  Lit "infinity"


intToExpr :: Int -> Expr
intToExpr =
  Lit . show


doubleToExpr :: Double -> Expr
doubleToExpr =
  Lit . show


boolToExpr :: Bool -> Expr
boolToExpr =
  Lit . map toLower . show


none :: Expr
none =
  Lit ""


exprToString :: Expr -> String
exprToString ( App f pos named ) =
  f
    <> "("
    <>
      intercalate
        ","
        ( map exprToString pos
            <> map ( \( k, e ) -> k <> "=" <> exprToString e ) named
        )
    <> ")"
exprToString ( Lit s ) =
  s
exprToString ( List l ) =
  "[" <> intercalate "," ( map exprToString l ) <> "]"


data Options =
  Options
    { fastDownward :: FilePath
    , problem :: FastDownward.SAS.Plan
    , planFilePath :: FilePath
    , search :: SearchEngine
    }


callFastDownward :: MonadIO m => Options -> m ( ExitCode, String, String )
callFastDownward Options{ fastDownward, problem, planFilePath, search } = liftIO $ do
  process@( Just writeProblemHandle, _, _, processHandle ) <-
    createProcess
      ( proc
          fastDownward
          [ "--internal-plan-file", planFilePath
          , "--search", "ehc(cg())"
          -- , exprToString ( searchEngineToExpr search )
          ]
      )
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

  {-# SCC writeProblem #-} Data.Text.Lazy.IO.hPutStr writeProblemHandle ( FastDownward.SAS.Plan.toSAS problem )

  hClose writeProblemHandle

  exitCode <-
    waitForProcess processHandle

  return ( exitCode, "", "" )


-- | See <http://www.fast-downward.org/Doc/SearchEngine>
data SearchEngine
  = AStar AStarConfiguration
  | EagerBestFirst EagerBestFirstConfiguration
  | EagerGreedy EagerGreedyConfiguration
  | EnforcedHillClimbing EnforcedHillClimbingConfiguration
  | Iterated IteratedConfiguration
  | LazyBestFirst LazyBestFirstConfiguration
  | LazyGreedy LazyGreedyConfiguration
  | LazyWeightedAStar LazyWeightedAStarConfiguration


searchEngineToExpr :: SearchEngine -> Expr
searchEngineToExpr =
  \case
    AStar cfg ->
      aStar cfg

    EagerBestFirst cfg ->
      eager cfg

    EagerGreedy cfg ->
      eagerGreedy cfg

    EnforcedHillClimbing cfg ->
      ehc cfg

    Iterated cfg ->
      iterated cfg

    LazyBestFirst cfg ->
      lazy cfg

    LazyGreedy cfg ->
      lazyGreedy cfg

    LazyWeightedAStar cfg ->
      lazyWAStar cfg


-- | See <http://www.fast-downward.org/Doc/SearchEngine#A.2A_search_.28eager.29>
data AStarConfiguration =
  AStarConfiguration
    { evaluator :: Evaluator
    , lazyEvaluator :: Maybe Evaluator
    , pruning :: PruningMethod
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


aStar :: AStarConfiguration -> Expr
aStar AStarConfiguration{ evaluator, lazyEvaluator, pruning, costType, bound, maxTime } =
  App
    "astar"
    [ evaluatorToExpr evaluator ]
    [ ( "lazy_evaluator", maybe none evaluatorToExpr lazyEvaluator )
    , ( "pruning", pruningMethodToExpr pruning )
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/SearchEngine#Eager_best-first_search>
data EagerBestFirstConfiguration =
  EagerBestFirstConfiguration
    { open :: OpenList
    , reopenClosed :: Bool
    , fEval :: Maybe Evaluator
    , preferred :: [ Evaluator ]
    , pruning :: PruningMethod
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


eager :: EagerBestFirstConfiguration -> Expr
eager EagerBestFirstConfiguration{ open, reopenClosed, fEval, preferred, pruning, costType, bound, maxTime } =
  App
    "eager"
    [ openListToExpr open ]
    [ ( "reopen_closed", boolToExpr reopenClosed )
    , ( "f_eval", maybe none evaluatorToExpr fEval )
    , ( "preferred", List ( map evaluatorToExpr preferred ) )
    , ( "pruning", pruningMethodToExpr pruning )
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/SearchEngine#Greedy_search_.28eager.29>
data EagerGreedyConfiguration =
  EagerGreedyConfiguration
    { evaluators :: [ Evaluator ]
    , preferred :: [ Evaluator ]
    , boost :: Int
    , pruning :: PruningMethod
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


eagerGreedy :: EagerGreedyConfiguration -> Expr
eagerGreedy EagerGreedyConfiguration{ evaluators, preferred, boost, pruning, costType, bound, maxTime } =
  App
    "eager_greedy"
    [ List ( map evaluatorToExpr evaluators ) ]
    [ ( "preferred", List ( map evaluatorToExpr preferred ) )
    , ( "boost", intToExpr boost )
    , ( "pruning", pruningMethodToExpr pruning )
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/SearchEngine#Lazy_enforced_hill-climbing>
data EnforcedHillClimbingConfiguration =
  EnforcedHillClimbingConfiguration
    { h :: Evaluator
    , preferredUsage :: PreferredOperatorUsage
    , preferred :: [ Evaluator ]
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


ehc :: EnforcedHillClimbingConfiguration -> Expr
ehc EnforcedHillClimbingConfiguration{ h, preferredUsage, preferred, costType, bound, maxTime } =
  App
    "ehc"
    [ evaluatorToExpr h ]
    [ ( "preferred_usage", preferredUsageToExpr preferredUsage )
    , ( "preferred", List ( map evaluatorToExpr preferred ) )
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/SearchEngine#Iterated_search>
data IteratedConfiguration =
  IteratedConfiguration
    { engines :: [ SearchEngine ]
    , passBound :: Bool
    , repeatLast :: Bool
    , continueOnFail :: Bool
    , continueOnSolve :: Bool
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


iterated :: IteratedConfiguration -> Expr
iterated IteratedConfiguration{ engines, passBound, repeatLast, continueOnFail, continueOnSolve, costType, bound, maxTime } =
  App
    "iterated"
    [ List ( map searchEngineToExpr engines ) ]
    [ ( "pass_bound", boolToExpr passBound )
    , ( "repeat_last", boolToExpr repeatLast )
    , ( "continue_on_fail", boolToExpr continueOnFail )
    , ( "continue_on_solve", boolToExpr continueOnSolve )
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/SearchEngine#Lazy_best-first_search>
data LazyBestFirstConfiguration =
  LazyBestFirstConfiguration
    { open :: OpenList
    , reopenClosed :: Bool
    , preferred :: [ Evaluator ]
    , randomizeSuccessors :: Bool
    , preferredSuccessorsFirst :: Bool
    , randomSeed :: Maybe Int
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


lazy :: LazyBestFirstConfiguration -> Expr
lazy LazyBestFirstConfiguration{ open, reopenClosed, preferred, randomizeSuccessors, preferredSuccessorsFirst, randomSeed, costType, bound, maxTime } =
  App
    "lazy"
    [ openListToExpr open ]
    [ ( "reopen_closed", boolToExpr reopenClosed )
    , ( "preferred", List ( map evaluatorToExpr preferred ) )
    , ( "randomize_successors", boolToExpr randomizeSuccessors )
    , ( "preferred_successors_first", boolToExpr preferredSuccessorsFirst )
    , randomSeedExpr randomSeed
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/SearchEngine#Greedy_search_.28lazy.29>
data LazyGreedyConfiguration =
  LazyGreedyConfiguration
    { evaluators :: [ Evaluator ]
    , preferred :: [ Evaluator ]
    , reopenClosed :: Bool
    , boost :: Int
    , randomizeSuccessors :: Bool
    , preferredSuccessorsFirst :: Bool
    , randomSeed :: Maybe Int
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


lazyGreedy :: LazyGreedyConfiguration -> Expr
lazyGreedy LazyGreedyConfiguration{ evaluators, preferred, reopenClosed, boost, randomizeSuccessors, preferredSuccessorsFirst, randomSeed, costType, bound, maxTime } =
  App
    "lazy_greedy"
    [ List ( map evaluatorToExpr evaluators ) ]
    [ ( "reopen_closed", boolToExpr reopenClosed )
    , ( "preferred", List ( map evaluatorToExpr preferred ) )
    , ( "boost", intToExpr boost )
    , ( "randomize_successors", boolToExpr randomizeSuccessors )
    , ( "preferred_successors_first", boolToExpr preferredSuccessorsFirst )
    , randomSeedExpr randomSeed
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/SearchEngine#A.28Weighted.29_A.2A_search_.28lazy.29>
data LazyWeightedAStarConfiguration =
  LazyWeightedAStarConfiguration
    { evaluators :: [ Evaluator ]
    , preferred :: [ Evaluator ]
    , reopenClosed :: Bool
    , boost :: Int
    , w :: Int
    , randomizeSuccessors :: Bool
    , preferredSuccessorsFirst :: Bool
    , randomSeed :: Maybe Int
    , costType :: CostType
    , bound :: Maybe Int
    , maxTime :: Maybe Double
    }


lazyWAStar :: LazyWeightedAStarConfiguration -> Expr
lazyWAStar LazyWeightedAStarConfiguration{ evaluators, preferred, reopenClosed, boost, w, randomizeSuccessors, preferredSuccessorsFirst, randomSeed, costType, bound, maxTime } =
  App
    "lazy_wastar"
    [ List ( map evaluatorToExpr evaluators ) ]
    [ ( "reopen_closed", boolToExpr reopenClosed )
    , ( "preferred", List ( map evaluatorToExpr preferred ) )
    , ( "boost", intToExpr boost )
    , ( "w", intToExpr w )
    , ( "randomize_successors", boolToExpr randomizeSuccessors )
    , ( "preferred_successors_first", boolToExpr preferredSuccessorsFirst )
    , randomSeedExpr randomSeed
    , ( "cost_type", costTypeToExpr costType )
    , ( "bound", maybe infinity intToExpr bound )
    , maxTimeExpr maxTime
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator>
data Evaluator
  = Add AddConfiguration
  | AllStatesPotential AllStatesPotentialConfiguration
  | Blind BlindConfiguration
  | CEA CEAConfiguration
  | CEGAR CEGARConfiguration
  | CG CGConfiguration
  | DiversePotentials DiversePotentialsConfiguration
  | FF FFConfiguration
  | GoalCount GoalCountConfiguration
  | HM HMConfiguration
  | HMax HMaxConfiguration
  | InitialStatePotential InitialStatePotentialConfiguration
  | LMCount LMCountConfiguration
  | LMCut LMCutConfiguration
  | Max [ Evaluator ]
  | MergeAndShrink MergeAndShrinkConfiguration
  | OperatorCounting OperatorCountingConfiguration
  | SampleBasedPotentials SampleBasedPotentialsConfiguration
  | ConstantEvaluator Int
  | G
  | Pref
  | Sum [ Evaluator ]
  | Weighted Evaluator Int
  | CanonicalPDB CanonicalPDBConfiguration
  | IPDB IPDBConfiguration
  | ZeroOnePDB ZeroOnePDBConfiguration


evaluatorToExpr :: Evaluator -> Expr
evaluatorToExpr =
  \case
    Add cfg ->
      add cfg

    AllStatesPotential cfg ->
      allStatesPotential cfg

    Blind cfg ->
      blind cfg

    CEA cfg ->
      cea cfg

    CEGAR cfg ->
      cegar cfg

    CG cfg ->
      cg cfg

    DiversePotentials cfg ->
      diversePotentials cfg

    FF cfg ->
      ff cfg

    GoalCount cfg ->
      goalCount cfg

    HM cfg ->
      hm cfg

    HMax cfg ->
      hmax cfg

    InitialStatePotential cfg ->
      initialStatePotential cfg

    LMCount cfg ->
      lmcount cfg

    LMCut cfg ->
      lmcut cfg

    Max evals ->
      App "max" [ List ( map evaluatorToExpr evals ) ] []

    MergeAndShrink cfg ->
      mergeAndShrink cfg

    OperatorCounting cfg ->
      operatorCounting cfg

    ConstantEvaluator cfg ->
      App "const" [] [ ( "vale", intToExpr cfg ) ]

    G ->
      App "g" [] []

    Pref ->
      App "pref" [] []

    Sum evals ->
      App "sum" [ List ( map evaluatorToExpr evals ) ] []

    Weighted eval weight ->
      App "weight" [ evaluatorToExpr eval, intToExpr weight ] []

    CanonicalPDB cfg ->
      cpdbs cfg

    SampleBasedPotentials cfg ->
      sampleBasedPotentials cfg

    IPDB cfg ->
      ipdb cfg

    ZeroOnePDB cfg ->
      zopdbs cfg


-- | See <http://www.fast-downward.org/Doc/PruningMethod>
data PruningMethod
  = Null
  | StubbornSetsEC StubbornSetsConfiguration
  | StubbornSetsSimple StubbornSetsConfiguration


pruningMethodToExpr :: PruningMethod -> Expr
pruningMethodToExpr =
  \case
    Null ->
      App "null" [] []

    StubbornSetsEC cfg ->
      stubbornSetsEc cfg

    StubbornSetsSimple cfg ->
      stubbornSetsSimple cfg


data CostType
  = Normal
    -- ^ All actions are accounted for with their real cost.
  | One
    -- ^ All actions are accounted for as unit cost.
  | PlusOne
    -- ^ All actions are accounted for as their real cost + 1 (except if all
    -- actions have original cost 1, in which case cost 1 is used). This is the
    -- behaviour known for the heuristics of the LAMA planner. This is intended
    -- to be used by the heuristics, not search engines, but is supported for
    -- both.
  deriving
    ( Show )


costTypeToExpr :: CostType -> Expr
costTypeToExpr =
  Lit . map toUpper . show


-- | See <http://www.fast-downward.org/Doc/OpenList>
data OpenList
  = Alt AltConfiguration
  | EpsilonGreedy EpsilonGreedyConfiguration
  | Pareto ParetoConfiguration
  | Single SingleConfiguration
  | Tiebreaking TiebreakingConfiguration
  | TypeBased TypeBasedConfiguration


openListToExpr :: OpenList -> Expr
openListToExpr =
  \case
    Alt cfg ->
      alt cfg

    EpsilonGreedy cfg ->
      epsilonGreedy cfg

    Pareto cfg ->
      pareto cfg

    Single cfg ->
      single cfg

    Tiebreaking cfg ->
      tiebreaking cfg

    TypeBased cfg ->
      typeBased cfg


data PreferredOperatorUsage =
  PruneByPreferred | RankPreferredFirst


preferredUsageToExpr :: PreferredOperatorUsage -> Expr
preferredUsageToExpr =
  \case
    PruneByPreferred ->
      Lit "PRUNE_BY_PREFERRED"

    RankPreferredFirst ->
      Lit "RANK_PREFERRED_FIRST"


-- | See <http://www.fast-downward.org/Doc/Evaluator#Additive_heuristic>
data AddConfiguration =
  AddConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


add :: AddConfiguration -> Expr
add AddConfiguration{ transform, cacheEstimates } =
  App
    "add"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Potential_heuristic_optimized_for_all_states>
data AllStatesPotentialConfiguration =
  AllStatesPotentialConfiguration
    { maxPotential :: Maybe Double
    , lpSolver :: LPSolver
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


allStatesPotential :: AllStatesPotentialConfiguration -> Expr
allStatesPotential AllStatesPotentialConfiguration{ maxPotential, lpSolver, transform, cacheEstimates } =
  App
    "all_states_potential"
    []
    [ maxPotentialOption maxPotential
    , lpSolverOption lpSolver
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Blind_heuristic>
data BlindConfiguration =
  BlindConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


blind :: BlindConfiguration -> Expr
blind BlindConfiguration{ transform, cacheEstimates } =
  App
    "blind"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Context-enhanced_additive_heuristic>
data CEAConfiguration =
  CEAConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


cea :: CEAConfiguration -> Expr
cea CEAConfiguration{ transform, cacheEstimates } =
  App
    "cea"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Additive_CEGAR_heuristic>
data CEGARConfiguration =
  CEGARConfiguration
    { subtasks :: [ SubtaskGenerator ]
    , maxStates :: Maybe Int
    , maxTransitions :: Maybe Int
    , maxTime :: Maybe Double
    , pick :: CEGARPick
    , useGeneralCosts :: Bool
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    , randomSeed :: Maybe Int
    }


cegar :: CEGARConfiguration -> Expr
cegar CEGARConfiguration{ subtasks, maxStates, maxTransitions, maxTime, pick, useGeneralCosts, transform, cacheEstimates, randomSeed } =
  App
    "cegar"
    []
    [ ( "subtasks", List ( map subtaskToExpr subtasks ) )
    , maxStatesOption maxStates
    , ( "max_transitions", maybe infinity intToExpr maxTransitions )
    , maxTimeExpr maxTime
    , ( "pick", cegarPickToExpr pick )
    , ( "use_general_costs", boolToExpr useGeneralCosts )
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Causal_graph_heuristic>
data CGConfiguration =
  CGConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


cg :: CGConfiguration -> Expr
cg CGConfiguration{ transform, cacheEstimates } =
  App
    "cg"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Diverse_potential_heuristics>
data DiversePotentialsConfiguration =
  DiversePotentialsConfiguration
    { numSamples :: Maybe Int
    , maxNumHeuristics :: Maybe Int
    , maxPotential :: Maybe Double
    , lpSolver :: LPSolver
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    , randomSeed :: Maybe Int
    }


diversePotentials :: DiversePotentialsConfiguration -> Expr
diversePotentials DiversePotentialsConfiguration{ numSamples, maxNumHeuristics, maxPotential, lpSolver, transform, cacheEstimates, randomSeed } =
  App
    "diverse_potentials"
    []
    [ ( "num_samples", maybe infinity intToExpr numSamples )
    , ( "max_num_heuristics", maybe infinity intToExpr maxNumHeuristics )
    , ( "max_potential", maybe infinity doubleToExpr maxPotential )
    , lpSolverOption lpSolver
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#FF_heuristic>
data FFConfiguration =
  FFConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


ff :: FFConfiguration -> Expr
ff FFConfiguration{ transform, cacheEstimates } =
  App
    "ff"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Goal_count_heuristic>
data GoalCountConfiguration =
  GoalCountConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


goalCount :: GoalCountConfiguration -> Expr
goalCount GoalCountConfiguration{ transform, cacheEstimates } =
  App
    "goalcount"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#h.5Em_heuristic>
data HMConfiguration =
  HMConfiguration
    { m :: Int
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


hm :: HMConfiguration -> Expr
hm HMConfiguration{ m, transform, cacheEstimates } =
  App
    "hm"
    []
    [ ( "m", intToExpr m )
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Max_heuristic>
data HMaxConfiguration =
  HMaxConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


hmax :: HMaxConfiguration -> Expr
hmax HMaxConfiguration{ transform, cacheEstimates } =
  App
    "hmax"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Potential_heuristic_optimized_for_initial_state>
data InitialStatePotentialConfiguration =
  InitialStatePotentialConfiguration
    { maxPotential :: Maybe Double
    , lpSolver :: LPSolver
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


initialStatePotential :: InitialStatePotentialConfiguration -> Expr
initialStatePotential InitialStatePotentialConfiguration{ maxPotential, lpSolver, transform, cacheEstimates } =
  App
    "initial_state_potential"
    []
    [ maxPotentialOption maxPotential
    , lpSolverOption lpSolver
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Landmark-count_heuristic>
data LMCountConfiguration =
  LMCountConfiguration
    { lmFactory :: LandmarkFactory
    , admissible :: Bool
    , optimal :: Bool
    , pref :: Bool
    , alm :: Bool
    , lpSolver :: LPSolver
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


lmcount :: LMCountConfiguration -> Expr
lmcount LMCountConfiguration{ lmFactory, admissible, optimal, pref, alm, lpSolver, transform, cacheEstimates } =
  App
    "lmcount"
    [ landmarkFactoryToExpr lmFactory ]
    [ ( "admissible", boolToExpr admissible )
    , ( "optimal", boolToExpr optimal )
    , ( "pref", boolToExpr pref )
    , ( "alm", boolToExpr alm )
    , lpSolverOption lpSolver
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Landmark-cut_heuristic>
data LMCutConfiguration =
  LMCutConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    }


lmcut :: LMCutConfiguration -> Expr
lmcut LMCutConfiguration{ transform, cacheEstimates } =
  App
    "lmcut"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Merge-and-shrink_heuristic>
data MergeAndShrinkConfiguration =
  MergeAndShrinkConfiguration
    { transform :: AbstractTask
    , cacheEstimates :: Bool
    , mergeStrategy :: MergeStrategy
    , shrinkStrategy :: ShrinkStrategy
    , labelReduction :: LabelReduction
    , pruneUnreachableStates :: Bool
    , pruneIrrelevantStates :: Bool
    , maxStates :: Maybe Int
    , maxStatesBeforeMerge :: Maybe Int
    , thresholdBeforeMerge :: Maybe Int
    , verbosity :: Verbosity
    }


mergeAndShrink :: MergeAndShrinkConfiguration -> Expr
mergeAndShrink MergeAndShrinkConfiguration{ transform, cacheEstimates, mergeStrategy, shrinkStrategy, labelReduction, pruneUnreachableStates, pruneIrrelevantStates, maxStates, maxStatesBeforeMerge, thresholdBeforeMerge, verbosity } =
  App
    "merge_and_shrink"
    []
    [ transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    , ( "merge_strategy", mergeStrategyToExpr mergeStrategy )
    , ( "shrink_strategy", shrinkStrategyToExpr shrinkStrategy )
    , ( "label_reduction", labelReductionToExpr labelReduction )
    , ( "prune_unreachable_states", boolToExpr pruneUnreachableStates )
    , ( "prune_irrelevant_states", boolToExpr pruneIrrelevantStates )
    , maxStatesOption maxStates
    , maxStatesBeforeMergeOption maxStatesBeforeMerge
    , thresholdBeforeMergeOption thresholdBeforeMerge
    , ( "verbosity", verbosityToExpr verbosity )
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Operator_counting_heuristic>
data OperatorCountingConfiguration =
  OperatorCountingConfiguration
    { constraintGenerators :: [ ConstraintGenerator ]
    , lpSolver :: LPSolver
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


operatorCounting :: OperatorCountingConfiguration -> Expr
operatorCounting OperatorCountingConfiguration{ constraintGenerators, lpSolver, transform, cacheEstimates } =
  App
    "operatorcounting"
    [ List ( map constraintGeneratorToExpr constraintGenerators ) ]
    [ lpSolverOption lpSolver
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Sample-based_potential_heuristics>
data SampleBasedPotentialsConfiguration =
  SampleBasedPotentialsConfiguration
    { numHeuristics :: Maybe Int
    , numSamples :: Maybe Int
    , maxPotential :: Maybe Double
    , lpSolver :: LPSolver
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    , randomSeed :: Maybe Int
    }


sampleBasedPotentials :: SampleBasedPotentialsConfiguration -> Expr
sampleBasedPotentials SampleBasedPotentialsConfiguration{ numHeuristics, numSamples, maxPotential, lpSolver, transform, cacheEstimates, randomSeed } =
  App
    "sample_based_potentials"
    []
    [ ( "num_heuristics", maybe infinity intToExpr numHeuristics )
    , ( "num_samples", maybe infinity intToExpr numSamples )
    , ( "max_potential", maybe infinity doubleToExpr maxPotential )
    , lpSolverOption lpSolver
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Canonical_PDB>
data CanonicalPDBConfiguration =
  CanonicalPDBConfiguration
    { patterns :: PatternCollectionGenerator
    , maxTimeDominancePruning :: Maybe Double
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


cpdbs :: CanonicalPDBConfiguration -> Expr
cpdbs CanonicalPDBConfiguration{ patterns, maxTimeDominancePruning, transform, cacheEstimates } =
  App
    "cpdbs"
    []
    [ ( "patterns", patternCollectionGeneratorToExpr patterns )
    , ( "max_time_dominance_pruning", maybe infinity doubleToExpr maxTimeDominancePruning )
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#iPDB>
data IPDBConfiguration =
  IPDBConfiguration
    { pdbMaxSize :: Maybe Int
    , collectionMaxSize :: Maybe Int
    , numSamples :: Maybe Int
    , minImprovement :: Maybe Int
    , maxTime :: Maybe Double
    , randomSeed :: Maybe Int
    , maxTimeDominancePruning :: Maybe Double
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


ipdb :: IPDBConfiguration -> Expr
ipdb IPDBConfiguration{ pdbMaxSize, collectionMaxSize, numSamples, minImprovement, maxTime, randomSeed, maxTimeDominancePruning, transform, cacheEstimates } =
  App
    "ipdb"
    []
    [ pdbMaxSizeOption pdbMaxSize
    , ( "collection_max_size", maybe infinity intToExpr collectionMaxSize )
    , ( "num_samples", maybe infinity intToExpr numSamples )
    , ( "min_improvement", maybe infinity intToExpr minImprovement )
    , maxTimeExpr maxTime
    , randomSeedExpr randomSeed
    , ( "max_time_dominance_pruning", maybe infinity doubleToExpr maxTimeDominancePruning )
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/Evaluator#Zero-One_PDB>
data ZeroOnePDBConfiguration =
  ZeroOnePDBConfiguration
    { patterns :: PatternCollectionGenerator
    , transform :: AbstractTask
    , cacheEstimates :: Bool
    }


zopdbs :: ZeroOnePDBConfiguration -> Expr
zopdbs ZeroOnePDBConfiguration{ patterns, transform, cacheEstimates } =
  App
    "zopdbs"
    []
    [ ( "patterns", patternCollectionGeneratorToExpr patterns )
    , transformExpr transform
    , cacheEstimatesExpr cacheEstimates
    ]


-- | See <http://www.fast-downward.org/Doc/PruningMethod#StubbornSetsEC>
data StubbornSetsConfiguration =
  StubbornSetsConfiguration
    { minRequiredPruningRatio :: Ratio Int
    , expansionsBeforeCheckingPruningRatio :: Maybe Int
    }


stubbornSetsOptions :: StubbornSetsConfiguration -> [ ( String, Expr ) ]
stubbornSetsOptions StubbornSetsConfiguration{ minRequiredPruningRatio, expansionsBeforeCheckingPruningRatio } =
  [ ( "min_required_pruning_ratio", doubleToExpr ( realToFrac minRequiredPruningRatio ) )
  , ( "expansions_before_checking_pruning_ratio", maybe infinity intToExpr expansionsBeforeCheckingPruningRatio )
  ]


stubbornSetsEc :: StubbornSetsConfiguration -> Expr
stubbornSetsEc =
  App "stubborn_sets_ec" [] . stubbornSetsOptions


stubbornSetsSimple :: StubbornSetsConfiguration -> Expr
stubbornSetsSimple =
  App "stubborn_sets_simple" [] . stubbornSetsOptions


-- | See <http://www.fast-downward.org/Doc/OpenList#Alternation_open_list>
data AltConfiguration =
  AltConfiguration
    { sublists :: [ OpenList ]
    , boost :: Int
    }


alt :: AltConfiguration -> Expr
alt AltConfiguration{ sublists, boost } =
  App
    "alt"
    [ List ( map openListToExpr sublists ) ]
    [ ( "boost", intToExpr boost ) ]


-- | See <http://www.fast-downward.org/Doc/OpenList#Epsilon-greedy_open_list>
data EpsilonGreedyConfiguration =
  EpsilonGreedyConfiguration
    { eval :: Evaluator
    , prefOnly :: Bool
    , epsilon :: Ratio Int
    , randomSeed :: Maybe Int
    }


epsilonGreedy :: EpsilonGreedyConfiguration -> Expr
epsilonGreedy EpsilonGreedyConfiguration{ eval, prefOnly, epsilon, randomSeed } =
  App
    "epsilon_greedy"
    [ evaluatorToExpr eval ]
    [ prefOnlyExpr prefOnly
    , ( "epsilon", doubleToExpr ( realToFrac epsilon ) )
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/OpenList#Pareto_open_list>
data ParetoConfiguration =
  ParetoConfiguration
    { evals :: [ Evaluator ]
    , prefOnly :: Bool
    , stateUniformSelection :: Bool
    , randomSeed :: Maybe Int
    }


pareto :: ParetoConfiguration -> Expr
pareto ParetoConfiguration{ evals, prefOnly, stateUniformSelection, randomSeed } =
  App
    "pareto_configuration"
    [ List ( map evaluatorToExpr evals ) ]
    [ prefOnlyExpr prefOnly
    , ( "state_uniform_selection", boolToExpr stateUniformSelection )
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/OpenList#Standard_open_list>
data SingleConfiguration =
  SingleConfiguration
    { eval :: Evaluator
    , prefOnly :: Bool
    }


single :: SingleConfiguration -> Expr
single SingleConfiguration{ eval, prefOnly } =
  App "single" [ evaluatorToExpr eval ] [ prefOnlyExpr prefOnly ]


-- | See <http://www.fast-downward.org/Doc/OpenList#Tie-breaking_open_list>
data TiebreakingConfiguration =
  TiebreakingConfiguration
    { evals :: [ Evaluator ]
    , prefOnly :: Bool
    , unsafePruning :: Bool
    }


tiebreaking :: TiebreakingConfiguration -> Expr
tiebreaking TiebreakingConfiguration{ evals, prefOnly, unsafePruning } =
  App
    "tiebreaking"
    [ List ( map evaluatorToExpr evals ) ]
    [ prefOnlyExpr prefOnly
    , ( "unsafe_pruning", boolToExpr unsafePruning )
    ]


-- | See <http://www.fast-downward.org/Doc/OpenList#Type-based_open_list>
data TypeBasedConfiguration =
  TypeBasedConfiguration
    { evaluators :: [ Evaluator ]
    , randomSeed :: Maybe Int
    }


typeBased :: TypeBasedConfiguration -> Expr
typeBased TypeBasedConfiguration{ evaluators, randomSeed } =
  App
    "type_based"
    [ List ( map evaluatorToExpr evaluators ) ]
    [ randomSeedExpr randomSeed ]


-- | See <http://www.fast-downward.org/Doc/AbstractTask>
data AbstractTask
  = AdaptCost CostType
  | NoTransform


abstractTaskToExpr :: AbstractTask -> Expr
abstractTaskToExpr =
  \case
    NoTransform ->
      App "no_transform" [] []

    AdaptCost ct ->
      App "adapt_cost" [] [ ( "cost_type", costTypeToExpr ct ) ]


data LPSolver
  = CLP
  | CPLEX
  | GUROBI
  deriving
    ( Show )


lpSolverToExpr :: LPSolver -> Expr
lpSolverToExpr =
  Lit . show


-- | See <http://www.fast-downward.org/Doc/SubtaskGenerator>
data SubtaskGenerator
  = Goals GoalsConfiguration
  | Landmarks LandmarksConfiguration
  | Original OriginalConfiguration


subtaskToExpr :: SubtaskGenerator -> Expr
subtaskToExpr =
  \case
    Goals cfg ->
      goals cfg

    Landmarks cfg ->
      landmarks cfg

    Original cfg ->
      original cfg


data CEGARPick
  = Random
  | MinUnwanted
  | MaxUnwanted
  | MinRefined
  | MaxRefined
  | MinHAdd
  | MaxHAdd


cegarPickToExpr :: CEGARPick -> Expr
cegarPickToExpr =
  \case
    Random ->
      Lit "RANDOM"

    MinUnwanted ->
      Lit "MIN_UNWANTED"

    MaxUnwanted ->
      Lit "MAX_UNWANTED"

    MinRefined ->
      Lit "MIN_REFINED"

    MaxRefined ->
      Lit "MX_REFINED"

    MinHAdd ->
      Lit "MIN_HADD"

    MaxHAdd ->
      Lit "MAX_HADD"


-- | See <http://www.fast-downward.org/Doc/LandmarkFactory>
data LandmarkFactory
  = LMExhaust LMExhaustConfiguration
  | LMHM LMHMConfiguration
  | LMMerged LMMergedConfiguration
  | LMRHW LMRHWConfiguration
  | LMZG LMZGConfiguration


landmarkFactoryToExpr :: LandmarkFactory -> Expr
landmarkFactoryToExpr =
  \case
    LMExhaust cfg ->
      lmexhaust cfg

    LMHM cfg ->
      lmhm cfg

    LMMerged cfg ->
      lmMerged cfg

    LMRHW cfg ->
      lmRHW cfg

    LMZG cfg ->
      lmzg cfg


-- | See <http://www.fast-downward.org/Doc/MergeStrategy>
data MergeStrategy
  = MergePrecomputed MergeTree
  | MergeSCCs MergeSCCsConfiguration
  | MergeStateless MergeSelector


mergeStrategyToExpr :: MergeStrategy -> Expr
mergeStrategyToExpr =
  \case
    MergePrecomputed cfg ->
      App "merge_precomputed" [ mergeTreeToExpr cfg ] []

    MergeSCCs cfg ->
      mergeSCCs cfg

    MergeStateless cfg ->
      App "merge_stateless" [ mergeSelectorToExpr cfg ] []


-- | See <http://www.fast-downward.org/Doc/ShrinkStrategy>
data ShrinkStrategy
  = Bisimulation BisimulationConfiguration
  | FPreserving FPreservingConfiguration
  | RandomShrink ( Maybe Int )


shrinkStrategyToExpr :: ShrinkStrategy -> Expr
shrinkStrategyToExpr =
  \case
    Bisimulation cfg ->
      bisimulation cfg

    FPreserving cfg ->
      fPreserving cfg

    RandomShrink randomSeed ->
      App "shrink_random" [] [ randomSeedExpr randomSeed ]


newtype LabelReduction
  = ExactGeneralizedLabelReduction ExactGeneralizedLabelReductionConfiguration


labelReductionToExpr :: LabelReduction -> Expr
labelReductionToExpr ( ExactGeneralizedLabelReduction cfg ) =
  exact cfg


data Verbosity =
  Silent | Basic | Verbose
  deriving
    ( Show )


verbosityToExpr :: Verbosity -> Expr
verbosityToExpr =
  Lit . map toLower . show


data ConstraintGenerator
  = LMCutConstraints
  | PosthocOptimizationConstraints PatternCollectionGenerator
  | StateEquationConstraints


constraintGeneratorToExpr :: ConstraintGenerator -> Expr
constraintGeneratorToExpr =
  \case
    LMCutConstraints ->
      App "lmcut_constraints" [] []

    PosthocOptimizationConstraints cfg ->
      App "pho_constraints" [] [ ( "patterns", patternCollectionGeneratorToExpr cfg ) ]

    StateEquationConstraints ->
      App "state_equation_constraints" [] []


-- | See <http://www.fast-downward.org/Doc/PatternCollectionGenerator>
data PatternCollectionGenerator
  = Combo ( Maybe Int )
  | Genetic GeneticConfiguration
  | Hillclimbing HillclimbingConfiguration
  | ManualPattern [ [ Int ] ]
  | Systematic SystematicConfiguration


patternCollectionGeneratorToExpr :: PatternCollectionGenerator -> Expr
patternCollectionGeneratorToExpr =
  \case
    Combo maxStates ->
      App "combo" [] [ maxStatesOption maxStates ]

    Genetic cfg ->
      genetic cfg

    Hillclimbing cfg ->
      hillclimbing cfg

    ManualPattern cfg ->
      App
        "manual_patterns"
        [ List ( map ( List . map intToExpr ) cfg ) ]
        []

    Systematic cfg ->
      systematic cfg


-- | See <http://www.fast-downward.org/Doc/SubtaskGenerator#goals>
data GoalsConfiguration =
  GoalsConfiguration
    { order :: GoalsOrder
    , randomSeed :: Maybe Int
    }


goals :: GoalsConfiguration -> Expr
goals GoalsConfiguration{ order, randomSeed } =
  App
    "goals"
    []
    [ orderExpr order
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/SubtaskGenerator#landmarks>
data LandmarksConfiguration =
  LandmarksConfiguration
    { order :: GoalsOrder
    , randomSeed :: Maybe Int
    , combineFacts :: Bool
    }


landmarks :: LandmarksConfiguration -> Expr
landmarks LandmarksConfiguration{ order, randomSeed, combineFacts } =
  App
    "landmarks"
    []
    [ orderExpr order
    , randomSeedExpr randomSeed
    , ( "combine_facts", boolToExpr combineFacts )
    ]


newtype OriginalConfiguration =
  OriginalConfiguration
    { copies :: Maybe Int }


original :: OriginalConfiguration -> Expr
original OriginalConfiguration{ copies } =
  App "original" [] [ ( "copies", maybe infinity intToExpr copies ) ]


-- | See <http://www.fast-downward.org/Doc/LandmarkFactory#Exhaustive_Landmarks>
data LMExhaustConfiguration =
  LMExhaustConfiguration
    { reasonableOrders :: Bool
    , onlyCausalLandmarks :: Bool
    , disjunctiveLandmarks :: Bool
    , conjunctiveLandmarks :: Bool
    , noOrders :: Bool
    }


lmexhaust :: LMExhaustConfiguration -> Expr
lmexhaust LMExhaustConfiguration{ reasonableOrders, onlyCausalLandmarks, disjunctiveLandmarks, conjunctiveLandmarks, noOrders } =
  App
    "lm_exhaust"
    []
    [ ( "reasonable_orders", boolToExpr reasonableOrders )
    , ( "only_causal_landmarks", boolToExpr onlyCausalLandmarks )
    , ( "disjunctive_landmarks", boolToExpr disjunctiveLandmarks )
    , ( "conjunctive_landmarks", boolToExpr conjunctiveLandmarks )
    , ( "no_orders", boolToExpr noOrders )
    ]


-- | See <http://www.fast-downward.org/Doc/LandmarkFactory#h.5Em_Landmarks>
data LMHMConfiguration =
  LMHMConfiguration
    { m :: Int
    , reasonableOrders :: Bool
    , onlyCausalLandmarks :: Bool
    , disjunctiveLandmarks :: Bool
    , conjunctiveLandmarks :: Bool
    , noOrders :: Bool
    }


lmhm :: LMHMConfiguration -> Expr
lmhm LMHMConfiguration{ m, reasonableOrders, onlyCausalLandmarks, disjunctiveLandmarks, conjunctiveLandmarks, noOrders } =
  App
    "lm_hm"
    []
    [ ( "m", intToExpr m )
    , ( "reasonable_orders", boolToExpr reasonableOrders )
    , ( "only_causal_landmarks", boolToExpr onlyCausalLandmarks )
    , ( "disjunctive_landmarks", boolToExpr disjunctiveLandmarks )
    , ( "conjunctive_landmarks", boolToExpr conjunctiveLandmarks )
    , ( "no_orders", boolToExpr noOrders )
    ]


-- | See <http://www.fast-downward.org/Doc/LandmarkFactory#Merged_Landmarks>
data LMMergedConfiguration =
  LMMergedConfiguration
    { factories :: [ LandmarkFactory ]
    , reasonableOrders :: Bool
    , onlyCausalLandmarks :: Bool
    , disjunctiveLandmarks :: Bool
    , conjunctiveLandmarks :: Bool
    , noOrders :: Bool
    }


lmMerged :: LMMergedConfiguration -> Expr
lmMerged LMMergedConfiguration{ factories, reasonableOrders, onlyCausalLandmarks, disjunctiveLandmarks, conjunctiveLandmarks, noOrders } =
  App
    "lm_merged"
    [ List ( map landmarkFactoryToExpr factories ) ]
    [ ( "reasonable_orders", boolToExpr reasonableOrders )
    , ( "only_causal_landmarks", boolToExpr onlyCausalLandmarks )
    , ( "disjunctive_landmarks", boolToExpr disjunctiveLandmarks )
    , ( "conjunctive_landmarks", boolToExpr conjunctiveLandmarks )
    , ( "no_orders", boolToExpr noOrders )
    ]


-- | See <http://www.fast-downward.org/Doc/LandmarkFactory#RHW_Landmarks>.
data LMRHWConfiguration =
  LMRHWConfiguration
    { reasonableOrders :: Bool
    , onlyCausalLandmarks :: Bool
    , disjunctiveLandmarks :: Bool
    , conjunctiveLandmarks :: Bool
    , noOrders :: Bool
    }


lmRHW :: LMRHWConfiguration -> Expr
lmRHW LMRHWConfiguration{ reasonableOrders, onlyCausalLandmarks, disjunctiveLandmarks, conjunctiveLandmarks, noOrders } =
  App
    "lm_rhw"
    []
    [ ( "reasonable_orders", boolToExpr reasonableOrders )
    , ( "only_causal_landmarks", boolToExpr onlyCausalLandmarks )
    , ( "disjunctive_landmarks", boolToExpr disjunctiveLandmarks )
    , ( "conjunctive_landmarks", boolToExpr conjunctiveLandmarks )
    , ( "no_orders", boolToExpr noOrders )
    ]


-- | See <http://www.fast-downward.org/Doc/LandmarkFactory#Zhu.2FGivan_Landmarks>
data LMZGConfiguration =
  LMZGConfiguration
    { reasonableOrders :: Bool
    , onlyCausalLandmarks :: Bool
    , disjunctiveLandmarks :: Bool
    , conjunctiveLandmarks :: Bool
    , noOrders :: Bool
    }


lmzg :: LMZGConfiguration -> Expr
lmzg LMZGConfiguration{ reasonableOrders, onlyCausalLandmarks, disjunctiveLandmarks, conjunctiveLandmarks, noOrders } =
  App
    "lm_zg"
    []
    [ ( "reasonable_orders", boolToExpr reasonableOrders )
    , ( "only_causal_landmarks", boolToExpr onlyCausalLandmarks )
    , ( "disjunctive_landmarks", boolToExpr disjunctiveLandmarks )
    , ( "conjunctive_landmarks", boolToExpr conjunctiveLandmarks )
    , ( "no_orders", boolToExpr noOrders )
    ]


newtype MergeTree
  = LinearMergeTree LinearMergeTreeConfiguration


mergeTreeToExpr :: MergeTree -> Expr
mergeTreeToExpr ( LinearMergeTree cfg ) =
  linear cfg


-- | See <http://www.fast-downward.org/Doc/MergeStrategy#Merge_strategy_SSCs>
data MergeSCCsConfiguration =
  MergeSCCsConfiguration
    { orderOfSCCs :: OrderOfSCCs
    , mergeTree :: Maybe MergeTree
    , mergeSelector :: Maybe MergeSelector
    }


mergeSCCs :: MergeSCCsConfiguration -> Expr
mergeSCCs MergeSCCsConfiguration{ orderOfSCCs, mergeTree, mergeSelector } =
  App
    "merge_sccs"
    []
    [ ( "order_of_sccs", orderOfSCCsToExpr orderOfSCCs )
    , ( "merge_tree", maybe none mergeTreeToExpr mergeTree )
    , ( "merge_selector", maybe none mergeSelectorToExpr mergeSelector )
    ]


newtype MergeSelector
  = ScoreBasedFiltering [ MergeScoringFunction ]


mergeSelectorToExpr :: MergeSelector -> Expr
mergeSelectorToExpr ( ScoreBasedFiltering scoringFunctions ) =
  App "score_based_filtering" [ List ( map mergeScoringFunctionToExpr scoringFunctions ) ] []


-- | See <http://www.fast-downward.org/Doc/ShrinkStrategy#Bismulation_based_shrink_strategy>
data BisimulationConfiguration =
  BisimulationConfiguration
    { greedy :: Bool
    , atLimit :: BisimulationLimitStrategy
    }


bisimulation :: BisimulationConfiguration -> Expr
bisimulation BisimulationConfiguration{ greedy, atLimit } =
  App
    "shrink_bisimulation"
    []
    [ ( "greedy", boolToExpr greedy )
    , ( "at_limit", bisimulationLimitStrategyToExpr atLimit )
    ]


-- | See <http://www.fast-downward.org/Doc/ShrinkStrategy#f-preserving_shrink_strategy>
data FPreservingConfiguration =
  FPreservingConfiguration
    { randomSeed :: Maybe Int
    , shrinkF :: HighLow
    , shrinkH :: HighLow
    }


fPreserving :: FPreservingConfiguration -> Expr
fPreserving FPreservingConfiguration{ randomSeed, shrinkF, shrinkH } =
  App
    "shrink_fh"
    []
    [ randomSeedExpr randomSeed
    , ( "shrink_f", highLowToExpr shrinkF )
    , ( "shrink_h", highLowToExpr shrinkH )
    ]


-- | See <http://www.fast-downward.org/Doc/LabelReduction#Exact_generalized_label_reduction>
data ExactGeneralizedLabelReductionConfiguration =
  ExactGeneralizedLabelReductionConfiguration
    { beforeShrinking :: Bool
    , beforeMerging :: Bool
    , method :: LabelReductionMethod
    , systemOrder :: SystemOrder
    , randomSeed :: Maybe Int
    }


exact :: ExactGeneralizedLabelReductionConfiguration -> Expr
exact ExactGeneralizedLabelReductionConfiguration{ beforeShrinking, beforeMerging, method, systemOrder, randomSeed } =
  App
    "exact"
    [ boolToExpr beforeShrinking
    , boolToExpr beforeMerging
    ]
    [ ( "method", labelReductionMethodToExpr method )
    , ( "system_order", systemOrderToExpr systemOrder )
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/PatternCollectionGenerator#Genetic_Algorithm_Patterns>
data GeneticConfiguration =
  GeneticConfiguration
    { pdbMaxSize :: Maybe Int
    , numCollections :: Maybe Int
    , numEpisodes :: Maybe Int
    , mutationProbability :: Ratio Int
    , disjoint :: Bool
    , randomSeed :: Maybe Int
    }


genetic :: GeneticConfiguration -> Expr
genetic GeneticConfiguration{ pdbMaxSize, numCollections, numEpisodes, mutationProbability, disjoint, randomSeed } =
  App
    "genetic"
    []
    [ pdbMaxSizeOption pdbMaxSize
    , ( "num_collections", maybe infinity intToExpr numCollections )
    , ( "num_episodes", maybe infinity intToExpr numEpisodes )
    , ( "mutation_probability", doubleToExpr ( realToFrac mutationProbability ) )
    , ( "disjoint", boolToExpr disjoint )
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/PatternCollectionGenerator#hillclimbing>
data HillclimbingConfiguration =
  HillclimbingConfiguration
    { pdbMaxSize :: Maybe Int
    , collectionMaxSize :: Maybe Int
    , numSamples :: Maybe Int
    , minImprovement :: Maybe Int
    , maxTime :: Maybe Double
    , randomSeed :: Maybe Int
    }


hillclimbing :: HillclimbingConfiguration -> Expr
hillclimbing HillclimbingConfiguration{ pdbMaxSize, collectionMaxSize, numSamples, minImprovement, maxTime, randomSeed } =
  App
    "hillclimbing"
    []
    [ pdbMaxSizeOption pdbMaxSize
    , ( "collection_max_size", maybe infinity intToExpr collectionMaxSize )
    , ( "num_samples", maybe infinity intToExpr numSamples )
    , ( "min_improvement", maybe infinity intToExpr minImprovement )
    , maxTimeExpr maxTime
    , randomSeedExpr randomSeed
    ]


-- | See <http://www.fast-downward.org/Doc/PatternCollectionGenerator#Systematically_generated_patterns>
data SystematicConfiguration =
  SystematicConfiguration
    { patternMaxSize :: Maybe Int
    , onlyInterestingPatterns :: Bool
    }


systematic :: SystematicConfiguration -> Expr
systematic SystematicConfiguration{ patternMaxSize, onlyInterestingPatterns } =
  App
    "systematic"
    []
    [ ( "pattern_max_size", maybe infinity intToExpr patternMaxSize )
    , ( "only_interesting_patterns", boolToExpr onlyInterestingPatterns )
    ]


data GoalsOrder =
  OriginalOrder | RandomOrder | HAddUp | HAddDown


goalsOrderToExpr :: GoalsOrder -> Expr
goalsOrderToExpr =
  \case
    OriginalOrder ->
      Lit "ORIGINAL"

    RandomOrder ->
      Lit "RANDOM"

    HAddUp ->
      Lit "HADD_UP"

    HAddDown ->
      Lit "HADD_DOWN"


-- | See <http://www.fast-downward.org/Doc/MergeTree#Linear_merge_trees>
data LinearMergeTreeConfiguration =
  LinearMergeTreeConfiguration
    { randomSeed :: Maybe Int
    , updateOption :: UpdateOption
    , variableOrder :: VariableOrder
    }


linear :: LinearMergeTreeConfiguration -> Expr
linear LinearMergeTreeConfiguration { randomSeed, updateOption, variableOrder } =
  App
    "linear"
    []
    [ randomSeedExpr randomSeed
    , ( "update_option", updateOptionToExpr updateOption )
    , ( "variable_order", variableOrderToExpr variableOrder )
    ]


data OrderOfSCCs =
  Topological | ReverseTopological | Decreasing | Increasing


orderOfSCCsToExpr :: OrderOfSCCs -> Expr
orderOfSCCsToExpr =
  \case
    Topological ->
      Lit "topological"

    ReverseTopological ->
      Lit "reverse_topological"

    Decreasing ->
      Lit "decreasing"

    Increasing ->
      Lit "increasing"


-- | See <http://www.fast-downward.org/Doc/MergeScoringFunction>
data MergeScoringFunction
  = DFP
  | GoalRelevance
  | MIASM MIASMConfiguration
  | SingleRandom ( Maybe Int )
  | TotalOrder TotalOrderConfiguration


mergeScoringFunctionToExpr :: MergeScoringFunction -> Expr
mergeScoringFunctionToExpr =
  \case
    DFP ->
      App "dfp" [] []

    GoalRelevance ->
      App "goal_relevance" [] []

    MIASM cfg ->
      miasm cfg

    SingleRandom cfg ->
      App "single_random" [] [ randomSeedExpr cfg ]

    TotalOrder cfg ->
      totalOrder cfg


data BisimulationLimitStrategy =
  Return | UseUp


bisimulationLimitStrategyToExpr :: BisimulationLimitStrategy -> Expr
bisimulationLimitStrategyToExpr =
  \case
    Return ->
      Lit "RETURN"

    UseUp ->
      Lit "USE_UP"


data HighLow =
  High | Low
  deriving
    ( Show )


highLowToExpr :: HighLow -> Expr
highLowToExpr =
  Lit . map toUpper . show


data LabelReductionMethod =
  TwoTransitionSystems | AllTransitionSystems | AllTransitionSystemsWithFixpoint


labelReductionMethodToExpr :: LabelReductionMethod -> Expr
labelReductionMethodToExpr =
  \case
    TwoTransitionSystems ->
      Lit "TWO_TRANSITION_SYSTEMS"

    AllTransitionSystems ->
      Lit  "ALL_TRANSITION_SYSTEMS"

    AllTransitionSystemsWithFixpoint ->
      Lit  "ALL_TRANSITION_SYSTEMS_WITH_FIXPOINT"


data SystemOrder =
  RegularSystemOrder | ReverseSystemOrder | RandomSystemOrder


systemOrderToExpr :: SystemOrder -> Expr
systemOrderToExpr =
  \case
    RegularSystemOrder ->
      Lit "REGULAR"

    ReverseSystemOrder ->
      Lit "REVERSE"

    RandomSystemOrder ->
      Lit "RANDOM"


data UpdateOption =
  UseFirst | UseSecond | UseRandom


updateOptionToExpr :: UpdateOption -> Expr
updateOptionToExpr =
  \case
    UseFirst ->
      Lit "use_first"

    UseSecond ->
      Lit "use_second"

    UseRandom ->
      Lit "use_random"


data VariableOrder =
  CGGoalLevel | CGGoalRandom | GoalCGLevel | RandomVariableOrder | LevelOrder | ReverseLevelOrder


variableOrderToExpr :: VariableOrder -> Expr
variableOrderToExpr =
  \case
    CGGoalLevel ->
      Lit "CG_GOAL_LEVEL"

    CGGoalRandom ->
      Lit "CG_GOAL_RANDOM"

    GoalCGLevel ->
      Lit "GOAL_CG_LEVEL"

    RandomVariableOrder ->
      Lit "RANDOM"

    LevelOrder ->
      Lit "LEVEL"

    ReverseLevelOrder ->
      Lit "REVERSE_LEVEL"


data MIASMConfiguration =
  MIASMConfiguration
    { shrinkStrategy :: ShrinkStrategy
    , maxStates :: Maybe Int
    , maxStatesBeforeMerge :: Maybe Int
    , thresholdBeforeMerge :: Maybe Int
    }


miasm :: MIASMConfiguration -> Expr
miasm MIASMConfiguration{ shrinkStrategy, maxStates, maxStatesBeforeMerge, thresholdBeforeMerge } =
  App
    "sf_miasm"
    [ shrinkStrategyToExpr shrinkStrategy ]
    [ maxStatesOption maxStates
    , maxStatesBeforeMergeOption maxStatesBeforeMerge
    , ( "threshold_before_merge", maybe infinity intToExpr thresholdBeforeMerge )
    ]


data TotalOrderConfiguration =
  TotalOrderConfiguration
    { atomicTsOrder :: AtomicTsOrder
    , productTsOrder :: ProductTsOrder
    , atomicBeforeProduct :: Bool
    , randomSeed :: Maybe Int
    }


totalOrder :: TotalOrderConfiguration -> Expr
totalOrder TotalOrderConfiguration{ atomicTsOrder, productTsOrder, atomicBeforeProduct, randomSeed } =
  App
    "total_order"
    []
    [ ( "atomic_ts_order", atomicTsOrderToExpr atomicTsOrder )
    , ( "product_ts_order", productTsOrderToExpr productTsOrder )
    , ( "atomic_before_product", boolToExpr atomicBeforeProduct )
    , randomSeedExpr randomSeed
    ]


data AtomicTsOrder =
  ReverseLevelAtomicTs | LevelAtomicTs | RandomAtomicTs


atomicTsOrderToExpr :: AtomicTsOrder -> Expr
atomicTsOrderToExpr =
  \case
    ReverseLevelAtomicTs ->
      Lit "reverse_level"

    LevelAtomicTs ->
      Lit "level"

    RandomAtomicTs ->
      Lit "random"


data ProductTsOrder =
  OldToNew | NewToOld | RandomProductTsOrder


productTsOrderToExpr :: ProductTsOrder -> Expr
productTsOrderToExpr =
  \case
    OldToNew ->
      Lit "old_to_new"

    NewToOld ->
      Lit "new_to_old"

    RandomProductTsOrder ->
      Lit "random"


randomSeedExpr :: Maybe Int -> ( String, Expr )
randomSeedExpr r =
  ( "random_seed", intToExpr ( fromMaybe (-1) r ) )


prefOnlyExpr :: Bool -> ( String, Expr )
prefOnlyExpr b =
  ( "pref_only", boolToExpr b )


transformExpr :: AbstractTask -> ( String, Expr )
transformExpr t =
  ( "transform", abstractTaskToExpr t )


cacheEstimatesExpr :: Bool -> ( String, Expr )
cacheEstimatesExpr b =
  ( "cache_estimates", boolToExpr b )


maxTimeExpr :: Maybe Double -> ( String, Expr )
maxTimeExpr t =
  ( "max_time", maybe infinity doubleToExpr t )


orderExpr :: GoalsOrder -> ( String, Expr )
orderExpr g =
  ( "order", goalsOrderToExpr g )


lpSolverOption :: LPSolver -> ( String, Expr )
lpSolverOption a =
  ( "lpsolver", lpSolverToExpr a )


maxPotentialOption :: Maybe Double -> ( String, Expr )
maxPotentialOption a =
  ( "max_potential", maybe infinity doubleToExpr a )


maxStatesOption :: Maybe Int -> ( String, Expr )
maxStatesOption a =
  ( "max_states", maybe infinity intToExpr a )


maxStatesBeforeMergeOption :: Maybe Int -> ( String, Expr )
maxStatesBeforeMergeOption a =
  ( "max_states_before_merge", maybe infinity intToExpr a )


thresholdBeforeMergeOption :: Maybe Int -> ( String, Expr )
thresholdBeforeMergeOption a =
  ( "threshold_before_merge", maybe infinity intToExpr a )


pdbMaxSizeOption :: Maybe Int -> ( String, Expr )
pdbMaxSizeOption a =
  ( "pdb_max_size", maybe infinity intToExpr a )
