module FastDownward.SAS.VariableIndex ( VariableIndex(..), toSAS ) where

import Data.String ( fromString )
import qualified Data.Text.Lazy


newtype VariableIndex =
  VariableIndex { unVariableIndex :: Int }
  deriving
    ( Eq, Ord, Show )


toSAS :: VariableIndex -> Data.Text.Lazy.Text
toSAS ( VariableIndex i ) =
  fromString ( show i )
