module FastDownward.SAS.VariableIndex ( VariableIndex(..), toSAS ) where

import Data.Word
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int


newtype VariableIndex =
  VariableIndex { unVariableIndex :: Word16 }
  deriving
    ( Eq, Ord, Show )


toSAS :: VariableIndex -> Data.Text.Lazy.Builder.Builder
toSAS ( VariableIndex i ) =
  Data.Text.Lazy.Builder.Int.decimal i
