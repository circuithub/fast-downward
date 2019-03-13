module FastDownward.SAS.DomainIndex ( DomainIndex(..), toSAS ) where

import Data.Word
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int


newtype DomainIndex =
  DomainIndex { unDomainIndex :: Word16 }
  deriving
    ( Eq, Ord, Show )


toSAS :: DomainIndex -> Data.Text.Lazy.Builder.Builder
toSAS ( DomainIndex i ) =
  Data.Text.Lazy.Builder.Int.decimal i
