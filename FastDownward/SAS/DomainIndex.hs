module FastDownward.SAS.DomainIndex ( DomainIndex(..), toSAS ) where

import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int


newtype DomainIndex =
  DomainIndex { unDomainIndex :: Int }
  deriving
    ( Eq, Ord, Show )


toSAS :: DomainIndex -> Data.Text.Lazy.Builder.Builder
toSAS ( DomainIndex i ) =
  Data.Text.Lazy.Builder.Int.decimal i
