module FastDownward.SAS.DomainIndex ( DomainIndex(..), toSAS ) where

import Data.String ( fromString )
import qualified Data.Text.Lazy


newtype DomainIndex =
  DomainIndex { unDomainIndex :: Int }
  deriving
    ( Eq, Ord, Show )


toSAS :: DomainIndex -> Data.Text.Lazy.Text
toSAS ( DomainIndex i ) =
  fromString ( show i )
