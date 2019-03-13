{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Variable ( Variable(..), toSAS ) where

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int


data Variable =
  Variable
    { name :: Data.Text.Lazy.Text
    , domain :: Seq Data.Text.Lazy.Text
    , axiomLayer :: Int
    }
  deriving
    ( Show )


toSAS :: Variable -> Data.Text.Lazy.Builder.Builder
toSAS Variable{..} =
    "begin_variable\n"
  <> Data.Text.Lazy.Builder.fromLazyText name <> "\n"
  <> Data.Text.Lazy.Builder.Int.decimal axiomLayer <> "\n"
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length domain ) <> "\n"
  <> foldMap ( \val -> Data.Text.Lazy.Builder.fromLazyText val <> "\n" ) domain
  <> "end_variable"
