{-# language OverloadedStrings #-}

module FastDownward.SAS.Version ( Version(..), toSAS ) where

import qualified Data.Text.Lazy


data Version =
  SAS3
  deriving
    ( Show )


toSAS :: Version -> Data.Text.Lazy.Text
toSAS v =
  Data.Text.Lazy.intercalate
    "\n"
    [ "begin_version"
    , case v of
        SAS3 ->
          "3"
    , "end_version"
    ]
