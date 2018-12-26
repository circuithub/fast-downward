{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Variable ( Variable(..), toSAS ) where

import qualified Data.Text.Lazy
import Data.String ( fromString )


data Variable =
  Variable
    { name :: Data.Text.Lazy.Text
    , domain :: [ Data.Text.Lazy.Text ]
    , axiomLayer :: Int
    }
  deriving
    ( Show )


toSAS :: Variable -> Data.Text.Lazy.Text
toSAS Variable{..} =
  Data.Text.Lazy.intercalate
    "\n"
    [ "begin_variable"
    , name
    , fromString ( show axiomLayer )
    , fromString ( show ( length domain ) )
    , Data.Text.Lazy.intercalate "\n" domain
    , "end_variable"
    ]
