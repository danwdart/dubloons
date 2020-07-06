{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults -Wno-unused-imports #-}

module Lib.Pirate.TPB where

import           Control.Exception
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Retry
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import           Data.Function
import           Data.Functor
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import           Lib.Types
import           Network.HTTP.Client        hiding (responseBody)
import           Network.HTTP.Req
import           System.IO

-- Don't error on 404. We just wanna check it.
torConfig ∷ HttpConfig
torConfig = defaultHttpConfig {
    httpConfigProxy = Just (Proxy "localhost" 8118)
}

getSearch ∷ APIDomain → Text → Req (JsonResponse [Row])
getSearch apiDomain term = req GET (https apiDomain /: "q.php") NoReqBody jsonResponse
    $ header "User-Agent" "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1 RuxitSynthetic/1.0 v966419272 t8706630810854404122 smf=0"
    <> queryParam "q" (Just term)

queryPirate ∷ APIDomain → Text → ExceptT String IO [Row]
queryPirate apiDomain t = catchE (
        ExceptT .
        try $ responseBody <$> runReq defaultHttpConfig (getSearch apiDomain t) -- torConfig
    ) (
        \(SomeException e) →
            ExceptT .
            return .
            Left $ "Yarr, twas a problem gettin' " <> T.unpack t <> ", cap'n! The ship is sank! Tis: " <> show e
    )
