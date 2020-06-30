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

data TPBRow = TPBRow {
    id        :: Text,
    name      :: Text,
    info_hash :: Text,
    leechers  :: Int,
    seeders   :: Int,
    num_files :: Int,
    size      :: Int,
    username  :: Text,
    added     :: Text, -- todo date
    status    :: Text,
    category  :: Int,
    imdb      :: Maybe Text
} deriving (Generic, Row)

instance Show TPBRow where
    show TPBRow {
        name = rowName,
        leechers = rowLeechers,
        seeders = rowSeeders,
        imdb = rowIMDB
    } = T.unpack rowName <>
        " (" <>
        show rowSeeders <>
        " seeders, " <>
        show rowLeechers <>
        " leechers)." <>
        maybe mempty (T.unpack . (" https://imdb.com/title/" <>)) rowIMDB

instance FromJSON TPBRow where
    parseJSON (Object value) = TPBRow <$>
        value .: "id" <*>
        value .: "name" <*>
        value .: "info_hash" <*>
        (read <$> value .: "leechers") <*>
        (read <$> value .: "seeders") <*>
        (read <$> value .: "num_files") <*>
        (read <$> value .: "size") <*>
        value .: "username" <*>
        value .: "added" <*>
        value .: "status" <*>
        (read <$> value .: "category") <*>
        ((\imdbID -> if imdbID == ""
            then Nothing
            else Just imdbID
        ) <$> value .: "imdb")
    parseJSON _ = error "Invalid JSON"

magnetPrefix ∷ Text
magnetPrefix = "magnet:?xt=urn:btih:"

magnetSuffix ∷ Text
magnetSuffix = "&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969%2Fannounce&tr=udp%3A%2F%2F9.rarbg.to%3A2920%2Fannounce&tr=udp%3A%2F%2Ftracker.opentrackr.org%3A1337&tr=udp%3A%2F%2Ftracker.internetwarriors.net%3A1337%2Fannounce&tr=udp%3A%2F%2Ftracker.leechers-paradise.org%3A6969%2Fannounce&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969%2Fannounce&tr=udp%3A%2F%2Ftracker.pirateparty.gr%3A6969%2Fannounce&tr=udp%3A%2F%2Ftracker.cyberia.is%3A6969%2Fannounceinfo_hash"


-- Don't error on 404. We just wanna check it.
torConfig ∷ HttpConfig
torConfig = defaultHttpConfig {
    httpConfigProxy = Just (Proxy "localhost" 8118)
}

getSearch ∷ APIDomain → Text → Req (JsonResponse [TPBRow])
getSearch apiDomain term = req GET (https apiDomain /: "q.php") NoReqBody jsonResponse
    $ header "User-Agent" "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1 RuxitSynthetic/1.0 v966419272 t8706630810854404122 smf=0"
    <> queryParam "q" (Just term)

queryPirate ∷ APIDomain → Text → ExceptT String IO [TPBRow]
queryPirate apiDomain t = catchE (
        ExceptT .
        try $ responseBody <$> runReq defaultHttpConfig (getSearch apiDomain t) -- torConfig
    ) (
        \(SomeException e) →
            ExceptT .
            return .
            Left $ "Yarr, twas a problem gettin' " <> T.unpack t <> ", cap'n! The ship is sank! Tis: " <> show e
    )
