{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Pirate.NyaaPantsu where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.ByteString.Char8      (ByteString)
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Lib.Prelude
import           Lib.Types
import           Network.HTTP.Req
import           Text.Feed.Import
import           Text.Feed.Types            hiding (RSSItem)
import           Text.RSS.Syntax

data NyaaPantsuRow = NyaaPantsuRow {
    title       :: Text,
    link        :: Text,
    infoHash    :: Maybe Text
} deriving (Row, Show)

ua ∷ ByteString
ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1 RuxitSynthetic/1.0 v966419272 t8706630810854404122 smf=0"

magnetPrefix :: Text
magnetPrefix = "magnet:?xt=urn:btih:"

magnetSuffix :: Text
magnetSuffix = "&tr=udp://tracker.uw0.xyz:6969/announce&tr=udp://tracker.coppersurfer.tk:6969&tr=udp://tracker.zer0day.to:1337/announce&tr=udp://tracker.leechers-paradise.org:6969&tr=udp://explodie.org:6969&tr=udp://tracker.opentrackr.org:1337&tr=udp://tracker.internetwarriors.net:1337/announce&tr=http://mgtracker.org:6969/announce&tr=udp://ipv6.leechers-paradise.org:6969/announce&tr=http://nyaa.tracker.wf:7777/announce&tr=http://sukebei.tracker.wf:7777/announce&tr=http://tracker.anirena.com:80/announce&tr=http://anidex.moe:6969/announce&tr=udp://tracker.uw0.xyz:6969/announce&tr=udp://tracker.coppersurfer.tk:6969&tr=udp://tracker.zer0day.to:1337/announce&tr=udp://tracker.leechers-paradise.org:6969&tr=udp://explodie.org:6969&tr=udp://tracker.opentrackr.org:1337&tr=udp://tracker.internetwarriors.net:1337/announce&tr=http://mgtracker.org:6969/announce&tr=udp://ipv6.leechers-paradise.org:6969/announce&tr=http://nyaa.tracker.wf:7777/announce&tr=http://sukebei.tracker.wf:7777/announce&tr=http://tracker.anirena.com:80/announce&tr=http://anidex.moe:6969/announce"

urlPrefix :: Text
urlPrefix = "https://nyaa.net/download/"

getSearch ∷ Text → Req [NyaaPantsuRow]
getSearch term = do
    response <- req GET (https "nyaa.net" /: "feed") NoReqBody bsResponse
        $ header "User-Agent" ua
        <> queryParam "q" (Just term :: Maybe Text)
    let body = responseBody response
    let feed = parseFeedString $ toString body
    return $ case feed of
        Just (RSSFeed (RSS _ _ RSSChannel {rssItems} _)) ->
            (\RSSItem {
                rssItemTitle,
                rssItemLink
            } -> NyaaPantsuRow {
                title = fromMaybe "" rssItemTitle,
                link = fromMaybe "" rssItemLink,
                infoHash = T.stripPrefix urlPrefix =<< rssItemLink
            }
            ) <$> rssItems
        Just _ -> []
        Nothing -> []

queryPirate ∷ Text → ExceptT String IO [NyaaPantsuRow]
queryPirate t = catchE (
        ExceptT .
        try $ runReq defaultHttpConfig (getSearch t) -- torConfig
    ) (
        \(SomeException e) →
            ExceptT .
            return .
            Left $ "Yarr, twas a problem gettin' " <> T.unpack t <> ", cap'n! The ship is sank! Tis: " <> show e
    )

-- $> import Control.Monad.Trans.Except

-- $> runExceptT $ Lib.Pirate.NyaaPantsu.queryPirate (pack "mirai nikki")