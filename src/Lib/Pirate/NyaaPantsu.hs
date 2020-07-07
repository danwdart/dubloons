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
import           Lib.Types as Types
import           Network.HTTP.Req
import           Text.Feed.Import
import           Text.Feed.Types            hiding (RSSItem)
import           Text.RSS.Syntax

ua ∷ ByteString
ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1 RuxitSynthetic/1.0 v966419272 t8706630810854404122 smf=0"

urlPrefix :: Text
urlPrefix = "https://nyaa.net/download/"

getSearch ∷ Text → Req [Row]
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
            } -> Row {
                Types.id = "",
                source = NyaaPantsu,
                title = fromMaybe "" rssItemTitle,
                infoHash = fromMaybe "" $ T.stripPrefix urlPrefix =<< rssItemLink,
                leechers = Nothing,
                seeders = Nothing,
                numFiles = Nothing,
                size = Nothing,
                username = Nothing,
                added = Nothing,
                status = Nothing,
                category = Nothing,
                imdb = Nothing
            }
            ) <$> rssItems
        Just _ -> []
        Nothing -> []

queryPirate ∷ Text → ExceptT String IO [Row]
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