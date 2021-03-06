{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Pirate.Nyaa where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.ByteString.Char8      (ByteString)
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Lib.Prelude
import           Lib.Types                  as Types
import           Network.HTTP.Req
import           Text.Feed.Import
import           Text.Feed.Types            hiding (RSSItem)
import           Text.RSS.Syntax

ua ∷ ByteString
ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1 RuxitSynthetic/1.0 v966419272 t8706630810854404122 smf=0"

getSearch ∷ Text → Req [Row]
getSearch term = do
    response <- req GET (https "nyaa.si") NoReqBody bsResponse
        $ header "User-Agent" ua
        <> queryParam "page" (Just "rss" :: Maybe Text)
        <> queryParam "q" (Just term :: Maybe Text)
    let body = responseBody response
    let feed = parseFeedString $ toString body
    pure $ case feed of
        Just (RSSFeed (RSS _ _ RSSChannel {rssItems} _)) ->
            (\RSSItem {
                rssItemTitle,
                rssItemOther
            } ->
            let info = (\Element {
                    elementName,
                    elementNodes
                } -> (
                    nameLocalName elementName,
                    (\(ContentText x) -> x) . (\(NodeContent x) -> x) $ head elementNodes
                    )
                        ) <$> rssItemOther
            in Row {
                Types.id = "",
                source = Nyaa,
                title = fromMaybe "" rssItemTitle,
                seeders = read . T.unpack <$> lookup "seeders" info :: Maybe Int,
                leechers = read . T.unpack <$> lookup "leechers" info,
                infoHash = fromMaybe "" $ lookup "infoHash" info,
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
            pure .
            Left $ "Yarr, twas a problem gettin' " <> T.unpack t <> ", cap'n! The ship is sank! Tis: " <> show e
    )
