{-# LANGUAGE NamedFieldPuns, OverloadedStrings, UnicodeSyntax #-}

module Lib.Pirate.Nyaa where

import           Control.Exception
import           Control.Monad.Trans.Except
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import Data.XML.Types
import Lib.Prelude
import           Network.HTTP.Req
import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax
import Debug.Trace

data NyaaRow = NyaaRow {
    title :: Text,
    description :: Text,
    link :: Text
} deriving (Show)

ua :: ByteString
ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1 RuxitSynthetic/1.0 v966419272 t8706630810854404122 smf=0"

getSearch ∷ Text → Req [NyaaRow]
getSearch term = do
    response <- req GET (https "nyaa.si") NoReqBody bsResponse
        $ header "User-Agent" ua
        <> queryParam "page" (Just "rss" :: Maybe Text)
        <> queryParam "q" (Just term :: Maybe Text)
    let body = responseBody response
    let feed = parseFeedString $ toString body
    return $ case feed of
        Just (AtomFeed f) -> undefined
        Just (RSSFeed (RSS _ _ RSSChannel {
            rssTitle,
            rssLink,
            rssDescription,
            rssItems
        } _)) ->
            fmap (\Text.RSS.Syntax.RSSItem {
                rssItemTitle,
                rssItemLink,
                rssItemDescription,
                rssItemOther
            } ->
                traceShow rssItemOther $ NyaaRow {
                    title = fromMaybe "" rssItemTitle,
                    description = fromMaybe "" rssItemDescription,
                    link = fromMaybe "" rssItemLink
                 } {-} <> " " <>
                show rssItemLink <> " " <>
                show rssItemDescription <> " " <>
                show rssItemCategories <> " " <>
                show rssItemComments <> " " <>
                show rssItemSource <> " " <>
                show rssItemAttrs <> " " <>
                show rssItemOther-}
            ) rssItems
        Just (RSS1Feed f) -> undefined
        Just (XMLFeed element) -> undefined
        Nothing -> undefined

queryPirate ∷ Text → ExceptT String IO [NyaaRow]
queryPirate t = catchE (
        ExceptT .
        try $ runReq defaultHttpConfig (getSearch t) -- torConfig
    ) (
        \(SomeException e) →
            ExceptT .
            return .
            Left $ "Yarr, twas a problem gettin' " <> T.unpack t <> ", cap'n! The ship is sank! Tis: " <> show e
    )
