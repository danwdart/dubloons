{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Discord where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Map.Strict            hiding (null)
import           Data.Text                  as T hiding (concat, head, map,
                                                  null, tail, take, zip)
import           Discord
import           Discord.Requests
import           Discord.Types
-- todo indexify
import qualified Lib.Pirate.Nyaa            as N
import qualified Lib.Pirate.NyaaPantsu      as NP
import qualified Lib.Pirate.TPB             as TPB
import           Lib.Prelude
import           Lib.Types
import           Prelude                    hiding (lookup, map, print,
                                             putStrLn, take, unwords, words,
                                             zip)
import           System.Exit
import           System.Posix.Signals

handleStart ∷ Env → DiscordHandle → IO ()
handleStart dEnv h = do
    putStrLn "Start handler called"
    _ <- sendMsg "-- Arrr, I be here! --"
    tid <- myThreadId
    void $ installHandler keyboardSignal (
        Catch $ do
            _ <- sendMsg "-- Bye, cap'n! --"
            throwTo tid UserInterrupt
            exitSuccess
        ) Nothing
    where
        sendMsg = sendMessage h (envCID dEnv)

sendMessage ∷ DiscordHandle → ChannelId → MessageText → IO MessageResult
sendMessage h cid = restCall h . CreateMessage cid

sendEmbedRow ∷ DiscordHandle → ChannelId → Row → IO MessageResult
sendEmbedRow h cid row = restCall h (CreateMessageEmbed cid "" (CreateEmbed {
    createEmbedAuthorName = T.pack $ show (source row),
    createEmbedAuthorUrl = "",
    createEmbedAuthorIcon = Nothing,
    createEmbedTitle = title row,
    createEmbedUrl = "https://itorrents.org/torrent/" <> infoHash row <> ".torrent",  -- magnet:?xt=urn:btih:B005AA
    createEmbedThumbnail = Nothing,
    createEmbedDescription = "",
    createEmbedImage = Nothing,
    createEmbedFields =
        maybe [] (\s -> [EmbedField "Seeders" (T.pack $ show s) Nothing]) (seeders row) <>
        maybe [] (\l -> [EmbedField "Leechers" (T.pack $ show l) Nothing]) (leechers row) <>
        maybe [] (\i -> [EmbedField "IMDB" (T.pack "https://imdb.com/title/" <> i) Nothing]) (imdb row),
    createEmbedFooterText = "",
    createEmbedFooterIcon = Nothing
    }))

getQuery ∷ Env → ChannelId → DiscordHandle → Query → IO ()
getQuery dEnv cid h query = void $ forkIO $ do
    _ <- sendMsg $ "Yarrrr, I be gettin' " <> query <> " for ye!"
    resTPB <- runExceptT $ TPB.queryPirate apiDomain query
    resN <- runExceptT $ N.queryPirate query
    resNP <- runExceptT $ NP.queryPirate query
    let results = concat =<< [resTPB, resN, resNP]
    let r = zip [1..] results
    mapM_ sendMsgRow (take 20 r)
    when (null results) $
        void . sendMsg $ "Yarrrr, there be nothin' fer " <> query <> "!"
    where
        apiDomain = envApiDomain dEnv
        sendMsg = sendMessage h cid
        sendMsgRow = sendEmbedRow h cid

parseMsg ∷ Env →  ChannelId → DiscordHandle → Query → Command → IO ()
parseMsg dEnv cid h query = \case
    "get" → getQuery dEnv cid h query
    _ → return ()

handleMessage ∷ Env → Username → ChannelId → DiscordHandle → MessageText → IO ()
handleMessage dEnv un cid h = \case
    "/hello" → void $ sendMsg $ "Ahoy, matey, " <> un <> "!"
    "/status" → void $ sendMsg "Yarr, all hands on deck!"
    "/help" → void $ sendMsg $ "Arr, ye can say:\n" <>
        "/hello - I be doin' an echo!\n" <>
        "/status - I tell ye how I be doin'!\n" <>
        "/help - This!\n" <>
        "/quit - I say bye cap'n!"
    "/quit" → do
        _ <- sendMsg "Bye, Cap'n!"
        putStrLn "Received quit message"
        stopDiscord h
    msg → do
        -- TODO pattern match had an issue so wat
        putStrLn $ un <> " said: " <> msg <> " in " <> T.pack (show cid)
        let w = T.words msg
        if null w then
            putStrLn "Empty message?"
        else do
            let cmd = head w
            let queries = tail w
            let query = T.unwords queries
            parseMsg dEnv cid h query cmd
    where
        sendMsg = sendMessage h cid

handleEvent ∷ Env → DiscordHandle → Event → IO ()
handleEvent dEnv h = \case
    MessageCreate Message {
        messageId = mid,
        messageChannel = cid,
        messageAuthor = author,
        messageText = msg,
        messageTimestamp = time,
        messageEdited = timeEdited,
        messageTts = tts,
        messageEveryone = isEveryone,
        messageMentions = mentions,
        messageMentionRoles = mentionRoles,
        messageAttachments = attachments,
        messageEmbeds = embeds,
        messageNonce = nonce,
        messagePinned = isPinned,
        messageGuild = gid
    } → do
        unless (userIsBot author) $ do
            let un = userName author
            putStrLn $
                un <>
                "#" <>
                userDiscrim author <>
                " said: " <>
                msg <>
                " in gid: " <>
                T.pack (show gid)
            putStrLn $ "MessageCreate " <> show (
                mid,
                cid,
                author,
                msg,
                time,
                timeEdited,
                tts,
                isEveryone,
                mentions,
                mentionRoles,
                attachments,
                embeds,
                nonce,
                isPinned,
                gid
                )
            handleMessage dEnv un cid h msg
    Ready i user cids gidsunavailable txt →
        putStrLn $
        "Received Ready event. Details: " <>
        show (
            i,
            userName user,
            userDiscrim user,
            cids,
            gidsunavailable,
            txt
            )
    GuildCreate
        Guild { guildRoles = gr }
        GuildInfo { guildMembers = gm } →
        putStrLn $ "Received GuildCreate event: " <> show grs <> "..." <> show gms where
            grs = roleName <$> gr
            gms = (\m → (
                userName . memberUser $ m,
                userDiscrim . memberUser $ m,
                memberNick m
                )) <$> gm
    ChannelCreate ch → case ch of
        ChannelText {
            channelId = cid,
            channelGuild = gid,
            channelName = cname,
            channelPosition = cpos,
            channelPermissions = cperms,
            channelTopic = ctop,
            channelLastMessage = clm
        } → putStrLn $ "ChannelText: " <> show (
            cid,
            gid,
            cname,
            cpos,
            cperms,
            ctop,
            clm
            )
        ChannelVoice {
            channelId = cid,
            channelGuild = gid,
            channelName = cname,
            channelPosition = cpos,
            channelBitRate = cbitrate,
            channelUserLimit = cuserlimit
        } → putStrLn $ "ChannelVoice: " <> show (
            cid,
            gid,
            cname,
            cpos,
            cbitrate,
            cuserlimit
            )
        ChannelDirectMessage {
            channelId = cid,
            channelRecipients = crecips,
            channelLastMessage = clastmsg
        } → putStrLn $ "ChannelDirectMessage: " <> show (
            cid,
            crecips,
            clastmsg
            )
        ChannelGroupDM {
            channelId = cid,
            channelRecipients = crecips,
            channelLastMessage = clastmsg
        } → putStrLn $ "ChannelDirectMessage: " <> show (
            cid,
            crecips,
            clastmsg
            )
        ChannelGuildCategory {
            channelId = cid,
            channelGuild = cguild
        } → putStrLn $ "ChannelGuildCategory: " <> show (
            cid,
            cguild
            )
        _ → putStrLn "Unsupported channel create message."
    TypingStart TypingInfo {
        typingUserId = uid,
        typingChannelId = cid
        } → putStrLn $ "A user is typing. Details: uid = " <> show uid <> ", cid = " <> show cid
    PresenceUpdate PresenceInfo {
        presenceUserId = uid,
        presenceRoles = rids,
        presenceGuildId = gid,
        presenceStatus = pstat
    } → putStrLn $
        "Received Presence update event. Details:  uid: " <>
        show uid <>
        ", roles: " <>
        show rids <>
        ", gid: " <>
        show gid <>
        ", status: " <>
        show pstat
    MessageReactionAdd _ → putStrLn "Received a reaction event."
    MessageUpdate _ _ -> return () -- seems to happen when we post the embeds
    m → do
        putStrLn "Event detected. Not handled."
        print m

handleQuit ∷ IO ()
handleQuit = putStrLn "Quit handler called"

runDiscordOpts ∷ Env → RunDiscordOpts
runDiscordOpts dEnv = RunDiscordOpts {
    discordToken = envToken dEnv,
    discordOnStart = handleStart dEnv,
    discordOnEnd = handleQuit,
    discordOnEvent = handleEvent dEnv,
    discordOnLog = putStrLn,
    discordForkThreadForEvents = False
}
