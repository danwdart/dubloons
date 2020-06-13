{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Discord where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           Data.IORef
import qualified Data.Text                  as T
import           Discord
import           Discord.Requests
import           Discord.Types
import           Lib.Pirate
import           Lib.Prelude
import           Lib.Types
import           Lib.Util
import           Prelude                    hiding (print, putStrLn)
import           System.Process

handleStart ∷ Env → DiscordHandle → IO ()
handleStart dEnv h = do
    putStrLn "Start handler called"
    void $ sendMessage dEnv h "-- Arrr, I be here! --"

sendMessage ∷ Env → DiscordHandle → MessageText → IO MessageResult
sendMessage dEnv h = restCall h . CreateMessage (envCID dEnv)

getQuery ∷ Env → DiscordHandle → Query → IO ()
getQuery dEnv h query = do
    _ <- sendMsg $ "Yarrrr, I be gettin' " <> query <> " for ye!"
    res <- runExceptT $ queryPirate apiDomain query
    case res of
        Left a → void $ sendMsg $ T.pack a
        Right results → do
            _ <- sendMsg $ "Yarrrr, I got ye " <> query <> " for ye!"
            _ <- liftIO $ writeIORef ir (indexList results)
            _ <- sendMsg $ "Yarrrr, I stored ye " <> query <> " for ye! Here they be:"
            mapM_ sendQueries $
                        take 10 (indexList results)
            void . sendMsg $ "Yarr, that be it! Ye can be pickin'! Ye says fer example 'dl 2' fer gettin' ye yer second pick! Arr!"
    where
        ir = envStateM dEnv
        apiDomain = envApiDomain dEnv
        sendMsg = sendMessage dEnv h
        sendQueries (ix, Row {
            name = rowName,
            leechers = rowLeechers,
            seeders = rowSeeders,
            imdb = rowIMDB
        }) = void $ sendMsg . T.pack $
            show ix <>
            ": " <>
            T.unpack rowName <>
            " (" <>
            show rowSeeders <>
            " seeders, " <>
            show rowLeechers <>
            " leechers)." <>
            maybe mempty (T.unpack . (" https://imdb.com/title/" <>)) rowIMDB

parseMsg ∷ Env →  DiscordHandle → Query → Command → IO ()
parseMsg dEnv h query = \case
    "cache" → print =<< readCache h
    "get" → getQuery dEnv h query
    "results" → do
        let ir = envStateM dEnv
        v <- liftIO . readIORef $ ir
        void $ sendMsg (T.pack $ show (map (second show) v))
    "dl" → do
        let ir = envStateM dEnv
        v <- liftIO . readIORef $ ir
        let result = lookup (read . T.unpack $ query) v
        -- void . sendMsg . T.pack $ show $ result) v
        maybe (void $ sendMsg "Yarr, that weren't existin'!") sendSpawned result
    _ → return ()
    where
        torrentClient = envTorrentClient dEnv
        sendMsg = sendMessage dEnv h
        sendSpawned r = do
            let magnetLink = magnetPrefix <> info_hash r <> magnetSuffix
            _ <- sendMsg "Yarr, I be spawnin' yer download!"
            _ <- liftIO . spawnCommand . T.unpack $ -- TODO nohup this
                torrentClient <>
                " -- '" <>
                magnetLink <>
                "'"
            void $ sendMsg "Yarr, I spawned yer download!"

handleMessage ∷ Env → Username → DiscordHandle → MessageText → IO ()
handleMessage dEnv un h = \case
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
        let (cmd : queries) = T.words msg
        let query = T.unwords queries
        parseMsg dEnv h query cmd
    where
        sendMsg = sendMessage dEnv h

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
        handleMessage dEnv un h msg
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
