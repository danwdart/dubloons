{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Discord where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except

import           Control.Monad.IO.Class
import           Data.Map.Strict            as M hiding (null)
import           Data.Text                  as T hiding (concat, head, map,
                                                  null, tail, take, zip)
import           Discord
import           Discord.Requests
import           Discord.Types
-- todo indexify
import           Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
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

handleStart ∷ Env → DiscordHandler ()
handleStart dEnv = do
    h <- ask
    putStrLn ("Start handler called" :: Text)
    _ <- sendMsg "-- Arrr, I be here! --"
    tid <- liftIO myThreadId
    liftIO . void $ installHandler keyboardSignal (
        Catch $ do
            -- if otherwise dead because connection failure, ignore.
            catch (flip runReaderT h $ do
                -- Say goodbye to all channels we're in.
                cache <- readCache
                mapM_ (`sendMessage` "-- Bye, cap'n! --") (M.keys . M.filter (\case
                    ChannelText {} -> True
                    _              -> False
                    ) $ _channels cache)
                ) $ \(SomeException _) -> pure ()
            throwTo tid UserInterrupt
            exitSuccess
        ) Nothing
    where
        sendMsg = sendMessage (envCID dEnv)

sendMessage ∷ ChannelId → MessageText → DiscordHandler MessageResult
sendMessage cid = restCall . CreateMessage cid

sendEmbedRow ∷ ChannelId → Row → DiscordHandler MessageResult
sendEmbedRow cid row = restCall (CreateMessageEmbed cid "" (CreateEmbed {
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

getQuery ∷ Env → ChannelId → Query → DiscordHandler ()
getQuery dEnv cid query = do
    _ <- sendMsg $ "Yarrrr, I be gettin' " <> query <> " for ye!"
    -- async this somehow
    results <- liftIO $ do
        resTPB <- runExceptT $ TPB.queryPirate apiDomain query
        resN <- runExceptT $ N.queryPirate query
        resNP <- runExceptT $ NP.queryPirate query
        pure $ concat =<< [resTPB, resN, resNP]
    let r = zip ([1..] :: [Int]) results
    mapM_ sendMsgRow (take 20 r)
    when (null results) . void . sendMsg $ ("Yarrrr, there be nothin' fer " <> query <> "!")
    where
        apiDomain = envApiDomain dEnv
        sendMsg = sendMessage cid
        sendMsgRow = sendEmbedRow cid

parseMsg ∷ Env →  ChannelId → Query → Command → DiscordHandler ()
parseMsg dEnv cid query = \case
    "get" → getQuery dEnv cid query
    _     → pure ()

handleMessage ∷ Env → Username → ChannelId → MessageText → DiscordHandler ()
handleMessage dEnv un cid = \case
    "/hello" → void . sendMsg $ ("Ahoy, matey, " <> un <> "!")
    "/status" → void $ sendMsg "Yarr, all hands on deck!"
    "/help" → void . sendMsg $ ("Arr, ye can say:\n" <>
        "/hello - I be doin' an echo!\n" <>
        "/status - I tell ye how I be doin'!\n" <>
        "/help - This!\n" <>
        "/quit - I say bye cap'n!")
    "/quit" → do
        _ <- sendMsg "Bye, Cap'n!"
        putStrLn ("Received quit message" :: Text)
        stopDiscord
    msg → do
        -- TODO pattern match had an issue so wat
        putStrLn $ un <> " said: " <> msg <> " in " <> T.pack (show cid)
        let w = T.words msg
        if null w then
            putStrLn ("Empty message?" :: Text)
        else do
            let cmd = head w
            let queries = tail w
            let query = T.unwords queries
            parseMsg dEnv cid query cmd
    where
        sendMsg = sendMessage cid

handleEvent ∷ Env → Event → DiscordHandler ()
handleEvent dEnv = \case
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
            handleMessage dEnv un cid msg
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
        _ → putStrLn ("Unsupported channel create message." :: Text)
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
    MessageReactionAdd _ → putStrLn ("Received a reaction event." :: Text)
    MessageUpdate _ _ -> pure () -- seems to happen when we post the embeds
    m → do
        putStrLn ("Event detected. Not handled." :: Text)
        print m

handleQuit ∷ IO ()
handleQuit = putStrLn ("Quit handler called" :: Text)

runDiscordOpts ∷ Env → RunDiscordOpts
runDiscordOpts dEnv = RunDiscordOpts {
    discordToken = envToken dEnv,
    discordOnStart = handleStart dEnv,
    discordOnEnd = handleQuit,
    discordOnEvent = handleEvent dEnv,
    discordOnLog = putStrLn,
    discordForkThreadForEvents = False
}
