{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module Lib.Prelude where

import qualified Data.IORef as P
import Control.Monad.IO.Class
import Data.String
import qualified Prelude as P
import Prelude (
    (.),
    IO,
    read,
    show,
    Show
    )
import qualified System.Process as P

io :: (MonadIO m) => IO a -> m a
io = liftIO

toString :: (IsString s, Show s) => s → String
toString = read . show

putStrLn :: (MonadIO m, IsString s, Show s) => s → m ()
putStrLn = io . P.putStrLn . toString

print :: (MonadIO m, Show s) => s → m ()
print = io . P.print

spawnCommand :: (MonadIO m, IsString s, Show s) => s -> m P.ProcessHandle
spawnCommand = io . P.spawnCommand . toString

newIORef :: MonadIO m => a -> m (P.IORef a)
newIORef = io . P.newIORef

readIORef :: MonadIO m => P.IORef a -> m a
readIORef = io . P.readIORef

writeIORef :: MonadIO m => P.IORef a -> a -> m ()
writeIORef i a = io (P.writeIORef i a)