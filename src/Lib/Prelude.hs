{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- TODO lifted-base

module Lib.Prelude where

import           Control.Monad.IO.Class
import qualified Data.IORef             as P
import           Data.Map.Strict
import           Data.String
import           Prelude                (IO, MonadFail, Show, read, show, ($),
                                         (.), (<$>))
import qualified Prelude                as P
import qualified System.Environment     as P
import qualified System.Process         as P

io :: (MonadIO m) => IO a -> m a
io = liftIO

toString :: (IsString s, Show s) => s → String
toString = read . show

getEnv :: (IsString s, Show s, MonadIO m) => s -> m s
getEnv s = io $ fromString <$> P.getEnv (toString s)

fail :: (MonadFail m, MonadIO m, IsString s, Show s) => s -> m a
fail = io . P.fail . toString

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
writeIORef i a = io $ P.writeIORef i a

modifyIORef :: MonadIO m => P.IORef a -> (a -> a) -> m ()
modifyIORef i f = io $ P.modifyIORef i f

zip :: P.Ord a => [a] -> [b] -> Map a b
zip as bs = fromList $ P.zip as bs
