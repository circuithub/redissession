module Web.RedisSession (
    makeRedisConnectionPool,
    makeRedisLocalConnectionPool,
    setSession, setSessionExpiring,
    getSession,
    newKey,
    Redis
  ) where

import Database.Redis
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toChunks)
import Data.Binary (decode, encode, Binary)
import Data.Conduit.Pool
import System.Entropy (getEntropy)
import Data.Digest.Pure.MD5 (md5)
import Data.Time (getCurrentTime)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

----makeRedisConnectionPool :: ConnectInfo -> IO Connection
----makeRedisConnectionPool cinfo = connect cinfo

----makeRedisLocalConnectionPool :: IO (Pool Connection)
----makeRedisLocalConnectionPool = makeRedisConnectionPool localhost defaultPort

--setSession :: (Binary a) => Connection -> ByteString -> a -> IO ()
--setSession conn key value = runRedis conn $ do
--    set key value
--    return ()

--setSessionExpiring :: (Binary a) => Connection -> ByteString -> a -> Int -> IO ()
--setSessionExpiring conn key value timeout = runRedis conn $ do
--    setex key timeout $ encode value
--    return ()

--getSession :: (Binary a) => Connection -> ByteString -> IO (Either Text a)
--getSession conn key = runRedis conn $ do
--    val <- get key
--    case val of
--        Left (Error e) -> Left $ decodeUtf8 e
--        _              -> val

--newKey :: IO ByteString
--newKey = do
--    ent <- getEntropy 80
--    now <- getCurrentTime
--    return $ B.concat [(B.concat . toChunks . encode . md5 . encode . show) now, ent]
