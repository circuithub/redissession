{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Yesod.Session.Redis (
    --localRedisSessionBackend,
    --redisSessionBackend
    loadRedisSession
  ) where

import qualified Database.Redis as R
--import qualified Web.RedisSession as R
import Yesod.Core
import Yesod.Core.Types
import qualified Network.Wai as W
import Web.Cookie
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, addUTCTime)
import Data.Conduit.Pool (Pool)
import Data.Binary
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad (liftM)
import Control.Exception
import Data.Typeable
import Data.Time (getCurrentTime)
import Data.Map (fromList, toList)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toChunks)
import Data.Digest.Pure.MD5 (md5)
import System.Entropy (getEntropy)


--instance Binary Text where 
--    put = put . encodeUtf8 
--    get = liftM decodeUtf8 get

data RedisSessionException = RedisSessionNotFound Text | RedisSessionCookieSessionNotFound
    deriving (Show, Typeable)
instance Exception RedisSessionException

loadRedisSession :: R.Connection 
                 -> ByteString    -- ^ Redis session key
                 -> ByteString    -- ^ session name
                 -> Integer       -- ^ time out
                 -> W.Request 
                 -> IO (SessionMap, SaveSession)
loadRedisSession conn redisKey sessionName timeout req = do
    let cookieVal = do
          raw <- lookup "Cookie" $ W.requestHeaders req
          lookup sessionName $ parseCookies raw
    expires <- fmap addTimeout getCurrentTime
    case cookieVal of
        Nothing -> return $ (M.empty, saveRedisSession expires)
        Just s -> R.runRedis conn $ do
            redisHash <- R.hgetall s
            case redisHash of
                Left (R.Error e) -> throw $ RedisSessionNotFound $ decodeUtf8 e
                Right hash       -> return (fromList $ mapFst decodeUtf8 hash, saveRedisSession expires)
    where
        saveRedisSession :: UTCTime -> SaveSession
        saveRedisSession expires sessionMap = do
            --R.setex conn key sess timeout
            R.runRedis conn $ R.hmset redisKey $ mapFst encodeUtf8 $ toList sessionMap
            return [AddCookie def 
                { setCookieName = sessionName
                , setCookieValue = redisKey
                , setCookiePath = Just "/"
                , setCookieExpires = Just expires
                , setCookieDomain = Nothing
                , setCookieHttpOnly = True
                }]
        mapFst f       = map $ \(a,b) -> (f a, b)
        addTimeout now = fromIntegral (timeout * 60) `addUTCTime` now

--newKey :: UTCTime -> IO ByteString
--newKey now = do
--    ent <- getEntropy 80
--    return $ B.concat [(B.concat . toChunks . encode . md5 . encode . show) now, ent]

        

--fmap (fromMaybe []) $ liftIO $ R.getSession conn s

--saveRedisSession :: (Yesod master) => R.Connection -> Int -> master -> W.Request -> UTCTime -> BackendSession -> BackendSession -> IO [Header]
--saveRedisSession conn timeout master req now _ sess = do
--  let val = do
--      raw <- lookup "Cookie" $ W.requestHeaders req
--      lookup sessionName $ parseCookies raw
--  key <- case val of
--      Nothing -> R.newKey
--      Just k -> return k
--  R.setex conn key sess timeout
--  return [AddCookie def {
--          setCookieName = sessionName,
--          setCookieValue = key,
--          setCookiePath = Just $ cookiePath master,
--          setCookieExpires = Just expires,
--          setCookieDomain = cookieDomain master,
--          setCookieHttpOnly = True
--      }]
--  where
--      expires = fromIntegral (timeout * 60) `addUTCTime` now


--localRedisSessionBackend :: (Yesod master) => Int -> IO (SessionBackend master)
--localRedisSessionBackend = sessionBackend R.makeRedisLocalConnectionPool

--redisSessionBackend :: (Yesod master) => String -> String -> Int -> IO (SessionBackend master)
--redisSessionBackend server port = sessionBackend (R.makeRedisConnectionPool server port)

--sessionBackend :: (Yesod master) => R.Connection -> Int -> IO (SessionBackend master)
--sessionBackend conn timeout = return $ SessionBackend {
--        sbLoadSession = loadRedisSession conn
--    }
