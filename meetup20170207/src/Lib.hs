{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Lib where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Text              (Text)
import           Data.Time
import           System.Log.FastLogger  (LoggerSet, ToLogStr,
                                         newStdoutLoggerSet, pushLogStrLn,
                                         toLogStr)
import           System.Random          (randomRIO)

type UserId = Int
type Task = Text

class Monad m => MonadTime m where
    getTime :: m UTCTime

class Monad m => MonadLog m where
    logInfo :: ToLogStr t => t -> m ()

class Monad m => MonadDB m where
    getTask :: UserId -> m Task

-- The new monad we will be running our program in
newtype MyMonad a = MyMonad (ReaderT LoggerSet IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader LoggerSet)

instance MonadTime MyMonad where
    getTime = liftIO getCurrentTime

instance MonadLog MyMonad where
    logInfo t = ask >>= liftIO . flip pushLogStrLn (toLogStr t)

instance MonadDB MyMonad where
    getTask _ = do
        i <- liftIO $ randomRIO (0, length tasks - 1)
        return $ tasks !! i
        where
            tasks = [ "Write some Haskell"
                    , "Make great stuff"
                    , "Fix write more tests"
                    ]

runMyMonad :: MyMonad a -> IO a
runMyMonad (MyMonad m) = newStdoutLoggerSet 0 >>= runReaderT m

-- Out pretend endopint: GET user/events
taskEndpoint :: (MonadLog m, MonadTime m, MonadDB m) => UserId -> m (Either Text Task)
taskEndpoint userId = do
    logInfo $ "Running taskEndpoint for user " <> show userId
    time <- utctDayTime <$> getTime
    if time > 8 * 3600 && time < 18 * 3600
        then Right <$> getTask userId
        else return . Left $ "Time to go home!"
