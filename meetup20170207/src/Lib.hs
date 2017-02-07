{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Lib
    ( MyMonad
    , runMyMonad
    , MonadTime (..)
    , MonadDB (..)
    , taskEndpoint
    , UserId
    , Task
    ) where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Data.Time

type Endpoint = Either Text Task
type UserId = Int
type Task = Text

class Monad m => MonadTime m where
    getTime :: m UTCTime

class Monad m => MonadDB m where
    getTask :: UserId -> m Task

-- The new monad we will be running our program in
newtype MyMonad a = MyMonad (IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTime MyMonad where
    getTime = liftIO getCurrentTime

instance MonadDB MyMonad where
    getTask _ = return "Write some Haskell"

runMyMonad :: MyMonad a -> IO a
runMyMonad (MyMonad m) = m

-- Out pretend endopint: GET user/events
taskEndpoint :: (MonadTime m, MonadDB m) => UserId -> m Endpoint
taskEndpoint userId = do
    workTime <- isWorkTime
    if workTime
        then Right <$> getTask userId
        else return . Left $ "Time to go home!"

isWorkTime :: MonadTime m => m Bool
isWorkTime = do
    time <- utctDayTime <$> getTime
    return $ time > 8 * 3600
          && time < 18 * 3600