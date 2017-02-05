{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid
import           Data.Text             (Text)
import           Data.Time
import           Lib
import           System.Log.FastLogger (LoggerSet, newStdoutLoggerSet,
                                        pushLogStrLn, toLogStr)
import           System.Random         (randomRIO)

-- We're pretending this is a web server
main :: IO ()
main = do
    logSet <- newStdoutLoggerSet 0
    result <- userTasksEndpoint logSet 123
    print result

-- Out pretend endopint: GET user/events
userTasksEndpoint :: LoggerSet -> UserId -> IO (Either Text Task)
userTasksEndpoint logSet userId = do
    pushLogStrLn logSet $ "Running userTasksEndpoint for user "
                       <> toLogStr (show userId)
    UTCTime _ time <- getCurrentTime
    if time > 8 * 3600 && time < 18 * 3600
        then Right <$> getTaskFromDB userId
        else return . Left $ "Time to go home!"


-- Database lookup
getTaskFromDB :: UserId -> IO Task
getTaskFromDB _ = do
    a <- randomRIO (0, length tasks - 1)
    return $ tasks !! a
    where
        tasks = [ "Write some Haskell"
                , "Make great stuff"
                , "Fix write more tests"
                ]
