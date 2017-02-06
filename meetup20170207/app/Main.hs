{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text             (Text)
import           Data.Time
import           Lib

-- We're pretending this is a web server
main :: IO ()
main = do
    result <- userTasksEndpoint 123
    print result

-- Out pretend endopint: GET user/events
userTasksEndpoint :: UserId -> IO (Either Text Task)
userTasksEndpoint userId = do
    workTime <- isWorkTime
    if workTime
        then Right <$> getTaskFromDB userId
        else return . Left $ "Time to go home!"

isWorkTime :: IO Bool
isWorkTime = do
    time <- utctDayTime <$> getCurrentTime
    return $ time > 8 * 3600 && time < 18 * 3600

-- Pretend this function talks to a database
getTaskFromDB :: UserId -> IO Task
getTaskFromDB _ = return  "Write some Haskell"
