module Main where

import           Lib

-- We're pretending this is a web server
main :: IO ()
main = do
    result <- runMyMonad $ taskEndpoint 123
    print result
