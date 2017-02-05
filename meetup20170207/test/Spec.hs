{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Monad.Reader
import           Data.Time
import           Lib
import           Test.Hspec

newtype TestMonad a = TestMonad (Reader UTCTime a)
    deriving (Functor, Applicative, Monad, MonadReader UTCTime)

instance MonadTime TestMonad where
    getTime = ask

instance MonadLog TestMonad where
    logInfo _ = return ()

instance MonadDB TestMonad where
    getTask _ = return "Test task"

runTestMonad :: UTCTime -> TestMonad a -> a
runTestMonad time (TestMonad m) = runReader m time

userId :: UserId
userId = 1

main :: IO ()
main = hspec $
    describe "Test task endpoint" $ do
        it "Gives new task at 9:00" $
            runTestMonad (atHour 9) (taskEndpoint userId) `shouldSatisfy`
                not . null
        it "Gives no new task at 19:00" $
            runTestMonad (atHour 19) (taskEndpoint userId) `shouldSatisfy`
                null
    where
        atHour h = UTCTime (fromGregorian 2017 2 7) (h * 3600)
