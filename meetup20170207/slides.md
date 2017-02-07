
![Henry](wordmark.svg)

## Writing testable code

------


``` Haskell
-- Out pretend endopint: GET user/events
userTasksEndpoint :: UserId -> IO Endpoint
userTasksEndpoint userId = do
    workTime <- isWorkTime
    if workTime
        then Right <$> getTaskFromDB userId
        else return . Left $ "Go home!"

isWorkTime :: IO Bool
isWorkTime = do
    time <- utctDayTime <$> getCurrentTime
    return $ time > 8 * 3600
          && time < 18 * 3600
```


### But how can we test this?

### Typeclasses to the rescue!

### Defining your typeclasses

``` Haskell
class Monad m => MonadTime m where
    getTime :: m UTCTime

class Monad m => MonadDB m where
    getTask :: UserId -> m Task

```

### Define our custom Monad

``` Haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype MyMonad a = MyMonad (IO a)
    deriving ( Functor, Applicative, Monad
             , MonadIO)
```

### Write instances

``` Haskell
instance MonadTime MyMonad where
    getTime = liftIO getCurrentTime

instance MonadDB MyMonad where
    getTask _ = return "Write some Haskell"

runMyMonad :: MyMonad a -> IO a
runMyMonad (MyMonad m) = m

```

### Function to check time

``` Haskell
isWorkTime :: MonadTime m => m Bool
isWorkTime = do
    time <- utctDayTime <$> getTime
    return $ time > 8 * 3600
          && time < 18 * 3600
```

### New taskEndpoint

``` Haskell
-- Out pretend endopint: GET user/tasks
taskEndpoint :: (MonadTime m, MonadDB m)
             => UserId -> m Endpoint
taskEndpoint userId = do
    workTime <- isWorkTime
    if workTime
        then Right <$> getTask userId
        else return . Left $ "Go home!"
```

### Defining a pure monad for testing
``` Haskell
newtype TestMonad a =
    TestMonad (Reader UTCTime a)
    deriving ( Functor, Applicative
             , Monad
             , MonadReader UTCTime)

````

### Write instances for TestMonad

``` Haskell
instance MonadTime TestMonad where
    getTime = ask

instance MonadDB TestMonad where
    getTask _ = return "Test task"

runTest :: UTCTime -> TestMonad a -> a
runTest time (TestMonad m) =
    runReader m time

```

### Test our implementation
``` Haskell
describe "Test task endpoint" $ do
    it "Gives new task at 9:00" $
        runTest (atHour 9)
                (taskEndpoint userId)
            `shouldSatisfy` not . null
    it "Gives no new task at 19:00" $
        runTest (atHour 19)
                (taskEndpoint userId)
            `shouldSatisfy` null
```

