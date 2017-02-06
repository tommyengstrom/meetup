% MTL style stuff
% Tommy Engström
% 2017-02-07

# MTL style stuff
I will talk a bit

------


``` haskell
-- Out pretend endopint: GET user/{user_id}/task
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
```


# But how can we test this?
`userTasksEndpoint` depends on database and time.

* We must have a database containing tasks to write integration tests
* If we write integration tests they will depend on what time you run the test

# Typeclasses to the rescue!
why are we using typeclasses?

# Defining your typeclasses
code

# Writing your custom monad
code

# New main
code

# Defining a pure monad for testing
code

# Writing some tests
code

# Questions?

