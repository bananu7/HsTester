module Utils where

askForInt :: IO Int
askForInt = do
    line <- getLine

    case reads line of
        [(i, "")] -> return i
        _ -> do
            putStrLn "Enter a valid number!"
            askForInt

askForIntR :: (Int, Int) -> IO Int
askForIntR (min,max) = do
    line <- getLine

    case reads line of
        [(i, "")] -> if i >= min && i <= max
            then return i
            else do
                putStrLn $ "A number must be in range (" ++ show min ++ ", " ++ show max ++ ")!"
                retry
        
        _ -> do
            putStrLn "Enter a valid number!"
            retry
     where retry = askForIntR (min,max)
