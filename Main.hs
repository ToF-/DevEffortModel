import DevEffort

loop :: DevSystem -> IO ()
loop ds = do
    putStr $ unlines $ pretty ds
    putStrLn prompt
    cmd <- fmap parse getLine
    case cmd of
        Nothing   -> putStrLn "unknown command" >> loop ds
        Just Quit -> return ()
        Just c    -> loop (execute c ds) 

main = loop initial
