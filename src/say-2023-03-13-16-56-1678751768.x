say::String -> Interpreter()
say = liftIO . putStrLn