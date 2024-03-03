errorString :: InterpreterError -> String
errorString (WontCompile es) = DL.intercalate "\n" (header : map unbox es)