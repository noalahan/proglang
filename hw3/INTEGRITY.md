I incorporated elements from the following sources into my assignment submission.

* Used LYAH ch 12 to understand Maybe types before answering 1(b)
* Used chatgpt to understand the following error message for problem 2(a)

prompt: 
    
    error:
 
    Variable not in scope:
      printf :: String -> String -> String -> String
    Suggested fix: Perhaps use ‘print’ (imported from Prelude)
        |

    179 |   show (TimesVE e1 e2) = printf "(%s * %s)" (show e1) (show e2)

        |                          ^^^^^^

response:
    
    This error is because printf is not in scope in your Haskell code. The printf function is not part of the standard Prelude; it comes from the Text.Printf module.

    To fix the error, import the correct module at the top of your file:

    import Text.Printf (printf)

    So your file should look something like:

    import Text.Printf (printf)

    instance Show Expr where
      show (TimesVE e1 e2) = printf "(%s * %s)" (show e1) (show e2)

    Alternatively, if you don't want to import Text.Printf, you could use string concatenation instead:

    show (TimesVE e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

    Both approaches work — printf is cleaner for formatting if you already have it imported.
