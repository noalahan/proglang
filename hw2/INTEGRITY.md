I incorporated elements from the following sources into my assignment submission.

* Used ChatGPT to aid in an error
    - prompt: 
    > src/TailRecursion.hs:28:17: error: Parse error in pattern: k      
    28 | assoc def key ((k v):xs) | k == key   = v
                
    - response: 

    > This error is caused by an incorrect pattern match in your function definition. In Haskell, when you're trying to pattern match a pair inside a list, you need parentheses around the pair. Let's look at the problematic line:       
    `assoc def key ((k v):xs) | k == key   = v`     
    You're trying to match a list of key-value pairs (tuples), but ((k v):xs) is not valid Haskell syntax for a tuple. Instead, you should use a comma to indicate a tuple:     
    `assoc def key ((k, v):xs) | k == key = v`


