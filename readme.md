SGF(Smart Game Format) Parser for haskell  **[Under development]**

Example:

```sample.hs
import SgfParser
import SgfUtil

main = let input_str = "( ;B[cc]  ;W[aa] (;W[aa] ;B[bb])(;W[bb])(;W[ab])))"
       in case parseSgf igo_parser input_str of
            Left  err -> putStrLn $ show err
            Right val -> putStrLn $ show val
```

```
# ./sample
B[cc]
W[aa]-------+---------+
W[aa]     W[bb]     W[ab]
B[bb]
```


