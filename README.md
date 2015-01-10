SGF(Smart Game Format) Parser for haskell

Example(sample.hs):

```sample.hs
import System.Environment (getArgs)
import SgfParser
import SgfUtil

main = do input_str <- fmap head getArgs
          case parseSgf igo_parser input_str of
            Left  err -> putStrLn $ show err
            Right val -> putStrLn $ show val
```


```
# ./sample " (  ; B[aa] ;W[ab] )"

B[aa]
W[ab]

```

```
# ./sample "(;B[aa];W[ab](;W[ac])(;W[ad];B[ak](;B[af])(;B[aj]))(;W[ae]))"

B[aa]
W[ab]-------+-------------------+
W[ac]     W[ad]               W[ae]
          B[ak]-------+
          B[af]     B[aj]
```

```
# ./sample " (;B[aa]"
(line 1, column 5):
unexpected end of input
```

```
# ./sample " (;B[aa])(:)"
(line 1, column 11):
unexpected ':'
expecting "(", ")", ";", "[", uppercase letter or end of input
```

```
# ./sample " (;B[aa];W[aaa];WW[aa])"

B[aa]
W[*ParseError*]
WW[*UnknownProp*]
```

