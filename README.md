# bfkhs
simple [Brainf**k](https://ja.wikipedia.org/wiki/Brainfuck) interpreter written by Haskell.

# usage
you have to install [stack](https://docs.haskellstack.org/en/stable/README/) prior.

```
$ git clone https://github.com/matchaBread/bfkhs.git
$ stack build
$ stack install
```

```
hbfk: Brainf**k interpreter written by Haskell

Usage: hbfk ((-f|--file FILEPATH) | (-i|--inline PROGRAM))
  interpret Brainf**k source code file or inline source code

Available options:
  -f,--file FILEPATH       Brainf**k source file path
  -i,--inline PROGRAM      (inline) Brainf**k program
  -h,--help                Show this help text
```
