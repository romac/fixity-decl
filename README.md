
> Just an experiment in handling Haskell-style user-defined fixity declarations with Megaparsec.

`test.fd`
```haskell
prefix -
postfix ++
infixl 6 +
infix ==
infixr 2 :
infixl !!

1+2 : 2+3 : -4 + 5++ : -6 : 123
```

    $ stack exec fixity-decl -- test.fd
    ((1 + 2) : ((2 + 3) : (((- 4) + (++ 5)) : ((- 6) : 123))))

