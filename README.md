# flock-parenthases-monad

In this library, a `Parens` `Monad` is defined and used with
[`Attoparsec`](http://hackage.haskell.org/package/attoparsec)
to experiment with parsing a subset of Flock.


# Parenthases data type

```haskell
newtype Parens a = Parens { getParens :: Either a [Parens a] } deriving (Eq, Ord)
```

- `Monad` instance

```haskell
instance Monad Parens where
  return = pure

  Parens (Left x) >>= f = f x
  Parens (Right xs) >>= f = Parens . Right $ fmap (>>= f) xs
```


- Isomorphism: `Parens a <-> Tree (Maybe a)`
- Combinators
- Parser using [`Attoparsec`](http://hackage.haskell.org/package/attoparsec): `parseParens :: Parser a -> Parser (Parens a)`


## Note:

`Parens a` is isomorphic to `Free [] a`,
where [`Free`](http://hackage.haskell.org/package/free-5.1/docs/Control-Monad-Free.html#t:Free)
is the free `Monad` for a given `Functor`.


# Parsing Flock

`Parens` is used to implement parsing and easier evaluation for a small prototype core of Flock:

```haskell
data Expr n a where
  (:#)  :: a -> n -> Expr n a
  (:&)  :: Expr n a -> Expr n a -> Expr n a
  (:|)  :: Expr n a -> Expr n a -> Expr n a
  (:->) :: Expr n a -> Expr n a -> Expr n a
  deriving (Eq, Ord, Show)

data Binop a = BinL a a | BinR a a | BinM a a a | BinO a deriving (Eq, Ord, Show)

examples :: IsString a => [a]
examples =
  [ "(a :# 1)"
  , "a :# 1"
  , "((a :# 1), (b :# 2))"
  , "((a :# 1) | (b :# 2))"
  , "(a :# 1) -> (b :# 2)"
  , "(a :# 1) -> a"
  , "(a :# 1, b :# 2) -> b"
  , "(a :# 1 | b :# 2) -> (a | b)"
  , "a :# 1 -> b :# 2 -> a"
  , "((a :# 2) -> (b :# 3)) -> (a , b) -> ((a -> b) | b)"
  , "(((z :# 0), ((a :# 1), (b :# 2))) | (a -> b, b -> a))"
  -- < a trick, ((z :# 0, constraints) | expr) === constraints => expr
  -- (('z' :# 0) :& (('a' :# 1) :& ('b' :# 2)))
  --   :| (('a' :# 1) :-> ('b' :# 2) :& ('b' :# 2) :-> ('a' :# 1))
  ]

-- A verbose proof that there is no bijection between ('a' :# 1) and ('b' :# 2):
-- λ> calcExpr $
--   (('z' :# 0) :& (('a' :# 1) :& ('b' :# 2)))
--     :| (('a' :# 1) :-> ('b' :# 2) :& ('b' :# 2) :-> ('a' :# 1))
-- 0

parseExamples ::
     Integral a
  => [Either String (Parens (Either (Expr a T.Text) (Either (Binop T.Text) ())))]
parseExamples =
  parseOnly
    (parseParens
       (eitherP
          (liftM2 (:#) word (skipSpace >> string ":#" >> skipSpace >> decimal))
          (eitherP binOp skipSpace1))) <$>
  examples

parseExamples' ::
     Integral a
  => Either String [Parens (Either (Expr a T.Text) (Either (Binop T.Text) ()))]
parseExamples' =
  mapM
    (parseOnly
       (parseParens
          (eitherP
             (liftM2
                (:#)
                word
                (skipSpace >> string ":#" >> skipSpace >> decimal))
             (eitherP binOp skipSpace1))))
    examples

parseExamples2 ::
     Integral a
  => Either String [Parens (Either (Expr a T.Text) (Binop T.Text))]
parseExamples2 =
  mapM
    (parseOnly
       (parseParens
          (eitherP
             (liftM2
                (:#)
                word
                (skipSpace >> string ":#" >> skipSpace >> decimal))
             binOp)))
    examples
```

