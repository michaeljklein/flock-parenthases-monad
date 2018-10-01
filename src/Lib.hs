{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveTraversable #-}


module Lib where

import Data.Maybe
-- import GHC.TypeLits
-- import Data.Proxy
-- import Data.Typeable

-- import Data.HashMap.Strict (fromListWith, mapMaybe, toList)
import Data.List (nub)

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import Data.Char
import Control.Applicative
import qualified Data.Text as T
-- import Data.Either
-- import Data.Function (on)

import Data.Tree
import Control.Monad
import Data.Bifunctor

import Data.String (IsString(..))
import Data.List

(<<) :: Monad m => m b -> m a -> m b
(<<) x y = do
  x' <- x
  _  <- y
  return x'

-- newtype Parens a = Parens { getParens :: Forest a } deriving (Eq, Show, Functor)

fromParens :: Parens a -> Tree (Maybe a)
fromParens (Parens (Left x)) = Node (Just x) []
fromParens (Parens (Right xs)) = Node (Nothing) (map fromParens xs)

toParens :: Tree (Maybe a) -> Parens a
toParens (Node Nothing xs) = Parens (Right (map toParens xs))
toParens (Node (Just x) []) = Parens (Left x)
toParens (Node (Just x) xs) = Parens (Right (Parens (Left x) : map (Parens . Right . (:[]) . toParens) xs))


-- λ> putStrLn . drawTree . fmap show . fromParens . fromRight undefined $ parseOnly (parseParens notParens) "(a :# 2 -> (a -> (b :# 1))(hi)"
-- Nothing
-- |
-- `- Nothing
--    |
--    +- Just "a :# 2 -> "
--    |
--    +- Nothing
--    |  |
--    |  `- Just "a -> "
--    |
--    +- Nothing
--    |  |
--    |  `- Nothing
--    |     |
--    |     `- Just "b :# 1"
--    |
--    `- Nothing
--       |
--       `- Just "hi"

-- *Main Data.Attoparsec.Text Data.Either Data.Tree
-- λ> toParens . fromParens . fromRight undefined $ parseOnly (parseParens notParens) "(a :# 2 -> (a -> (b :# 1))(hi)"
-- (
--   (
--     "a :# 2 -> "
--
--     (
--       "a -> "
--
--     )
--
--     (
--       (
--         "b :# 1"
--
--       )
--
--     )
--
--     (
--       "hi"
--
--     )
--
--   )
--
-- )


newtype Parens a = Parens { getParens :: Either a [Parens a] } deriving (Eq, Ord)

instance Show a => Show (Parens a) where
  show (Parens (Left  x)) = show x
  show (Parens (Right x)) = unlines $ "(" : map (unlines . map ("  "++) . lines . show) x ++ [")"]


instance Functor Parens where
  fmap f (Parens x) = Parens (either (Left . f) (Right . fmap (fmap f)) x)


instance Applicative Parens where
  pure = Parens . Left

  Parens (Left f) <*> Parens (Left x) = Parens (Left (f x))
  Parens (Left f) <*> Parens (Right xs) = Parens (Right (fmap f <$> xs))
  Parens (Right fs) <*> Parens (Left x) = Parens (Right (fmap ($ x) <$> fs))
  Parens (Right fs) <*> Parens (Right xs) = Parens (Right (liftM2 (<*>) fs xs))


instance Monad Parens where
  return = pure

  Parens (Left x) >>= f = f x
  Parens (Right xs) >>= f = Parens . Right $ fmap (>>= f) xs


instance Foldable Parens where
  foldr :: (a -> b -> b) -> b -> Parens a -> b
  foldr f x (Parens (Left y)) = f y x
  foldr f x (Parens (Right ys)) = foldr (flip (foldr f)) x ys


instance Traversable Parens where
  traverse :: Applicative f => (a -> f b) -> Parens a -> f (Parens b)
  traverse f (Parens (Left y)) = Parens . Left <$> f y
  traverse f (Parens (Right ys)) = Parens . Right <$> traverse (traverse f) ys


joinParens1 :: [Parens a] -> Parens a
joinParens1 = Parens . Right . joinParens

joinParens :: [Parens a] -> [Parens a]
joinParens [] = []
joinParens [x] = [x]
joinParens (x:y:zs) = maybe (x : joinParens (y : zs)) (joinParens . (: zs)) (maybeJoinParens x y)

maybeJoinParens :: Parens a -> Parens a -> Maybe (Parens a)
maybeJoinParens (Parens (Left _)) _ = Nothing
maybeJoinParens _ (Parens (Left _)) = Nothing
maybeJoinParens (Parens (Right x)) (Parens (Right y)) = Just (Parens (Right (x ++ y)))

-- | Recursively simplifies: @Parens (Right ([Parens (Right xs)])) -> Parens (Right xs)@
flattenParens :: Parens a -> Parens a
flattenParens (Parens (Right ([Parens (Right xs)]))) = flattenParens (Parens (Right xs))
flattenParens (Parens (Right xs)) = Parens (Right (map flattenParens xs))
flattenParens  x                  = x


-- If all Left, collect into all left

-- allLeftParens :: Parens (Either a b) -> Either (Parens a) (Parens (Either a b))
-- allLeftParens   (Parens (Left (Left x))) = Left (Parens (Left x))
-- allLeftParens x@(Parens (Left (_     ))) = Right x
-- allLeftParens (Parens (Right

-- allLeftParensL :: [Parens (Either a b)] -> Either [Parens a] [Parens (Either a b)]
-- allLeftParensL =

-- allLeft

swapEither :: Either a b -> Either b a
swapEither = either Right Left

allLeft :: Traversable t => t (Either a b) -> Either b (t a)
allLeft = mapM swapEither


-- | `Parens`, nested @N@ deep
--
-- @
-- λ> parensN 0 ()
-- Parens {getParens = Left ()}
--
-- λ> parensN 1 ()
-- Parens {getParens = Right [Parens {getParens = Left ()}]}
--
-- λ> parensN 2 ()
-- Parens {getParens = Right [Parens {getParens = Right [Parens {getParens = Left ()}]}]}
-- @
--
parensN :: Int -> a -> Parens a
parensN 0 = Parens . Left
parensN n = Parens . Right . (:[]) . parensN (n-1)



-- f-it, lets just do a a map-reduce parser:
--                    - split into (), other
--                    - zip with +/- 1, for (/)
--                    - map Parens, nested i deep
--                    - fold Parens into single Parens

-- | One-liner lexer/pre-parser
preParens :: Parser a -> Parser [Either Char a]
preParens p = many1 ((Left <$> (char '(' <|> char ')')) <|> (Right <$> notParens >- p))

notParens :: Parser T.Text
notParens = takeWhile1 (\x->x/='('&&x/=')')

-- | Note: This skips @()@:
--
-- @
-- λ> parseOnly (parseParens) "()"
-- Right (Parens {getParens = Right []})
--
-- λ> parseOnly (parseParens) "()()"
-- Right (Parens {getParens = Right []})
-- @
--
depthParens :: [Either Char a] -> [(Int, a)]
depthParens (Left '(':xs) = first (\x -> x + 1) <$> depthParens xs
depthParens (Left ')':xs) = first (\x -> x - 1) <$> depthParens xs
depthParens (Left  x :_ ) = error $ "depthParens: " ++ show x ++ " is not either '(' or ')'"
depthParens (Right x :xs) = (0, x) : depthParens xs
depthParens  _            = []


parseParens :: Parser a -> Parser (Parens a)
parseParens p = flattenParens . joinParens1 . map (uncurry parensN) . depthParens <$> preParens p


-- [Either Char a] -> ..

-- (Left '(':xs) =
-- (Left ')':xs)

-- rawParens :: Parser a -> Parser a
-- rawParens p = do
--   _  <- char '('
--   p' <- rawParens0 p
--   _  <- char ')'
--   return p'

-- rawParens0 :: Parser a -> Parser a
-- rawParens0 p = takeWhile1 (\x->x/='('&&x/=')') >- p



-- | Parses a selection of `T.Text`, then attempts to `parseOnly` or throws `fail` with an error message
(>-) :: Parser T.Text -> Parser a -> Parser a
p >- q = (p <?> "(>-) p") >>= either (fail . ("(>-) q: "++)) return . parseOnly q


-- parens :: Parser a -> Parser (Parens a)
-- parens p = (skipSpace1 >> parens p) <|> (return <$> rawParens0 p) <|> (rawParens (parens p)) <|> (Parens . Right <$> many1 (parens p))


-- parens :: Parser a -> Parser (Parens a)
-- parens p = foldr1 (<|>) [ skipSpace1 >> parens p
--                         , endOfInput >> return (Parens [])
--                         , satisfy (/= '(') >> fail ".."
--                         , Parens <$> many1 (parens1 p)
--                         ]


-- parens1 :: Parser a -> Parser (Tree a)
-- parens1 p = do
--   _ <- char '('
--   skipSpace
--   c <- peekChar'
--   if c == '('
--      then do
--        p' <- parens1 p
--        skipSpace
--        _  <- char ')'
--        return p'
--      else do
--        p' <- p
--        ps <- getParens <$> parens p
--        skipSpace
--        _  <- char ')'
--        return $ Node p' ps


skipSpace1 :: Parser ()
skipSpace1 = skipMany1 space


-- data Expr (n :: Nat) (a :: Symbol) where
--   Lit :: (KnownNat n, KnownSymbol a) => Proxy n -> Proxy a -> Expr n a
--   And :: (KnownNat n, KnownSymbol a, KnownNat m, KnownSymbol b) => Expr n a -> Expr m b -> Expr (n * m) (AppendSymbol "(" (AppendSymbol a (AppendSymbol " , " (AppendSymbol b ")"))))
--   Or  :: (KnownNat n, KnownSymbol a, KnownNat m, KnownSymbol b) => Expr n a -> Expr m b -> Expr (n + m) (AppendSymbol "(" (AppendSymbol a (AppendSymbol " | " (AppendSymbol b ")"))))
--   To  :: (KnownNat n, KnownSymbol a, KnownNat m, KnownSymbol b) => Expr n a -> Expr m b -> Expr (Choose n m) (AppendSymbol "(" (AppendSymbol a (AppendSymbol " -> " (AppendSymbol b ")"))))


-- (a :# n)
-- (a , b)
-- (a | b)
-- (a -> b)

-- exprN :: KnownNat n => Expr n a -> SomeNat
-- exprN x = SomeNat ((const Proxy :: Expr n' a' -> Proxy n') x)

-- declass :: (KnownNat n, Typeable a) => Expr n a -> [(SomeNat, TypeRep)]
-- declass x@((:#) _ y) = [(exprN x, typeRep (return y `asTypeOf` Proxy))]
-- declass   ((:->) n m y) = [(SomeNat n, typeRep ((const Proxy :: (s -> t) -> Proxy s) y)), (SomeNat m, typeRep ((const Proxy :: (s -> t) -> Proxy t) y))]
-- declass (And x y) = declass x ++ declass y
-- declass (Or  x y) = declass x ++ declass y


-- -- | Check that all (:#)'s and (:->)'s agree on their numbers of inhabitants.
-- --
-- -- If any replicates are found (types with two or more unique inhabitance numbers), `Left`, else `Right`.
-- coherent :: (Typeable a, KnownNat n) => Expr n a -> Either [(TypeRep, [SomeNat])] (Expr n a)
-- coherent expr | null replicates = Right expr
--               | otherwise       = Left  replicates
--   where
--     replicates :: [(TypeRep, [SomeNat])]
--     replicates = toList . mapMaybe (\x -> let y = nub x in case y of {(_:_)->Nothing;_->Just y}) $ fromListWith (++) [ (k, [v]) | (v, k) <- declass expr ]

-- -- | Derive any missing inhabitance annotations, adding them to the tree
-- derive :: Expr n a -> Expr n a
-- derive = error "unimplemented"

-- data Expr (n :: Nat) a where
--   (:#)  :: a -> Proxy n -> Expr n a
--   And   :: (KnownNat n, KnownNat m, KnownNat (n * m)) => Expr n a -> Expr m a -> Expr (n * m) a
--   Or    :: (KnownNat n, KnownNat m, KnownNat (n + m)) => Either (Expr n a) (Expr m b) -> Expr (n + m) a
--   (:->) :: (KnownNat n, KnownNat m, KnownNat (Choose m n)) => Proxy n -> Proxy m -> (a -> a) -> Expr (Choose m n) a

-- type family Choose (n :: Nat) (m :: Nat) :: Nat where
--   Choose _ 0 = 1
--   Choose n n = 1
--   Choose n k = Choose (n-1) k + Choose (n-1) (k-1)




-- data SomeExpr a = forall (n :: Nat). KnownNat n => SomeExpr (Expr n a)


-- someLit :: Typeable a => a -> SomeNat -> SomeExpr a
-- someLit x (SomeNat n) = SomeExpr (x :# n)

-- someAnd :: SomeExpr a -> SomeExpr a -> SomeExpr a
-- someAnd (SomeExpr x) (SomeExpr y) = SomeExpr (And x y)



data Expr n a where
  (:#)  :: a -> n -> Expr n a
  (:&)  :: Expr n a -> Expr n a -> Expr n a
  (:|)  :: Expr n a -> Expr n a -> Expr n a
  (:->) :: Expr n a -> Expr n a -> Expr n a
  deriving (Eq, Ord, Show)

calcExpr :: (Num n, Integral n) => Expr n a -> n
calcExpr ((:#) _ n) = n
calcExpr ((:&) x y) = calcExpr x * calcExpr y
calcExpr ((:|) x y) = calcExpr x + calcExpr y
calcExpr ((:->) x y) = choose (calcExpr y) (calcExpr x)


instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

-- choose :: Int -> Int -> Int
choose :: (Num a, Integral a) => a -> a -> a
choose n k = (([1,1]^n)++repeat 0) `genericIndex` k




-- eitherP :: Parser a -> Parser b -> Parser (Either a b)
-- eitherP p q = (Left <$> p) <|> (Right <$> q)

-- :#
-- ,
-- |
-- ->

-- | Non-empty word
word :: Parser T.Text
word = takeWhile1 isAlphaNum

-- | Word, incl. empty
word0 :: Parser T.Text
word0 = A.takeWhile isAlphaNum

exprLit :: Integral n => Parser (Expr n T.Text)
exprLit = liftM2 (:#) word (skipSpace >> string ":#" >> skipSpace >> decimal)

exprTo :: Integral n => Parser (Expr n T.Text)
exprTo = liftM2 (:->) (word >- parseExpr1) (skipSpace >> string "->" >> skipSpace >> (word0 >- parseExpr1))

exprAnd :: Integral n => Parser (Expr n T.Text)
exprAnd = liftM2 (:&) (word >- parseExpr1) (skipSpace >> char ',' >> skipSpace >> (word0 >- parseExpr1))

exprOr :: Integral n => Parser (Expr n T.Text)
exprOr = liftM2 (:|) (word0 >- parseExpr1) (skipSpace >> char '|' >> skipSpace >> (word0 >- parseExpr1))

parseExpr1 :: Integral n => Parser (Expr n T.Text)
parseExpr1 = exprLit <|> exprTo <|> exprAnd <|> exprOr


data Binop a = BinL a a | BinR a a | BinM a a a | BinO a deriving (Eq, Ord, Show)

-- word bin word
--      bin word
-- word bin

-- word bin
binL :: Parser (Binop T.Text)
binL = liftM2 BinL word notWord

--      bin word
binR :: Parser (Binop T.Text)
binR = liftM2 BinR notWord word

-- word bin word
binM :: Parser (Binop T.Text)
binM = liftM3 BinM word notWord word

binO :: Parser (Binop T.Text)
binO = skipSpace >> BinO <$> notWord

binOp :: Parser (Binop T.Text)
binOp = (binM <|> binR <|> binL <|> binO) <?> "binOp"


extractLeft :: Parens (Either a b) -> Maybe a
extractLeft (Parens (Left (Left x))) = Just x
extractLeft (Parens (Right [x])) = extractLeft x
extractLeft  _                   = Nothing

extractRight :: Parens (Either a b) -> Maybe b
extractRight (Parens (Left (Right x))) = Just x
extractRight (Parens (Right [x])) = extractRight x
extractRight  _                   = Nothing

-- (Parens (Right (x : (Right (BinO y   ))) : Parens z : ws))) =
-- Parens (Right (Parens (Right xs) : Parens (Left (Right (BinR y ys))) : zs))
-- Parens (Right (Parens (Left (Right (BinL x xs))) : Parens () : zs))


-- newtype Parens a = Parens { getParens :: Either a [Parens a] } deriving (Eq, Ord)






notWord :: Parser T.Text
notWord = takeWhile1 (not . isAlphaNum)

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
  , "(((z :# 0), ((a :# 1), (b :# 2))) | (a -> b, b -> a))" -- < a trick, ((z :# 0, constraints) | expr) === constraints => expr
  -- (('z' :# 0) :& (('a' :# 1) :& ('b' :# 2))) :| (('a' :# 1) :-> ('b' :# 2) :& ('b' :# 2) :-> ('a' :# 1))
  ]

-- A verbose proof that there is no bijection between ('a' :# 1) and ('b' :# 2):
-- λ> calcExpr $ (('z' :# 0) :& (('a' :# 1) :& ('b' :# 2))) :| (('a' :# 1) :-> ('b' :# 2) :& ('b' :# 2) :-> ('a' :# 1))
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

-- parseExamples3 = flip map examples $ \x -> do
--   y <- parseOnly(parseParens(eitherP (liftM2(:#)word(skipSpace>>string ":#">>skipSpace>>decimal)) binOp)) x
--   case y of
--     (Parens (Left (Left  x))) -> Right x
--     (Parens (Left (Right x))) -> Left $ unlines ["not sure how a Right got in here:", show x]
--     (Parens (Right [z]))      -> maybe (Left $ show z) Right $ extractLeft z
--     (Parens (Right  z ))      -> do
--       -- All operations on this level are the same
--       case allEq (catMaybes (map extractRight z)) of
--         Nothing -> Left "Not all binOps on a single level of Parens are equal"
--         (Just w) -> do
--           let filtered = filterMaps (either Just (const Nothing)) z
--           -- filter out binops
--           -- parse that binop
--           -- fold, using that binop, over the filtered list
--           _ w filtered

-- Meh, the above needs to be adjusted, since it's checking the binops for equality, including their arguments..


filterMap :: (a -> Maybe b) -> Parens a -> Maybe (Parens b)
filterMap f (Parens (Left x)) = maybe Nothing (Just . Parens . Left) (f x)
filterMap f (Parens (Right x)) = case filterMaps f x of
                                   [] -> Nothing
                                   y  -> Just (Parens (Right y))

filterMaps :: (a -> Maybe b) -> [Parens a] -> [Parens b]
filterMaps f = catMaybes . map (filterMap f)

allEq :: Eq a => [a] -> Maybe a
allEq x = case nub x of
            [y] -> Just y
            _   -> Nothing

-- xs = case allLeft xs of
--        Left  ys -> ys
--        Right _  ->



-- takes = do
--   x <- sepBy1 (takeWhile1 (\x -> not (isSpace x) && x /= ':' && x /= '-' && x /= ',' && x /= '|')) (string ":#")
--   skipSpace
--   c <- peekChar'
--   case c of
--     ':' -> do
--       string ":#"
--       y <- takeText >- parseE1
--     '-' -> do
--       string "->"
--       y <- takeText >- parseE1
--     ',' -> do
--       char ','
--       y <- takeText
--       return (x, y)
--     '|' -> do
--       char '|'
--       y <- takeText
--       return (x, y)


parseExpr :: Integral n => Parser (Parens (Expr n T.Text))
parseExpr = parseParens parseExpr1



-- maybeParens :: Parser a -> Parser a
-- maybeParens p = do
--   skipSpace
--   c <- peekChar'
--   if c == '('
--      then do
--        p' <- p
--        _  <- char ')'
--        return p'
--      else p

-- binop :: Parser a -> Parser t -> Parser b -> Parser (a, b)
-- binop p q r = maybeParens $ do
--   p' <- p
--   skipSpace
--   _ <- q
--   skipSpace
--   r' <- r
--   return (p', r')



-- -- | An `Expr`
-- --
-- -- @
-- -- (xs :# n)
-- -- (a , b)
-- -- (a | b)
-- -- (a -> b)
-- -- @
-- expr :: Parser (Expr (Maybe Int) T.Text)
-- expr = parseLiteral <|> parseAnd <|> parseOr <|> parseTo <|> parseRawLiteral

-- parseRawLiteral :: Parser (Expr (Maybe Int) T.Text)
-- parseRawLiteral = (:# Nothing) <$> takeWhile1 (not . isSpace)

-- parseLiteral :: Parser (Expr (Maybe Int) T.Text)
-- parseLiteral = uncurry (:#) <$> binop (takeWhile1 (not . isSpace)) (string ":#") (fmap Just decimal)

-- parseAnd :: Parser (Expr (Maybe Int) T.Text)
-- parseAnd = uncurry And <$> binop expr (char ',') expr

-- parseOr :: Parser (Expr (Maybe Int) T.Text)
-- parseOr = uncurry Or <$> binop expr (char '|') expr

-- parseTo :: Parser (Expr (Maybe Int) T.Text)
-- parseTo = uncurry (:->) <$> binop expr (string "->") expr

-- int :: Parser Int
-- int = read . init . tail . show <$> takeWhile1 isDigit


-- expr = parens | nat | lit

-- parens = do
--   _  <- char '('
--   x <- expr
--   skipSpace
--   y <- string ":#" <|> string "->" <|> return <$> char ',' <|> return <$> char '|'
--   skipSpace
--   z <- takeWhile (/= ')')
--   _ <- char ')'





-- expr :: Parser a -> Parser (SomeExpr a)
-- expr p = _

-- exprInh p = parens $ do
--   name <- p . takeWhile (not . isSpace)
--   skipSpace
--   _ <- string ":#"
--   skipSpace
--   inh  <- takeWhile isDigit
--   SomeExp (name :# inh)

-- exprAnd p = parens $ do
--   x <- expr p . takeWhile (not . isSpace)
--   skipSpace
--   _ <- char ','
--   skipSpace
--   y  <- expr p . takeWhile (not . isSpace)
--   SomeExp (And x y)

-- exprOr p = parens $ do
--   name <- p . takeWhile (not . isSpace)
--   skipSpace
--   _ <- char '|'
--   skipSpace
--   inh  <- takeWhile isDigit
--   SomeExp (Or x y)

-- exprTo p = parens $ do
--   name <- p . takeWhile (not . isSpace)
--   skipSpace
--   _ <- string ":->"
--   skipSpace
--   inh  <- takeWhile isDigit
--   SomeExp (name :-> inh)


-- parens :: Parser a -> Parser a
-- parens p = do
--   _  <- char '('
--   skipSpace
--   p' <- p
--   skipSpace
--   _  <- char ')'


-- instance Eq a => Eq (Expr n a) where
--   (Lit _ y) == (Lit _ w) = y == w
--   (And x y) == (And z w) = x == z && y == q
--   (Or  x y) == (Or z w) = x == z && y == w
--   (To  x y) == (To z w) = x == z && y == w
--   _       == _      = False

-- instance (KnownNat n, Show a) => Show (Expr n a) where
--   show (Lit p x) = concat ["(", show x, " :# ", show (natVal p)")"]
--   show (And x y) = concat ["(", show x, ", ", show y, ")"]
--   show (Or  x y) = concat ["(", show x, " | ", show y, ")"]
--   show (To  x y) = concat ["(", show x, " -> ", show y, ")"]

-- data MaybeExpr (n :: Nat) where
--   JustExpr    :: (m <= n, KnownSymbol a) => Expr m a -> MaybeExpr n
--   NothingExpr :: MaybeExpr n





-- data Exp = forall (n :: Nat) (a :: *). (KnownNat n, Typeable a) => Exp { getExp :: Expr n a }

-- instance Eq Exp where



someFunc :: IO ()
someFunc = putStrLn "todo"
