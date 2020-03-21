{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}

module Parser (parsePara) where

import Paranet.Common

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Char as Char
import Data.Bifunctor (first, second)

import GHC.Base (Int(..), (+#))

parsePara :: String -> Either String Expression
parsePara = runParser (many space *> pExpression <* eof)

type Parser = ExceptT String (State [(Int, Char)])

runParser :: Parser a -> String -> Either String a
runParser p input = evalState (runExceptT p) (indexed input)

data Assoc m a
    = InfixR (m (a -> a -> a))
    | InfixL (m (a -> a -> a))

pExpression :: Parser Expression
pExpression = lexeme do
    foldl makeParser pValue
        [ [ InfixR ((:|)  <$ pSymbol "|")
          , InfixL ((:&)  <$ pSymbol "&")
          , InfixL ((:~)  <$ pSymbol "~")  ]
        , [ InfixL ((:->) <$ pSymbol "->") ] ]

makeParser :: Parser a -> [Assoc Parser a] -> Parser a
makeParser pTerm ops =
    pTerm >>= \x -> choice [ infixR x, infixL x, pure x ]
  where (inR, inL) = foldr splitOp ([], []) ops
        infixR = pInfixR (choice inR) pTerm
        infixL = pInfixL (choice inL) pTerm

        splitOp (InfixR p) = first (p:)
        splitOp (InfixL p) = second (p:)

        pInfixR op p x =
            op <*> pure x <*> (p >>= liftA2 (<|>) (pInfixR op p) pure)

        pInfixL op p x = do
            f <- op
            y <- p
            let r = f x y
            pInfixL op p r <|> pure r

pValue :: Parser Expression
pValue = lexeme do
    choice
        [ pInteger
        , pId
        , pSymbol "(" *> pExpression <* pSymbol ")" ]

pInteger :: Parser Expression
pInteger = EInt . read @Integer <$> some (satisfy Char.isDigit)

pId :: Parser Expression
pId = EId <$> some (satisfy Char.isLetter)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = get >>= \case
  []         -> throwError "Unexpected EOF"
  (n,x):xs | not (f x)
             -> throwError ("At char #" <> show n <> ": Unexpected " <> show x)
           | otherwise
             -> x <$ put xs

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

eof :: Parser ()
eof = get >>= \case
  []      -> pure ()
  (n,x):_ -> throwError ("At char #" <> show n <> ": Unexpected " <> show x <> ", expected EOF")

pSymbol :: String -> Parser String
pSymbol ""     = lexeme (pure "")
pSymbol (s:ss) = lexeme do
    (:) <$> satisfy (== s)-- `catchError` \e -> throwError (e <> ", expected " <> show s))
        <*> pSymbol ss

lexeme :: Parser a -> Parser a
lexeme p = p <* many space -- skipMany (some space)
  -- where skipMany = void . many

space :: Parser ()
space = void (satisfy Char.isSpace)

-----------------------------------------------------------------------------

indexed :: [a] -> [(Int, a)]
indexed xs = go 0# xs
  where
    go i (a:as) = (I# i, a) : go (i +# 1#) as
    go _ _      = []
{-# NOINLINE [1] indexed #-}
