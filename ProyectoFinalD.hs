{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
-- import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec as M
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Expr
  = Var Char
  | Abs {var :: Expr, cuerpo :: String}
  | App {var :: Expr, cuerpo :: String, apli :: Expr}
  deriving (Eq, Ord, Show)

sust :: Char -> String -> String -> String
sust c a [] = []
sust c a (x:xs) = if c == x then a ++ sust c a xs else (:) x $ sust c a xs

pApp :: Parser Expr
pApp = do 
  void (string "(\\")
  vDeLigado <- M.some alphaNumChar
  void (char '.')
  cuerpo <- M.some alphaNumChar
  void (char ')')
  space1 <?> "Tienes que separar tus términos por al menos un espacio"
  apli <- try <|> pApp <|> pAbs <|> pVar
  return (App (Var $ head vDeLigado) cuerpo apli)

pAbs :: Parser Expr
pAbs = do 
  void (string "\\")
  vDeLigado <- M.some alphaNumChar
  void (char '.')
  cuerpo <- M.some alphaNumChar <?> "Estás usando characteres especiales en el cuerpo"
  return (Abs (Var $ head vDeLigado) cuerpo)

pVar :: Parser Expr
pVar = do
  void (string "")
  var <- M.some alphaNumChar
  return (Var $ head var)




main :: IO ()
main = do
  putStrLn "Introduce tu abstracción Lambda:"
  linea <- getLine
  parseTest (single 'a' :: Parser Char) "a"
