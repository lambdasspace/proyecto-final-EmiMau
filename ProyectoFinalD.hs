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
  | App {var :: Expr, cuerpo :: String, apli :: Expr} --- , rest :: String}
  deriving (Eq, Ord, Show)

sust :: Char -> String -> String -> String
sust c a [] = []
sust c a (x:xs) = if c == x then a ++ sust c a xs else (:) x $ sust c a xs

pApp :: Parser Expr
pApp = do 
  void (string "(\\")
  vDeLigado <- alphaNumChar <?> "Error: Identificador de más de un character"
  void (char '.')
  cuerpo <- M.some alphaNumChar
  void (char ')')
  space1 <?> "Error: Tienes que separar tus términos por al menos un espacio"
  apli <- try pAbs <|> pVar <?> "Falló parser"
  return (App (Var vDeLigado) cuerpo apli ) --- Esto ya no funciona porque estaba tratando de hacer la recusión sobre una función básica "ola")

pAbs :: Parser Expr
pAbs = do 
  void (string "\\")
  vDeLigado <- alphaNumChar
  void (char '.') <?> "Error: Identificador de más de un character"
  cuerpo <- M.some alphaNumChar <?> "Error: Estás usando characteres especiales en el cuerpo"
  return (Abs (Var vDeLigado) cuerpo)

pVar :: Parser Expr
pVar = do
  void (string "") <?> "Error: Identificador de más de un character"
  var <- alphaNumChar
  return (Var var)




main :: IO ()
main = do
  putStrLn "Introduce tu abstracción Lambda:"
  linea <- getLine
  parseTest (single 'a' :: Parser Char) "a"
