{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec as M
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Expr
  = Var {var :: Char}
  | Abs {var :: Char, cuerpo :: String}
  | App {var :: Char, cuerpo :: String, apli :: Expr}
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
  apli <- try pAbs <|> pVar <?> "Formato erróneao"
  return (App vDeLigado cuerpo apli )

pAbs :: Parser Expr
pAbs = do 
  void (string "\\")
  vDeLigado <- alphaNumChar
  void (char '.') <?> "Error: Identificador de más de un character"
  cuerpo <- M.some alphaNumChar <?> "Error: Estás usando characteres especiales en el cuerpo"
  return (Abs vDeLigado cuerpo)

pVar :: Parser Expr
pVar = do
  void (string "")
  var <- alphaNumChar
  return (Var var)

reduccion :: Expr -> String
reduccion Var{var=var}= [var]
reduccion Abs{var=var,cuerpo=cuerpo} = "(\\"++[var]++"."++cuerpo++")"
reduccion App{var=var,cuerpo=cuerpo,apli=apli} = sust var red cuerpo
  where red = reduccion apli

parserLambda = do 
  c <- try pApp <|> pAbs <|> pVar
  return (reduccion c)

main :: IO ()
main = do
  putStrLn "Introduce tu abstracción Lambda:"
  --Aquí por los escape tenemos que usar
  -- la sintaxis de una sóla '\'
  -- por ejemplo (\x.xyz) w 
  linea <- getLine
  parseTest parserLambda linea
