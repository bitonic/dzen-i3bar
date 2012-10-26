module Dzen where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)

import Text.Parsec
import Text.Parsec.String

data Dzen
    = FG (Maybe String)
    | BG (Maybe String)
    | I FilePath
    | R String
    | RO String
    | C String
    | CO String
    | P String
    | PA String
    | TW
    | IB String
    | String String
    deriving (Show, Eq, Read)

parseDzen :: String -> Either ParseError [Dzen]
parseDzen = parse dzen ""

dzen :: Parser [Dzen]
dzen = do s <- manyTill anyChar (void (char '^') <|> lookAhead eof)
          (String s :) <$> ((eof >> return []) <|> ((:) <$> controlSeqs <*> dzen))

controlSeqs :: Parser Dzen
controlSeqs =     maybeSeq "fg" FG
              <|> maybeSeq "bg" BG
              <|> try (stringSeq "i" I)
              <|> stringSeq "ib" IB
              <|> try (stringSeq "c" C)
              <|> stringSeq "co" CO
              <|> try (stringSeq "r" R)
              <|> stringSeq "ro" RO
              <|> try (stringSeq "p" P)
              <|> stringSeq "pa" PA
              <|> nullSeq "tw" TW
              <|> (String <$> string "^")
              <?> "dzen command"

parseSeq :: String -> Parser a -> (a -> b) -> Parser b
parseSeq s p f = string s >> (f <$> between (char '(') (char ')') p)

notLParen :: Parser String
notLParen = many (satisfy (/= ')'))

stringSeq :: String -> (String -> a) -> Parser a
stringSeq s = parseSeq s notLParen

maybeSeq :: String -> (Maybe String -> a) -> Parser a
maybeSeq s f = do res <- parseSeq s notLParen id
                  return (f (if null res then Nothing else Just res))

nullSeq :: String -> a -> Parser a
nullSeq s v = parseSeq s (return ()) (const v)
