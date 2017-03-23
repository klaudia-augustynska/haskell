module ParserLogo (parsujPlik) where
import Komenda
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

np :: Parser Komenda
np = do
    spaces
    string "np"
    spaces
    ile <- many1 digit
    spaces
    return (Naprzod (read ile))

pw :: Parser Komenda
pw = do
    spaces
    string "pw"
    spaces
    kat <- many1 digit
    spaces
    return (Prawo (read kat))

file = many1 (try np <|> try pw)

parsujPlik :: String -> Either ParseError [Komenda]
parsujPlik tresc = parse file "nieistotne dopoki nie ma bledu" tresc