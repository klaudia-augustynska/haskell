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

opu :: Parser Komenda
opu = do
    spaces
    string "opu"
    spaces
    return Opusc

pod :: Parser Komenda
pod = do
    spaces
    string "pod"
    spaces
    return Podnies

cs :: Parser Komenda
cs = do
    spaces
    string "cs"
    spaces
    return Czysc

file = many1 (try np <|> try pw <|> try opu <|> try pod <|> try cs)

parsujPlik :: String -> Either ParseError [Komenda]
parsujPlik tresc = parse file "nieistotne dopoki nie ma bledu" tresc