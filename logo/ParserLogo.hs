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

ws :: Parser Komenda
ws = do
    spaces
    string "ws"
    spaces
    ile <- many1 digit
    spaces
    return (Wstecz (read ile))

pw :: Parser Komenda
pw = do
    spaces
    string "pw"
    spaces
    kat <- many1 digit
    spaces
    return (Prawo (read kat))

lw :: Parser Komenda
lw = do
    spaces
    string "lw"
    spaces
    kat <- many1 digit
    spaces
    return (Lewo (read kat))

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

ukp :: Parser Komenda
ukp = do
    spaces
    string "ukp"
    spaces
    nrKoloru <- many1 digit
    spaces
    return (UstawKolorPisaka (read nrKoloru))

ugp :: Parser Komenda
ugp = do
    spaces
    string "ugp"
    spaces
    grubosc <- many1 digit
    spaces
    return (UstawGruboscPisaka (read grubosc))

file = many1 (try np <|> try ws <|> try pw <|> try lw <|> try opu <|> try pod <|> try cs <|> try ukp <|> try ugp)

parsujPlik :: String -> Either ParseError [Komenda]
parsujPlik tresc = parse file "nieistotne dopoki nie ma bledu" tresc