module ParserLogo (parsujPlik) where
import Komenda
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

np :: Parser Komenda
np = do
    spaces
    string "np"
    spaces
    ile <- (many1 digit) <|> argument
    spaces
    return (Naprzod ile)

ws :: Parser Komenda
ws = do
    spaces
    string "ws"
    spaces
    ile <- (many1 digit) <|> argument
    spaces
    return (Wstecz ile)

pw :: Parser Komenda
pw = do
    spaces
    string "pw"
    spaces
    kat <- (many1 digit) <|> argument
    spaces
    return (Prawo kat)

lw :: Parser Komenda
lw = do
    spaces
    string "lw"
    spaces
    kat <- (many1 digit) <|> argument
    spaces
    return (Lewo kat)

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
    nrKoloru <- many1 digit <|> argument
    spaces
    return (UstawKolorPisaka nrKoloru)

ugp :: Parser Komenda
ugp = do
    spaces
    string "ugp"
    spaces
    grubosc <- many1 digit <|> argument
    spaces
    return (UstawGruboscPisaka grubosc)

powtorz :: Parser Komenda
powtorz = do
    spaces
    string "powtórz"
    spaces
    ile <- many1 digit <|> argument
    spaces
    char '['
    lista <- many (tryEverything)
    char ']'
    spaces
    return (Powtorz ile lista)

argument :: Parser String
argument = do
    spaces
    char ':'
    nazwa <- many1 letter
    spaces
    return nazwa

procedura :: Parser Komenda
procedura = do
    spaces
    string "oto"
    spaces
    nazwa <- many1 letter
    spaces
    argumenty <- many (try argument)
    spaces
    komendy <- many1 (tryEverything)
    spaces
    string "już"
    spaces
    return (Procedura nazwa argumenty komendy)

liczbaAlboNazwa :: Parser String
liczbaAlboNazwa = do
    spaces
    wartosc <- (many1 digit) <|> (many1 letter)
    spaces
    return wartosc

listaSlowKluczowych :: [String]
listaSlowKluczowych = ["np","ws","pw","lw","opu","pod","cs","ukp","ugp","powtórz","oto","już"]

-- sprawdzanie jest na samym końcu gdy już i tak nie pasuje żadna komenda
wywolanieProcedury :: Parser Komenda
wywolanieProcedury = do
    spaces
    nazwa <- many1 letter
    spaces
    argumenty <- manyTill liczbaAlboNazwa $
        foldl (<|>) (try $ lookAhead $ eof >> (string "")) (map (try . lookAhead . string) listaSlowKluczowych)
    spaces
    return (WywolanieProcedury nazwa argumenty)

listaDostepnychKomend :: [Parser Komenda]
listaDostepnychKomend = [np,ws,pw,lw,opu,pod,cs,ukp,ugp,powtorz]

tryEverything :: Parser Komenda
tryEverything = foldl (<|>) (try np) (map try listaDostepnychKomend)

file = many1 (tryEverything <|> try procedura <|> try wywolanieProcedury)

parsujPlik :: String -> Either ParseError [Komenda]
parsujPlik tresc = parse file "nieistotne dopoki nie ma bledu" tresc