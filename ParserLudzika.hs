module ParserLudzika (uzyjParseca) where
import KomendaRysowania
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

kolko :: Parser Komenda
kolko = do
   spaces
   string "Kolko"
   spaces
   x <- many1 digit
   spaces
   y <- many1 digit
   spaces
   z <- many1 digit
   spaces
   return (Kolko (read x) (read y) (read z))

kreska :: Parser Komenda
kreska = do
    spaces
    string "Kreska"
    spaces
    x1 <- many1 digit
    spaces
    y1 <- many1 digit
    spaces
    x2 <- many1 digit
    spaces
    y2 <- many1 digit
    spaces
    return (Kreska (read x1) (read y1) (read x2) (read y2))

file = many1 (try kreska <|> try kolko)

uzyjParseca :: String -> Either ParseError [Komenda]
uzyjParseca tresc = parse file "nieistotne dopoki nie ma bledu" tresc