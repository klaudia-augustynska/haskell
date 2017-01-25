import Options.Applicative
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

data Argumenty = Argumenty
  { plik :: String
  , x :: Maybe Int
  , y :: Maybe Int }

data Komenda = Kolko Float Float Float
             | Kreska Float Float Float Float deriving Show

argumenty :: Options.Applicative.Parser Argumenty
argumenty = Argumenty
  <$> argument str 
    (  metavar "nazwaPliku"
    <> help "Plik z komendami do rysowania ludzika" )
  <*> Options.Applicative.optional 
    ( argument auto
      (  metavar "x"
      <> help "Ilość pikseli od lewej w pliku wynikowym" ))
  <*> Options.Applicative.optional 
    ( argument auto
      (  metavar "y"
      <> help "Ilość pikseli od góry w pliku wynikowym" ))

przechwycArgumenty :: Argumenty -> IO ()
przechwycArgumenty Argumenty {plik = nazwaPliku, x = maybeX, y = maybeY} =
  let 
    x = fromMaybe 0 maybeX
    y = fromMaybe 0 maybeY
    -- komendy = parsujPlik nazwaPliku
  in
    do
      dupa <- readFile nazwaPliku
      -- writeFile "wynik.txt" (dupa ++ "\n")
      -- appendFile "asdf" dupa
      print dupa

--parsujPlik :: String -> [Komenda]
-- parsujPlik _ = [Kolko 50 100 30]
-- parsujPlik nazwaPliku = do
--   dupa <- readFile "test.txt"
--   return dupa
  -- inh <- openFile "test.txt" ReadMode
  -- outh <- openFile "output.txt" WriteMode
  -- mainloop inh outh
  -- hClose inh
  -- hClose outh
  -- return [Kolko 50 100 30]

file = endBy line eol
line = sepBy cell (char ' ')
cell = Text.ParserCombinators.Parsec.many (noneOf ",\n\r")
eol =   try (string "\n\r")
    Text.ParserCombinators.Parsec.<|> try (string "\r\n")
    Text.ParserCombinators.Parsec.<|> string "\n"
    Text.ParserCombinators.Parsec.<|> string "\r"
uzyjParseca :: String -> String -> Either Text.ParserCombinators.Parsec.ParseError [[String]]
uzyjParseca = parse file

main :: IO ()
main = execParser opts >>= przechwycArgumenty
  where
    opts = info (helper <*> argumenty)
      ( fullDesc
        <> progDesc "Podaj nazwę pliku z komendami oraz opcjonalnie współrzędne (x,y) gdzie ma zaczynać się obrazek."
        <> header "ludzik - program do rysowania ludzików!" )