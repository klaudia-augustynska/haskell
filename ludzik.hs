import Options.Applicative
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

data Argumenty = Argumenty
  { nazwaPliku :: String
  , x :: Maybe Int
  , y :: Maybe Int }

data Komenda = Kolko Float Float Float
             | Kreska Float Float Float Float

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
przechwycArgumenty (Argumenty nazwaPliku maybeX maybeY) =
  let 
    x = fromMaybe 0 maybeX
    y = fromMaybe 0 maybeY
    komendy = parsujPlik nazwaPliku
  in
    print (x*y)

parsujPlik :: String -> [Komenda]
parsujPlik _ = [Kolko 50 100 30]

main :: IO ()
main = execParser opts >>= przechwycArgumenty
  where
    opts = info (helper <*> argumenty)
      ( fullDesc
        <> progDesc "Podaj nazwę pliku z komendami oraz opcjonalnie współrzędne (x,y) gdzie ma zaczynać się obrazek."
        <> header "ludzik - program do rysowania ludzików!" )