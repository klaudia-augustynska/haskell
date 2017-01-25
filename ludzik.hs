import Options.Applicative
import Data.Maybe (fromMaybe)

data Argumenty = Argumenty
  { nazwaPliku :: String
  , x :: Maybe Int
  , y :: Maybe Int }

data Komenda = Kolko Float Float Float
             | Kreska Float Float Float Float

argumenty :: Parser Argumenty
argumenty = Argumenty
  <$> argument str 
    (  metavar "nazwaPliku"
    <> help "Plik z komendami do rysowania ludzika" )
  <*> optional 
    ( argument auto
      (  metavar "x"
      <> help "Ilość pikseli od lewej w pliku wynikowym" ))
  <*> optional 
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