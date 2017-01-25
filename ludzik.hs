import Options.Applicative
import Data.Maybe (fromMaybe)

data Argumenty = Argumenty
  { nazwaPliku :: String
  , x :: Maybe Int
  , y :: Maybe Int }

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
  in
    print (x*y)

main :: IO ()
main = execParser opts >>= przechwycArgumenty
  where
    opts = info (helper <*> argumenty)
      ( fullDesc
        <> progDesc "Podaj nazwę pliku z komendami oraz opcjonalnie współrzędne (x,y) gdzie ma zaczynać się obrazek."
        <> header "ludzik - program do rysowania ludzików!" )