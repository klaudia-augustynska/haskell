import Options.Applicative
import Data.Maybe (fromJust, isJust)

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
  do
    case maybeX of
      Just n -> putStrLn "mamy x"
      Nothing -> putStrLn "nie mamy x"
    case maybeY of
      Just n -> putStrLn "mamy y"
      Nothing -> putStrLn "nie mamy y"

main :: IO ()
main = execParser opts >>= przechwycArgumenty
  where
    opts = info (helper <*> argumenty)
      ( fullDesc
        <> progDesc "Podaj nazwę pliku z komendami oraz opcjonalnie współrzędne (x,y) gdzie ma zaczynać się obrazek."
        <> header "ludzik - program do rysowania ludzików!" )