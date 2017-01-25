-- file: repstring.hs
import Options.Applicative
import Data.Maybe (fromJust, isJust)
-- import Data.Monoid ((<>))

data Argumenty = Argumenty
  { nazwaPliku :: String
  , x :: Maybe Int
  , y :: Maybe Int }

-- replicateString :: Sample -> IO ()
-- replicateString (Sample string n) = 
--     do 
--       if not True then putStrLn repstring else putStrLn $ reverse repstring
--           where repstring = foldr (++) "" $ replicate n string

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
przechwycArgumenty (Argumenty nazwaPliku maybeX maybeY)   = putStrLn nazwaPliku

main :: IO ()
main = execParser opts >>= przechwycArgumenty
  where
    opts = info (helper <*> argumenty)
      ( fullDesc
        <> progDesc "Podaj nazwę pliku z komendami oraz opcjonalnie współrzędne (x,y) gdzie ma zaczynać się obrazek."
        <> header "ludzik - program do rysowania ludzików!" )