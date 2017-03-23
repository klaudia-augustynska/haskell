import Options.Applicative
import ParserLogo
import GenerujSvg

data Argumenty = Argumenty
  { input :: String
  , output :: Maybe String }

argumenty :: Parser Argumenty
argumenty = Argumenty
  <$> argument str 
    (  metavar "input"
    <> help "Plik z komendami Logo" )
  <*> optional 
    ( argument str
      (  metavar "output"
      <> help "Nazwa pliku wynikowego, bez rozszerzenia, opcjonalnie" ))

przechwycArgumenty :: Argumenty -> IO ()
przechwycArgumenty Argumenty {input = inputFile, output = maybeOutput} =
    do
        tresc <- readFile inputFile
        let Right listaKomend = parsujPlik tresc
        generujSvg listaKomend (outputFile maybeOutput)

outputFile :: Maybe String -> String
outputFile (Just x) = x
outputFile Nothing = "wynik"

main :: IO ()
main = execParser opts >>= przechwycArgumenty
  where
    opts = info (helper <*> argumenty)
      ( fullDesc
        <> progDesc "Podaj nazwę pliku z komendami Logo Komeniusz oraz ew. nazwę pliku wyjściowego (ale i tak dodam do niego końcówkę .HTML, więc to nie jest tak że masz jakiś duży wybór)"
        <> header "logo - program, który pomimo że jest napisany w Haskellu, to daje radę zinterpretować komendy języka Logo i coś narysować w SVG." )