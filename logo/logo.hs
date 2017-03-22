import Options.Applicative

data Argumenty = Argumenty
  { input :: String
  , output :: Maybe String }

argumenty :: Parser Argumenty
argumenty = Argumenty
  <$> argument str 
    (  metavar "input"
    <> help "Plik z komendami do rysowania ludzika" )
  <*> optional 
    ( argument str
      (  metavar "output"
      <> help "Nazwa pliku wynikowego, bez rozszerzenia, opcjonalnie" ))

przechwycArgumenty :: Argumenty -> IO ()
przechwycArgumenty Argumenty {input = inputFile, output = maybeOutput} =
    do
        print inputFile
        case maybeOutput of 
            Just x -> print x 
            Nothing -> print "dupa"

main :: IO ()
main = execParser opts >>= przechwycArgumenty
  where
    opts = info (helper <*> argumenty)
      ( fullDesc
        <> progDesc "Podaj nazwę pliku z komendami Logo Komeniusz oraz ew. nazwę pliku wyjściowego (ale i tak dodam do niego końcówkę .HTML, więc to nie jest tak że masz jakiś duży wybór)"
        <> header "logo - program, który pomimo że jest napisany w Haskellu, to daje radę zinterpretować komendy języka Logo i coś narysować w SVG." )