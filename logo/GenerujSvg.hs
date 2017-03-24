module GenerujSvg (generujSvg) where 
import Komenda

type Kat = Int
type Podniesiony = Bool
type CzyJuzCzyscimy = Bool
type Wspolrzedne = (Int,Int)

poczatkowyKat :: Kat
poczatkowyKat = 0

poczatkowyPodniesiony :: Podniesiony
poczatkowyPodniesiony = False

poczatkowyCzyJuzCzyscimy :: CzyJuzCzyscimy
poczatkowyCzyJuzCzyscimy = False

poczatkoweWspolrzedne :: Wspolrzedne
poczatkoweWspolrzedne = (250,250)

poczatkowyKolor :: Int
poczatkowyKolor = 0

poczatkowaGrubosc :: Int
poczatkowaGrubosc = 4

generujSvg :: [Komenda] -> String -> IO ()
generujSvg lista plik = writeFile (plik ++ ".html") $ ((svgNaglowek 0 20) ++ (svgSrodek lista) ++ svgStopka)
--generujSvg lista plik = print $ obslugaCzysc lista

svgNaglowek :: Int -> Int -> String
svgNaglowek x y = "<!doctype html> \n\
    \<html style=\"height:100%\">\n\
    \   <head></head>\n\
    \   <body style=\"margin:0;background:#c0c0c0;padding:" ++ (show y) ++ "px 0 0 " ++ (show x) ++ "px\">\n\
    \       <svg style=\"width:500px;height:500px;margin:auto;display:block;background:#fff\">\n"

svgSrodek :: [Komenda] -> String
svgSrodek lista = interpretacjaKomend poczatkowyKat poczatkowyPodniesiony poczatkowyCzyJuzCzyscimy poczatkoweWspolrzedne poczatkowyKolor poczatkowaGrubosc (obslugaCzysc lista)

svgStopka :: String
svgStopka = "           Sorry, your browser does not support inline SVG.\n\
    \       </svg>\n\
    \   </body>\n\
    \</html>"

obslugaCzysc :: [Komenda] -> [Komenda]
obslugaCzysc lista = reverse $ obslugaCzysc' (reverse lista) False

obslugaCzysc' :: [Komenda] -> CzyJuzCzyscimy -> [Komenda]
obslugaCzysc' (Czysc:resztaListy) False = KoniecCzyszczenia : (obslugaCzysc' resztaListy True)
obslugaCzysc' (Czysc:resztaListy) True = obslugaCzysc' resztaListy True
obslugaCzysc' [] True = [BedzieCzyszczenie]
obslugaCzysc' [] False = []
obslugaCzysc' (a:ax) czyJuzCzyscimy = a : (obslugaCzysc' ax czyJuzCzyscimy)

interpretacjaKomend :: Kat -> Podniesiony -> CzyJuzCzyscimy -> Wspolrzedne -> Int -> Int -> [Komenda] -> String
-- Naprzód, Wstecz
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne kolor grubosc (Naprzod ile:resztaKomend) = 
    (if czyPodniesiony == False && czyCzyscimy == False   
        then 
            let kolor' = kodKoloruNaNazwe kolor 
            in rysujLinie wspolrzedne (noweWspolrzedne wspolrzedne ile kat) kolor' grubosc
        else "") 
    ++ interpretacjaKomend kat czyPodniesiony czyCzyscimy (noweWspolrzedne wspolrzedne ile kat) kolor grubosc resztaKomend
interpretacjaKomend a b c d e f (Wstecz ile:resztaKomend) = 
    let x = -ile in 
        interpretacjaKomend a b c d e f ((Naprzod x) : resztaKomend)
-- Prawo, Lewo
interpretacjaKomend kat b c d e f (Prawo nowyKat:resztaKomend) = 
    interpretacjaKomend ((kat + nowyKat) `mod` 360) b c d e f resztaKomend
interpretacjaKomend kat b c d e f (Lewo nowyKat:resztaKomend) = 
    interpretacjaKomend ((kat - nowyKat) `mod` 360) b c d e f resztaKomend
-- Opuść, Podnieś
interpretacjaKomend a _ c d e f (Opusc:resztaKomend) = 
    interpretacjaKomend a False c d e f resztaKomend
interpretacjaKomend a _ c d e f (Podnies:resztaKomend) =
    interpretacjaKomend a True c d e f resztaKomend
-- Czyść
interpretacjaKomend a b _ d e f (BedzieCzyszczenie:resztaKomend) = 
    interpretacjaKomend a b True d e f resztaKomend
interpretacjaKomend a b _ d e f (KoniecCzyszczenia:resztaKomend) = 
    interpretacjaKomend a b False d e f resztaKomend
-- Kolor, Grubość
interpretacjaKomend a b c d _ f (UstawKolorPisaka nrKoloru:resztaKomend) = 
    interpretacjaKomend a b c d nrKoloru f resztaKomend
interpretacjaKomend a b c d e _ (UstawGruboscPisaka grubosc:resztaKomend) =
    interpretacjaKomend a b c d e grubosc resztaKomend
-- Powtórz
interpretacjaKomend a b c d e f (Powtorz 0 _:resztaKomend) =
    interpretacjaKomend a b c d e f resztaKomend
interpretacjaKomend a b c d e f (Powtorz ile lista:resztaKomend) = 
    interpretacjaKomend a b c d e f (lista ++ [Powtorz (ile-1) lista] ++ resztaKomend)
interpretacjaKomend _ _ _ _ _ _ [] = ""

kodKoloruNaNazwe :: Int -> String
kodKoloruNaNazwe k = case k of 
    0 -> "#000000"
    1 -> "#000080"
    2 -> "#008000"
    3 -> "#008080"
    4 -> "#800000"
    5 -> "#800080"
    6 -> "#808000"
    7 -> "#C0C0FF"
    8 -> "#808080"
    9 -> "#0000FF"
    10 -> "#00FF00"
    11 -> "#00FFFF"
    12 -> "#FF0000"
    13 -> "#FF00FF"
    14 -> "#FFFF00"
    15 -> "#FFFFFF"
    _ -> "#000000"

noweWspolrzedne :: Wspolrzedne -> Int -> Kat -> Wspolrzedne
noweWspolrzedne (x,y) ile kat = (x + ( round $ sin (radiany kat) * (fromIntegral ile)), y - (round $ cos (radiany kat) * (fromIntegral ile)))

radiany :: Kat -> Float
radiany stopnie = fromIntegral stopnie * pi / 180

rysujLinie :: (Int,Int) -> (Int,Int) -> String -> Int -> String
rysujLinie (x1,y1) (x2,y2) kolor grubosc = "         <line x1=\"" ++ (show x1) 
    ++ "\" y1=\"" ++ (show y1) 
    ++ "\" x2=\"" ++ (show x2)
    ++ "\" y2=\"" ++ (show y2)
    ++ "\" stroke=\"" ++ kolor
    ++ "\" stroke-width=\"" ++ (show grubosc)
    ++ "\" />\n" 