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
svgSrodek lista = interpretacjaKomend poczatkowyKat poczatkowyPodniesiony poczatkowyCzyJuzCzyscimy poczatkoweWspolrzedne (obslugaCzysc lista)

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

interpretacjaKomend :: Kat -> Podniesiony -> CzyJuzCzyscimy -> Wspolrzedne -> [Komenda] -> String
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne (Naprzod ile:resztaKomend) = 
    (if czyPodniesiony == False && czyCzyscimy == False   
        then rysujLinie wspolrzedne (noweWspolrzedne wspolrzedne ile kat)
        else "") 
    ++ interpretacjaKomend kat czyPodniesiony czyCzyscimy (noweWspolrzedne wspolrzedne ile kat) resztaKomend
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne (Prawo nowyKat:resztaKomend) = 
    interpretacjaKomend ((kat + nowyKat) `mod` 360) czyPodniesiony czyCzyscimy wspolrzedne resztaKomend
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne (Lewo nowyKat:resztaKomend) = 
    interpretacjaKomend ((kat - nowyKat) `mod` 360) czyPodniesiony czyCzyscimy wspolrzedne resztaKomend
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne (Opusc:resztaKomend) = 
    interpretacjaKomend kat False czyCzyscimy wspolrzedne resztaKomend
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne (Podnies:resztaKomend) =
    interpretacjaKomend kat True czyCzyscimy wspolrzedne resztaKomend
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne (BedzieCzyszczenie:resztaKomend) = 
    interpretacjaKomend kat czyPodniesiony True wspolrzedne resztaKomend
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne (KoniecCzyszczenia:resztaKomend) = 
    interpretacjaKomend kat czyPodniesiony False wspolrzedne resztaKomend
interpretacjaKomend _ _ _ _ [] = ""

noweWspolrzedne :: Wspolrzedne -> Int -> Kat -> Wspolrzedne
noweWspolrzedne (x,y) ile kat = (x + ( round $ sin (radiany kat) * (fromIntegral ile)), y - (round $ cos (radiany kat) * (fromIntegral ile)))

radiany :: Kat -> Float
radiany stopnie = fromIntegral stopnie * pi / 180

rysujLinie :: (Int,Int) -> (Int,Int) -> String
rysujLinie (x1,y1) (x2,y2) = "         <line x1=\"" ++ (show x1) 
    ++ "\" y1=\"" ++ (show y1) 
    ++ "\" x2=\"" ++ (show x2)
    ++ "\" y2=\"" ++ (show y2)
    ++ "\" stroke=\"black\" stroke-width=\"4\" />\n" 