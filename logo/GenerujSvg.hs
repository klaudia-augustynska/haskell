module GenerujSvg (generujSvg) where 
import Komenda

type Kat = Int
type Podniesiony = Bool
type Wspolrzedne = (Int,Int)

poczatkowyKat :: Kat
poczatkowyKat = 0

poczatkowyPodniesiony :: Podniesiony
poczatkowyPodniesiony = True

poczatkoweWspolrzedne :: Wspolrzedne
poczatkoweWspolrzedne = (250,250)

generujSvg :: [Komenda] -> String -> IO ()
generujSvg lista plik = writeFile (plik ++ ".html") $ ((svgNaglowek 0 20) ++ (svgSrodek lista) ++ svgStopka)
--generujSvg lista plik = print $ lista

svgNaglowek :: Int -> Int -> String
svgNaglowek x y = "<!doctype html> \n\
    \<html style=\"height:100%\">\n\
    \   <head></head>\n\
    \   <body style=\"margin:0;background:#c0c0c0;padding:" ++ (show y) ++ "px 0 0 " ++ (show x) ++ "px\">\n\
    \       <svg style=\"width:500px;height:500px;margin:auto;display:block;background:#fff\">\n"

svgSrodek :: [Komenda] -> String
svgSrodek lista = interpretacjaKomend poczatkowyKat poczatkowyPodniesiony poczatkoweWspolrzedne lista

svgStopka :: String
svgStopka = "           Sorry, your browser does not support inline SVG.\n\
    \       </svg>\n\
    \   </body>\n\
    \</html>"

interpretacjaKomend :: Kat -> Podniesiony -> Wspolrzedne -> [Komenda] -> String
interpretacjaKomend kat czyPodniesiony wspolrzedne (Naprzod ile:resztaKomend) = case czyPodniesiony of
    True  -> rysujLinie wspolrzedne (noweWspolrzedne wspolrzedne ile kat)
    False -> ""
    ++ interpretacjaKomend kat czyPodniesiony (noweWspolrzedne wspolrzedne ile kat) resztaKomend
interpretacjaKomend kat czyPodniesiony wspolrzedne (Prawo nowyKat:resztaKomend) = 
    interpretacjaKomend ((kat + nowyKat) `mod` 360) czyPodniesiony wspolrzedne resztaKomend
interpretacjaKomend kat czyPodniesiony wspolrzedne (Opusc:resztaKomend) = 
    interpretacjaKomend kat True wspolrzedne resztaKomend
interpretacjaKomend kat czyPodniesiony wspolrzedne (Podnies:resztaKomend) =
    interpretacjaKomend kat False wspolrzedne resztaKomend
interpretacjaKomend _ _ _ [] = ""

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