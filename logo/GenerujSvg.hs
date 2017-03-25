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

poczatkoweProcedury :: [Komenda]
poczatkoweProcedury = []

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
svgSrodek lista = interpretacjaKomend poczatkowyKat poczatkowyPodniesiony poczatkowyCzyJuzCzyscimy poczatkoweWspolrzedne poczatkowyKolor poczatkowaGrubosc poczatkoweProcedury (obslugaCzysc lista)

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

interpretacjaKomend :: Kat -> Podniesiony -> CzyJuzCzyscimy -> Wspolrzedne -> Int -> Int -> [Komenda] -> [Komenda] -> String
-- Naprzód, Wstecz
interpretacjaKomend kat czyPodniesiony czyCzyscimy wspolrzedne kolor grubosc g (Naprzod x:resztaKomend) = 
    let ile = read x in
        (if czyPodniesiony == False && czyCzyscimy == False   
            then 
                let kolor' = kodKoloruNaNazwe kolor 
                in rysujLinie wspolrzedne (noweWspolrzedne wspolrzedne ile kat) kolor' grubosc
            else "") 
        ++ interpretacjaKomend kat czyPodniesiony czyCzyscimy (noweWspolrzedne wspolrzedne ile kat) kolor grubosc g resztaKomend
interpretacjaKomend a b c d e f g (Wstecz x:resztaKomend) = 
    let ile = show $ -(read x)
    in  interpretacjaKomend a b c d e f g ((Naprzod ile) : resztaKomend)
-- -- Prawo, Lewo
interpretacjaKomend kat b c d e f g (Prawo x:resztaKomend) = 
    let nowyKat = read x 
    in  interpretacjaKomend ((kat + nowyKat) `mod` 360) b c d e f g resztaKomend
interpretacjaKomend kat b c d e f g (Lewo x:resztaKomend) = 
    let nowyKat = read x 
    in  interpretacjaKomend ((kat - nowyKat) `mod` 360) b c d e f g resztaKomend
-- Opuść, Podnieś
interpretacjaKomend a _ c d e f g (Opusc:resztaKomend) = 
    interpretacjaKomend a False c d e f g resztaKomend
interpretacjaKomend a _ c d e f g (Podnies:resztaKomend) =
    interpretacjaKomend a True c d e f g resztaKomend
-- Czyść
interpretacjaKomend a b _ d e f g (BedzieCzyszczenie:resztaKomend) = 
    interpretacjaKomend a b True d e f g resztaKomend
interpretacjaKomend a b _ d e f g (KoniecCzyszczenia:resztaKomend) = 
    interpretacjaKomend a b False d e f g resztaKomend
-- Kolor, Grubość
interpretacjaKomend a b c d _ f g (UstawKolorPisaka x:resztaKomend) = 
    let nrKoloru = read x
    in  interpretacjaKomend a b c d nrKoloru f g resztaKomend
interpretacjaKomend a b c d e _ g (UstawGruboscPisaka x:resztaKomend) =
    let grubosc = read x
    in  interpretacjaKomend a b c d e grubosc g resztaKomend
-- Powtórz
interpretacjaKomend a b c d e f g (Powtorz x lista:resztaKomend) = 
    let ile = read x in
        if ile > 0
            then interpretacjaKomend a b c d e f g (lista ++ [Powtorz (show(ile-1)) lista] ++ resztaKomend)
            else interpretacjaKomend a b c d e f g resztaKomend
-- Procedura
interpretacjaKomend a b c d e f listaProcedur (Procedura nazwa argumenty instrukcje:resztaKomend) = 
    if (nazwa `elem` (map (\x -> case x of (Procedura nazwa' _ _ ) -> nazwa') listaProcedur)) == True
        then
            interpretacjaKomend a b c d e f listaProcedur resztaKomend
        else
            interpretacjaKomend a b c d e f ((Procedura nazwa argumenty instrukcje):listaProcedur) resztaKomend
-- Wywolanie procedury
interpretacjaKomend a b c d e f listaProcedur (WywolanieProcedury nazwa argumenty:resztaKomend) =
    let procedura = filter (\x -> case x of 
                                    (Procedura nazwa' _ _) -> nazwa' == nazwa
                                    _ -> False) listaProcedur
    in
        if null procedura == True 
            then interpretacjaKomend a b c d e f listaProcedur resztaKomend
            else
                let rozpleconeKomendyProcedury = wplecArgumentyDoKomend (head procedura) argumenty
                in interpretacjaKomend a b c d e f listaProcedur (rozpleconeKomendyProcedury ++ resztaKomend)
-- Koniec
interpretacjaKomend _ _ _ _ _ _ _ [] = ""

wplecArgumentyDoKomend :: Komenda -> [String] -> [Komenda]
wplecArgumentyDoKomend (Procedura _ argumenty komendy) wartosci = wplecArgumentyDoKomend' (zip argumenty wartosci) komendy
wplecArgumentyDoKomend _ _ = []

wplecArgumentyDoKomend' :: [(String,String)] -> [Komenda] -> [Komenda]
wplecArgumentyDoKomend' argumentyOrazWartosci (komenda:resztaKomend) = 
    (wplecWartosciDoKomendy komenda argumentyOrazWartosci) : wplecArgumentyDoKomend' argumentyOrazWartosci resztaKomend
wplecArgumentyDoKomend' _ [] = []

wplecWartosciDoKomendy :: Komenda -> [(String,String)] -> Komenda
wplecWartosciDoKomendy (Naprzod ile) lista = (Naprzod $ zastapWartoscCzymsZListy ile lista)
wplecWartosciDoKomendy (Wstecz ile) lista = (Wstecz $ zastapWartoscCzymsZListy ile lista)
wplecWartosciDoKomendy (Prawo kat) lista = (Prawo $ zastapWartoscCzymsZListy kat lista)
wplecWartosciDoKomendy (Lewo kat) lista = (Lewo $ zastapWartoscCzymsZListy kat lista)
wplecWartosciDoKomendy (UstawKolorPisaka nr) lista = (UstawKolorPisaka $ zastapWartoscCzymsZListy nr lista)
wplecWartosciDoKomendy (UstawGruboscPisaka nr) lista = (UstawGruboscPisaka $ zastapWartoscCzymsZListy nr lista)
wplecWartosciDoKomendy (Powtorz ile komendy) lista = (Powtorz (zastapWartoscCzymsZListy ile lista) (wplecArgumentyDoKomend' lista komendy))
wplecWartosciDoKomendy komendaBezArg _ = komendaBezArg

zastapWartoscCzymsZListy :: String -> [(String,String)] -> String
zastapWartoscCzymsZListy argument ((key,value):resztaListy) = 
    if argument == key
        then value
        else zastapWartoscCzymsZListy argument resztaListy
zastapWartoscCzymsZListy argument [] = argument

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