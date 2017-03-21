module GenerujSvg (generujSvg) where
import KomendaRysowania

generujSvg :: [Komenda] -> Int -> Int -> IO ()
generujSvg lista x y = writeFile "wynik.html" ((svgNaglowek x y) ++ (svgSrodek lista) ++ svgStopka)

svgSrodek :: [Komenda] -> String
svgSrodek ((Kolko x y r):kx) = "        <circle cx=\"" ++ (show x) 
    ++ "\" cy=\"" ++ (show y) 
    ++ "\" r=\"" ++ (show r) 
    ++ "\" stroke=\"black\" stroke-width=\"4\" fill=\"white\"  />\n" 
    ++ (svgSrodek kx)
svgSrodek ((Kreska x1 y1 x2 y2):kx) = "         <line x1=\"" ++ (show x1) 
    ++ "\" y1=\"" ++ (show y1) 
    ++ "\" x2=\"" ++ (show x2)
    ++ "\" y2=\"" ++ (show y2)
    ++ "\" stroke=\"black\" stroke-width=\"4\" />\n" 
    ++ (svgSrodek kx)
svgSrodek (k:kx) = svgSrodek kx
svgSrodek _ = ""

svgNaglowek :: Int -> Int -> String
svgNaglowek x y = "<!doctype html> \n\
    \<html style=\"height:100%\">\n\
    \   <head></head>\n\
    \   <body style=\"height:100%;margin:0;padding:" ++ (show y) ++ "px 0 0 " ++ (show x) ++ "px\">\n\
    \       <svg style=\"height:100%\">\n"

svgStopka = "           Sorry, your browser does not support inline SVG.\n\
    \       </svg>\n\
    \   </body>\n\
    \</html>"

kolor = "#FF3269"