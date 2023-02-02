# Aaaaa

aaaaaaaa



{% code title="GenerujSvg.hs" overflow="wrap" lineNumbers="true" %}
```haskell
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
```
{% endcode %}



## H1

{% embed url="https://www.youtube.com/watch?v=KL6XhiwrzM0" %}
My caption for the video
{% endembed %}
