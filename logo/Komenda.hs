module Komenda where 

data Komenda = Naprzod Int
             | Prawo Int 
             | Lewo Int
             | Opusc
             | Podnies
             | Czysc
             | BedzieCzyszczenie
             | KoniecCzyszczenia
             deriving Show
