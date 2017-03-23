module Komenda where 

data Komenda = Naprzod Int
             | Prawo Int 
             | Opusc
             | Podnies
             | Czysc
             | BedzieCzyszczenie
             | KoniecCzyszczenia
             deriving Show
