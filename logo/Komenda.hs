module Komenda where 

data Komenda = Naprzod Int
             | Wstecz Int
             | Prawo Int 
             | Lewo Int
             | Opusc
             | Podnies
             | Czysc
             | BedzieCzyszczenie
             | KoniecCzyszczenia
             | UstawKolorPisaka Int
             | UstawGruboscPisaka Int
             | Powtorz Int [Komenda]
             deriving Show
