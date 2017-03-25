module Komenda where 

data Komenda = Naprzod String
             | Wstecz String
             | Prawo String 
             | Lewo String
             | Opusc
             | Podnies
             | Czysc
             | BedzieCzyszczenie
             | KoniecCzyszczenia
             | UstawKolorPisaka String
             | UstawGruboscPisaka String
             | Powtorz String [Komenda]
             | Procedura String [String] [Komenda]
             | WywolanieProcedury String [String]
             deriving Show
