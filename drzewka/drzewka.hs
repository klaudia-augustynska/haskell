import Data.Char -- isLetter, chr

{-

drzewko [1]

      1
     / \
    2   5
   / \
  3   4
  
  
drzewko [2]

      /\
     /  \
    /\   5
   /  \
  3    4

drzewko [3]

         1
	    / \
       /   \
	  /     \
     2       5
    / \     / \
   /   \    * *
  3     4
 / \   / \
 * *   * *

-}

-- a) mamy 3 typy drzewiaste (były na tablicy)

data Tree1 a = Branch1 a (Tree1 a) (Tree1 a) | Leaf1 a deriving Show
data Tree2 a = Branch2 (Tree2 a) (Tree2 a) | Leaf2 a deriving Show
data Tree3 a = Branch3 a (Tree3 a) (Tree3 a) | Leaf3 deriving Show

-- b) piszemy dla nich funkcje:

-- największy element w drzewie
maxElement1 (Leaf1 a) = a
maxElement1 (Branch1 a left right) = max ( max a (maxElement1 left) ) (maxElement1 right)

maxElement2 (Leaf2 a) = a
maxElement2 (Branch2 left right) = max (maxElement2 left) (maxElement2 right)

maxElement3 Leaf3 = minBound :: Int
maxElement3 (Branch3 a left right) = max ( max a (maxElement3 left) ) (maxElement3 right)

-- suma elementów (+)
sumOfTree1 (Leaf1 a) = a
sumOfTree1 (Branch1 a left right) = a + (sumOfTree1 left) + (sumOfTree1 right)

sumOfTree2 (Leaf2 a) = a
sumOfTree2 (Branch2 left right) = (sumOfTree2 left) + (sumOfTree2 right)

sumOfTree3 Leaf3 = 0
sumOfTree3 (Branch3 a left right) = a + (sumOfTree3 left) + (sumOfTree3 right)

-- zamiana drzewa na listę ([a] lub Lista a)
treeToList1 (Leaf1 a) = [a]
treeToList1 (Branch1 a left right) = a : ((treeToList1 left) ++ (treeToList1 right))

treeToList2 (Leaf2 a) = [a]
treeToList2 (Branch2 left right) = (treeToList2 left) ++ (treeToList2 right)

treeToList3 Leaf3 = []
treeToList3 (Branch3 a left right) = a : ((treeToList3 left) ++ (treeToList3 right))

-- wdrzewiesamelitery (Drzewo String -> Bool)
onlyLettersInTree1 (Leaf1 a) = isLetter a
onlyLettersInTree1 (Branch1 a left right) = (isLetter a) && (onlyLettersInTree1 left) && (onlyLettersInTree1 right)

onlyLettersInTree2 (Leaf2 a) = isLetter a
onlyLettersInTree2 (Branch2 left right) = (onlyLettersInTree2 left) && (onlyLettersInTree2 right)

onlyLettersInTree3 Leaf3 = True
onlyLettersInTree3 (Branch3 a left right) = (isLetter a) && (onlyLettersInTree3 left) && (onlyLettersInTree3 right)

-- c) używając schematów

-- mapowania funkcji a->b na Drzewo a, żeby dostać Drzewo b

mapTree1 func (Leaf1 a) = Leaf1 (func a)
mapTree1 func (Branch1 a left right) = Branch1 (func a) (mapTree1 func left) (mapTree1 func right)

mapTree2 func (Leaf2 a) = Leaf2 (func a)
mapTree2 func (Branch2 left right) = Branch2 (mapTree2 func left) (mapTree2 func right)

mapTree3 _ Leaf3 = Leaf3
mapTree3 func (Branch3 a left right) = Branch3 (func a) (mapTree3 func left) (mapTree3 func right)

-- zwijania Drzewa a przy pomocy funkcji a->a->a do wielkości typu a

reduceTree1 _ (Leaf1 a) = a
reduceTree1 func (Branch1 a left right) = func ( func a (reduceTree1 func left) ) (reduceTree1 func right)

reduceTree2 _ (Leaf2 a) = a
reduceTree2 func (Branch2 left right) = func (reduceTree2 func left) (reduceTree2 func right)

reduceTree3 _ (Branch3 a Leaf3 Leaf3) = a
reduceTree3 func (Branch3 a left Leaf3) = func a (reduceTree3 func left)
reduceTree3 func (Branch3 a Leaf3 right) = func a (reduceTree3 func right)
reduceTree3 func (Branch3 a left right) = func ( func a (reduceTree3 func left) ) (reduceTree3 func right)

-- 4. Dla dociekliwych:
-- proszę sprawić, aby typ Natu zaczął należeć do klasy Ord i Eq.
-- (instance Ord Natu where ....)
-- ..żeby można było dla Drzew Natu użyć funkcji szukającej największy
-- element w drzewie j.w..

data Natu = Zero | S Natu deriving Show
instance Eq Natu where
   Zero == Zero = True
   (S _) == Zero = False
   Zero == (S _) = False
   (S x) == (S y) = x == y
   
instance Ord Natu where
   Zero <= Zero = True
   Zero <= (S _) = True
   (S _) <= Zero = False
   (S x) <= (S y) = x <= y

-- a może uda się również zdefiniować instance Num Natu żeby dodawać
-- nasze Natu operatorem + ?

instance Num Natu where
   x + Zero = x
   x + (S y) = S(x + y)

-- I jeszcze zadanie dla osób chcących zdobyć tzw. Duży Plus :)
-- Zaproponować typ drzew niebinarnych - dowolnie rozgałęzionych
--    data DrzewoNB a = ...

data NBTree a = NBBranch a [NBTree a] deriving Show

-- i dla takich drzew schematy mapowania i zwijania

mapNBTree func (NBBranch a []) = NBBranch (func a) []
mapNBTree func (NBBranch a list) = NBBranch (func a) (map (\l -> mapNBTree func l) list)

reduceNBTree func (NBBranch a []) = a
reduceNBTree func (NBBranch a list) = foldl func a (map (\l -> reduceNBTree func l) list)

maxElement1' a = reduceTree1 max a
maxElement2' a = reduceTree2 max a
maxElement3' a = reduceTree3 max a
sumOfTree1' a = reduceTree1 (+) a
sumOfTree2' a = reduceTree2 (+) a
sumOfTree3' a = reduceTree3 (+) a
treeToList1' a = reduceTree1 (++) (mapTree1 (\x -> [x]) a)
treeToList2' a = reduceTree2 (++) (mapTree2 (\x -> [x]) a)
treeToList3' a = reduceTree3 (++) (mapTree3 (\x -> [x]) a)
onlyLettersInTree1' a = reduceTree1 (&&) (mapTree1 isLetter a) 
onlyLettersInTree2' a = reduceTree2 (&&) (mapTree2 isLetter a) 
onlyLettersInTree3' a = reduceTree3 (&&) (mapTree3 isLetter a) 

-- oraz przy pomocy tych schematów - wspominane poprzednio funkcje

maxElementNB a = reduceNBTree max a
sumOfNBTree a = reduceNBTree (+) a
nBTreeToList a = reduceNBTree (++) (mapNBTree (\x -> [x]) a)
onlyLettersInNBTree a = reduceNBTree (&&) (mapNBTree isLetter a) 

-- pytanie za extra 8 punktów: czy jest szansa na zdefiniowanie klasy
-- NaszeDrzewa z operatorami mapujDrzewo i zwijajDrzewo, do której należeć
-- będą wszystkie 4 nasze typy drzewiaste (poprzez odpowiednie
-- zdefiniowanie instancji.. np  instance NaszeDrzewa (DrzewoNB a) where...
-- )  ?

class NuestrosArboles t where
   mapTree :: (a -> b) -> t a -> t b
   reduceTree :: (a -> a -> a) -> t a -> a
   
instance NuestrosArboles Tree1 where
   mapTree func (Leaf1 a) = Leaf1 (func a)
   mapTree func (Branch1 a left right) = Branch1 (func a) (mapTree func left) (mapTree func right)

   reduceTree _ (Leaf1 a) = a
   reduceTree func (Branch1 a left right) = func ( func a (reduceTree func left) ) (reduceTree func right)

instance NuestrosArboles Tree2 where
   mapTree func (Leaf2 a) = Leaf2 (func a)
   mapTree func (Branch2 left right) = Branch2 (mapTree func left) (mapTree func right)

   reduceTree _ (Leaf2 a) = a
   reduceTree func (Branch2 left right) = func (reduceTree func left) (reduceTree func right)

instance NuestrosArboles Tree3 where
   mapTree _ Leaf3 = Leaf3
   mapTree func (Branch3 a left right) = Branch3 (func a) (mapTree func left) (mapTree func right)  

   reduceTree _ (Branch3 a Leaf3 Leaf3) = a
   reduceTree func (Branch3 a left Leaf3) = func a (reduceTree func left)
   reduceTree func (Branch3 a Leaf3 right) = func a (reduceTree func right)
   reduceTree func (Branch3 a left right) = func ( func a (reduceTree func left) ) (reduceTree func right)

maxTreeElement a = reduceTree max a
sumOfTree a = reduceTree (+) a
treeToList a = reduceTree (++) (mapTree (\x -> [x]) a)
onlyLettersInTree a = reduceTree (&&) (mapTree isLetter a) 

  
-------------------------------------------------------------------------------
--------------------------------- DEMO ----------------------------------------
-------------------------------------------------------------------------------

x = Branch1 1 ( Branch1 2 (Leaf1 3) (Leaf1 4) ) (Leaf1 5)
y = Branch1 'a' ( Branch1 'b' (Leaf1 'c') (Leaf1 'd') ) (Leaf1 'E')
z = Branch1 ',' ( Branch1 'b' (Leaf1 'c') (Leaf1 'd') ) (Leaf1 'E')

xx = Branch2 ( Branch2 (Leaf2 3) (Leaf2 4) ) (Leaf2 5)
yy = Branch2 ( Branch2 (Leaf2 'c') (Leaf2 'd') ) (Leaf2 'E')
zz = Branch2 ( Branch2 (Leaf2 ',') (Leaf2 'd') ) (Leaf2 'E')

xxx = Branch3 1 (Branch3 2 (Branch3 3 Leaf3 Leaf3) (Branch3 4 Leaf3 Leaf3)) (Branch3 5 Leaf3 Leaf3)
yyy = Branch3 'a' (Branch3 'b' (Branch3 'c' Leaf3 Leaf3) (Branch3 'd' Leaf3 Leaf3)) (Branch3 'e' Leaf3 Leaf3)
zzz = Branch3 ',' (Branch3 'b' (Branch3 'c' Leaf3 Leaf3) (Branch3 'd' Leaf3 Leaf3)) (Branch3 'e' Leaf3 Leaf3)

xNatu = Branch1 Zero ( Branch1 (S ( Zero )) (Leaf1 (S(S ( Zero )))) (Leaf1 (S(S(S ( Zero ))))) ) (Leaf1 (S(S(S(S ( Zero ))))))

xNB = NBBranch 1 [ (NBBranch 2 []) , (NBBranch 3 []), (NBBranch 4 []) , (NBBranch 5 []) ]
yNB = NBBranch 'a' [ (NBBranch 'b' []) , (NBBranch 'c' []), (NBBranch 'd' []) , (NBBranch 'e' []) ]
zNB = NBBranch ',' [ (NBBranch 'b' []) , (NBBranch 'c' []), (NBBranch 'd' []) , (NBBranch 'e' []) ]

main = do
          -- drzewo [1]
          putStrLn $ "Najwiekszy element drzewa X: " ++ (show $ maxElement1 $ x)
          putStrLn $ "Suma elementow drzewa X: " ++ (show $ sumOfTree1 $ x)
          putStrLn $ "Drzewo X jako lista: " ++ (show $ treeToList1 $ x)
          putStrLn $ "Drzewo Y czy same litery: " ++ (show $ onlyLettersInTree1 $ y)
          putStrLn $ "Drzewo Z czy same litery: " ++ (show $ onlyLettersInTree1 $ z)
          putStrLn $ "Drzewo X zmapowane na litery: " ++ (show $ mapTree1 (\a -> chr (a+64)) x )
          putStrLn $ "Drzewo X zwiniete do najmniejszego elementu: " ++ (show $ reduceTree1 min x )
          -- drzewo [2]
          putStrLn $ "Najwiekszy element drzewa XX: " ++ (show $ maxElement2 $ xx)
          putStrLn $ "Suma elementow drzewa XX: " ++ (show $ sumOfTree2 $ xx)
          putStrLn $ "Drzewo XX jako lista: " ++ (show $ treeToList2 $ xx)
          putStrLn $ "Drzewo YY czy same litery: " ++ (show $ onlyLettersInTree2 $ yy)
          putStrLn $ "Drzewo ZZ czy same litery: " ++ (show $ onlyLettersInTree2 $ zz)
          putStrLn $ "Drzewo XX zmapowane na litery: " ++ (show $ mapTree2 (\a -> chr (a+64)) xx )
          putStrLn $ "Drzewo XX zwiniete do najmniejszego elementu: " ++ (show $ reduceTree2 min xx )
          -- drzewo [3]
          putStrLn $ "Najwiekszy element drzewa XXX: " ++ (show $ maxElement3 $ xxx)
          putStrLn $ "Suma elementow drzewa XXX: " ++ (show $ sumOfTree3 $ xxx)
          putStrLn $ "Drzewo XXX jako lista: " ++ (show $ treeToList3 $ xxx)
          putStrLn $ "Drzewo YYY czy same litery: " ++ (show $ onlyLettersInTree3 $ yyy)
          putStrLn $ "Drzewo ZZZ czy same litery: " ++ (show $ onlyLettersInTree3 $ zzz)
          putStrLn $ "Drzewo XXX zmapowane na litery: " ++ (show $ mapTree3 (\a -> chr (a+64)) xxx )
          putStrLn $ "Drzewo XXX zwiniete do najmniejszego elementu: " ++ (show $ reduceTree3 min xxx )
          -- mapowanie i zwijanie
          putStrLn $ "Max X: " ++ (show $ maxElement1' $ x)
          putStrLn $ "Max XX: " ++ (show $ maxElement2' $ xx)
          putStrLn $ "Max XXX: " ++ (show $ maxElement3' $ xxx)
          putStrLn $ "Sum X: " ++ (show $ sumOfTree1' $ x)
          putStrLn $ "Sum XX: " ++ (show $ sumOfTree2' $ xx)
          putStrLn $ "Sum XXX: " ++ (show $ sumOfTree3' $ xxx)
          putStrLn $ "X jako lista: " ++ (show $ treeToList1' $ x)
          putStrLn $ "XX jako lista: " ++ (show $ treeToList2' $ xx)
          putStrLn $ "XX jako lista: " ++ (show $ treeToList3' $ xxx)
          putStrLn $ "Z czy same litery: " ++ (show $ onlyLettersInTree1' $ z)
          putStrLn $ "YY czy same litery: " ++ (show $ onlyLettersInTree2' $ yy)
          putStrLn $ "ZZZ czy same litery: " ++ (show $ onlyLettersInTree3' $ zzz)
          -- operacje na Natu
          putStrLn $ "Najwiekszy element w drzewie xNatu: " ++ (show $ reduceTree1 max xNatu)
          putStrLn $ show $ (S(S ( Zero ))) + (S(Zero))
          -- niebinarne drzewo
          putStrLn $ "Drzewo XNB zmapowane na litery: " ++ (show $ mapNBTree (\a -> chr (a+64)) xNB )
          putStrLn $ "Drzewo XNB zwiniete do najmniejszego elementu: " ++ (show $ reduceNBTree min xNB )
          putStrLn $ "Najwiekszy element drzewa XNB: " ++ (show $ maxElementNB xNB)
          putStrLn $ "Suma elementow drzewa XNB: " ++ (show $ sumOfNBTree $ xNB)
          putStrLn $ "Drzewo XNB jako lista: " ++ (show $ nBTreeToList $ xNB)
          putStrLn $ "Drzewo YNB czy same litery: " ++ (show $ onlyLettersInNBTree $ yNB)
          putStrLn $ "Drzewo ZNB czy same litery: " ++ (show $ onlyLettersInNBTree $ zNB)
          -- drzewo jako klasa typow
          putStrLn $ "Najwiekszy element X: " ++ (show $ maxTreeElement x)
          putStrLn $ "Suma elementow XX: " ++ (show $ sumOfTree xx)
          putStrLn $ "XXX jako lista: " ++ (show $ treeToList xxx)
          putStrLn $ "Z czy same litery: " ++ (show $ onlyLettersInTree z)

