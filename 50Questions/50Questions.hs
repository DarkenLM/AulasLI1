{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
import Prelude hiding (lookup, unlines, unwords, replicate, zip, drop, take, reverse, (!!), (++), enumFromThenTo, concat)
import Control.Arrow

-- 1
enumFromTo :: Int -> Int -> [Int]
enumFromTo i e = [i..e]

-- 2
enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo i s l = i : enumFromThenTo (i + s) s (l - 1)

-- 3
(++) :: [a] -> [a] -> [a]
(++) [] [] = []
(++) a [] = a
(++) [] a = a
(++) (ha:ta) b = ha : ta ++ b

-- 4
(!!) :: [a] -> Int -> a
(!!) l 0 = head l
(!!) l i = tail l !! (i - 1)

-- 5
reverse :: [a] -> [a]
reverse [] = []
reverse (h:t) = reverse t ++ [h]

-- 6
take :: Int -> [a] -> [a]
take 0 l = []
take 1 l = [head l]
take n (h:t) = h : take (n - 1) t

-- 7
drop :: Int -> [a] -> [a]
drop 0 l = l
drop n l = drop (n - 1) (tail l)

-- 8
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip (ha:ta) (hb:tb) = (ha, hb) : zip ta tb

-- 9
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : replicate (n - 1) a

-- 10
intersperse :: a -> [a] -> [a]
intersperse a [] = []
intersperse a (h:t)
    | null t = [h]
    | otherwise = [h, a] ++ intersperse a t

-- 11
group [] = []
group (x:xs) = group_loop [x] x xs
    where
        group_loop s m [] = [s]
        group_loop s m (y:ys)
            | y == m    = group_loop (s ++ [y]) m ys
            | otherwise = s : group_loop [y] y ys

-- groupElems :: Eq a => [a] -> a -> [a] -> Int -> ([a], Int)
-- groupElems l2 cm s i
--     | null l2 = (s, i)
--     | head l2 == cm = groupElems (tail l2) cm (s ++ [head l2]) (i + 1)
--     | otherwise = (s, i)

-- groupAll :: Eq a => [a] -> a -> [[a]] -> [[a]]
-- groupAll l2 cm mS
--     | null l2 = mS
--     | otherwise = do
--         let (s, i) = groupElems l2 cm [] 0
--             nl2 = take 2 $ drop 1 $ l2

--         if null nl2 then
--             mS ++ [s]
--         else
--             groupAll nl2 (head nl2) (mS ++ [s])

-- group :: Eq a => [a] -> [[a]]
-- group [] = []
-- group l = gl where gl = groupAll l (head l) []

-- 12
concat :: [[a]] -> [a]
concat = concat' []
    where
        concat' acc [] = acc
        concat' acc (h:t) = acc ++ h ++ concat' acc t

-- 13
inits :: [a] -> [[a]]
inits [] = []
inits l = inits (init l) ++ [l]

-- inits :: [a] -> [[a]]
-- inits a = reverse (take (length a + 1) (reverse (inits' [[]] a)))
--     where
--         inits' :: [[a]] -> [a] -> [[a]]
--         inits' acc [] = []
--         inits' acc (h:t) = (acc ++ [last acc ++ [h]]) ++ inits' (acc ++ [last acc ++ [h]]) t

-- 14

tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

-- 15
heads :: [[a]] -> [a]
heads [] = []
heads (h:t) = if null h then heads t else head h : heads t

-- 16
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

-- 17
fun :: [(a,b,c)] -> [(a,c)]
fun = map(\(a,_,c) -> (a,c))

-- 18
cola :: [(String,b,c)] -> String
--cola l = foldr (((++)) . (\(s,_,_) -> s)) [] l
cola [] = []
cola ((a,_,_):t) = a ++ cola t

-- 19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade y a l = map fst (filter(\(_,by) -> (y - by) >= a) l)

-- 20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = pEF [] 0 n m
    where
        pEF :: [Int] -> Int -> Int -> Int -> [Int]
        pEF acc c n m | c == m = acc
                      | otherwise = acc ++ [n ^ c] ++ pEF acc (c+1) n m

-- 21
isPrime :: Int -> Bool
isPrime n | n >= 2 = checkPrime n 2
          | otherwise = False
            where
                checkPrime :: Int -> Int -> Bool
                checkPrime n m | m * m > n = True
                               | mod n m == 0 = False
                               | otherwise = checkPrime n (m + 1)

-- 22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h2:t2) = h == h2 && isPrefixOf t t2

-- 23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l l2@(_:t) = l == l2 || isSuffixOf l t

-- 24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf s [] = False
isSubsequenceOf s@(h:t) (lh:lt) | h == lh = isSubsequenceOf t lt || isSubsequenceOf s lt
                                | otherwise = isSubsequenceOf s lt

-- 25
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices e l = getIndices [] 0 e l
    where
        getIndices acc i _ [] = acc
        getIndices acc i e (h:t) | h == e = getIndices (acc ++ [i]) (i+1) e t
                                 | otherwise = getIndices acc (i+1) e t

-- 26
nub :: Eq a => [a] -> [a]
nub [] = []
nub l = remDupes [] l
    where
        remDupes :: Eq a => [a] -> [a] -> [a]
        remDupes acc [] = acc
        remDupes acc (h:t) | elem h acc = remDupes acc t
                           | otherwise = remDupes (acc ++ [h]) t

-- 27
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete e l = deleter [] e l
    where
        deleter acc _ [] = acc
        deleter acc e (h:t) | h == e = acc ++ t
                            | otherwise = deleter (acc ++ [h]) e t

-- 28
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l [] = l
(\\) l (h:t) = (\\) (delete h l) t

-- 29
union :: Eq a => [a] -> [a] -> [a]
union [] _ = []
union l [] = l
union l (eh:et) | elem eh l = union l et
                | otherwise = union (l ++ [eh]) et

-- 30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (lh:lt) e | elem lh e = lh : intersect lt e
                    | otherwise = intersect lt e

-- 31
insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e (lh:lt) | e > lh = lh : insert e lt
                 | otherwise = e : lh : lt

-- 32
unwords :: [String] -> String
unwords [] = []
unwords (h:t) = h ++ (if null t then "" else " ") ++ unwords t

-- 33
unlines :: [String] -> String
unlines [] = []
unlines (h:t) = h ++ "\n" ++ unlines t

-- 34
pMaior :: Ord a => [a] -> Int
pMaior l@(h:_) = iter (h,0) 0 l
    where
        iter :: Ord a => (a,Int) -> Int -> [a] -> Int
        iter (_,li) _ [] = li
        iter (e,li) i (h:t) | e > h = iter (e,li) (i+1) t
                            | otherwise = iter (h,i) (i+1) t

-- 35
lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ [] = Nothing
lookup e ((h1,h2):t) | h1 == e = Just h2
                     | otherwise = lookup e t

-- 36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t)
    | s >= h = h : preCrescente (s:t)
    | otherwise = [h]

-- 37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) =
    let smallerSorted = iSort (filter (<= h) t)-- [a | a <- t, a <= h]
        biggerSorted = iSort (filter (> h) t)-- [a | a <- t, a > h]
    in  smallerSorted ++ [h] ++ biggerSorted

-- 38
menor :: String -> String -> Bool
menor "" _ = True
menor _ "" = False
menor (h1:t1) (h2:t2) | h1 < h2 = True
                      | h1 == h2 = menor t1 t2
                      | otherwise = False

-- 39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet e = any (\(a,_) -> a == e)

-- 40
converteMSet :: [(a, Int)] -> [a]
converteMSet l = foldr (++) [] (map (\(a,c) -> take c (repeat a)) l)
-- converteMSet l = join [] (map (\(a,c) -> take c (repeat a)) l)
--     where
--         join acc [] = acc
--         join acc (h:t) = join (acc ++ h) t

-- 41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet e l | any (\(a,_) -> a == e) l = map (\(a,c) -> if a == e then (a,c+1) else (a,c)) l
               | otherwise = l ++ [(e,1)]

-- 42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
--removeMSet e = filter (\(a,_) -> a /= e)
removeMSet _ [] = []
removeMSet e l = remover [] False e l
    where
        remover :: Eq a => [(a,Int)] -> Bool -> a -> [(a,Int)] -> [(a,Int)]
        remover acc _ _ [] = acc
        remover acc rem e l@(h@(ha,hc):t) | rem = acc ++ l
                                      | ha == e = remover acc True e t
                                      | otherwise = remover (acc ++ [h]) False e t

-- 43 
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet l = map (\x -> (head x, length x)) (group l)

-- 44
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a : ha,ta)
    where (ha,ta) = partitionEithers t
partitionEithers ((Right b):t) = (hb,b : tb)
    where (hb,tb) = partitionEithers t

-- 45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of
    Just a -> a : catMaybes t
    Nothing -> catMaybes t

-- 46
data Movimento = Norte | Sul | Este | Oeste deriving Show
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xp1, yp1) (xp2, yp2) =
    let (x,y) = (xp2-xp1,yp2-yp1)
    in replicate (abs x) (if x > 0 then Este else Oeste) ++ replicate (abs y) (if y > 0 then Norte else Sul)

-- 47
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops p m = pather p p m
    where
        pather i p [] = False
        pather i (x,y) (mh:mt) | posicao (x,y) mh == i = True
                               | otherwise = pather i (posicao (x,y) mh) mt
            where
                posicao (x, y) m = case m of
                    Norte -> (x, y+1)
                    Sul -> (x, y-1)
                    Este -> (x+1, y)
                    Oeste -> (x-1, y)

-- 48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados l = length (filter(\(Rect (x1,y1) (x2,y2)) -> y2-y1 == x2-x1) l)

-- 49
areaTotal :: [Rectangulo] -> Float
areaTotal = foldr ((+) . (\(Rect (x1,y1) (x2,y2)) -> abs (x2-x1) * abs (y2-y1))) 0

-- 50
data Equipamento = Bom | Razoavel | Avariado deriving Show
naoReparar :: [Equipamento] -> Int
naoReparar l = length (filter (\e -> case e of
  Bom -> True
  Razoavel -> True
  Avariado -> False) l)