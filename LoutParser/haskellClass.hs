-- import Prelude



-- change :: Int -> [Int]
-- change = map snd . newSub coins 
--   where coins = [ 100 , 50 , 25 , 10 , 5 , 1 ]

-- newSub :: (Ord t, Num t) => [t] -> t -> [(t, t)]
-- newSub oriValues = newSub' oriValues 
--   where
--     newSub' [] _ = []
--     newSub' (y:ys) x | x >= y    = (newAmount,y) : newSub oriValues newAmount
--                      | otherwise = newSub' ys x 
--       where newAmount = x - y




-- change' :: Int -> [Int]
-- change' = map snd . newSub coins 
--   where coins = [ 100 , 50 , 25 , 10 , 5 , 1 ]
--         newSub [] _ = []
--         newSub (y:ys) x | x >= y    = (newAmount,y) : newSub coins newAmount
--                         | otherwise = newSub ys x 
--           where newAmount = x - y

-- -- change'' (c:cs) a = if a - c >= 0 then c : (change'' (c:cs) (a-c)) else change'' cs a
-- -- change''' = fix ((`ap` tail) . (. head) . (flip =<< (liftM2 ap .) . ap ((.) . ap . (if' .) '. flip flip 0 . ((>=) .) . subtract) . ap ((.) . (.) . (:)) . (`ap` subtract) . ((flip . ((.) .)) .) . (. (:)) . (.)))



{-# LANGUAGE TupleSections #-}

module FCT 

where 

import Data.List

import Text.PrettyPrint


-- foo :: [a] -> [b -> [(a,b)]]
ppThis :: Show a => [a] -> IO()
ppThis = putStrLn . render . vcat . map (text.show) 


testAnswer :: [Int->Int] 
testAnswer = list2fct <$> result
createTests :: [[Int] -> Bool]
createTests = [injective] <*> testAnswer
testAll = ppThis $ createTests <*> [domain]
disResult = ppThis result

domain,codomain :: [Int]
domain = [1..3] 
codomain = [3..6]
result = injs domain codomain 

diffSecondValue xs = length (nub $ map snd xs) == length xs
injs xs ys = filter diffSecondValue . cartprod $ [[(x,y)|y<-ys]|x<-xs]
-- I stole this =(
cartprod :: [[a]] -> [[a]];
cartprod [] = [[]]
cartprod (xs:xss) = [x:ys | x<- xs, ys <-yss]
  where yss = cartprod xss


fun fs = (foldr . id fs) 

-- foo = map (,) 
-- meh = buzz (foo domain) codomain
-- buzz [] _ = []
-- buzz _ [] = []
-- buzz (f:fs) l = map f l : buzz fs l



f x = x^2 + 1

list2fct :: Eq a => [(a,b)] -> a -> b
list2fct [] _ = error "function not total"
list2fct ((u,v):uvs) x | x == u    = v
                       | otherwise = list2fct uvs x

fct2list :: (a -> b) -> [a] -> [(a,b)]
fct2list f xs = [ (x, f x) | x <- xs ]

ranPairs :: Eq b => [(a,b)] -> [b]
ranPairs f = nub [ y | (_,y) <- f ]

listValues  :: Enum a => (a -> b) -> a -> [b]
listValues f i = (f i) : listValues f (succ i) 

listRange :: (Bounded a, Enum a) => (a -> b) -> [b]
listRange f = [ f i | i <- [minBound..maxBound] ]

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z) 

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d 
uncurry3 f (x,y,z) = f x y z 

f1 x = x^2 + 2 * x + 1
g1 x = (x + 1)^2
f1' = \x -> x^2 + 2 * x + 1
g1' = \x -> (x + 1)^2

g 0 = 0 
g n = g (n-1) + n 

g' n = ((n + 1) * n ) / 2 

h 0 = 0 
h n = h (n-1) + (2*n)

k 0 = 0 
k n = k (n-1) + (2*n-1)

fac 0 = 1 
fac n = fac (n-1) * n

fac' n = product [1..n]

restrict :: Eq a => (a -> b) -> [a] -> a -> b 
restrict f xs x | elem x xs = f x 
                | otherwise = error "argument not in domain"

restrictPairs :: Eq a => [(a,b)] -> [a] -> [(a,b)]
restrictPairs xys xs = [ (x,y) |  (x,y) <- xys, elem x xs ]

image :: Eq b => (a -> b) -> [a] -> [b] 
image f xs = nub [ f x | x <- xs ]

coImage :: Eq b => (a -> b) -> [a] -> [b] -> [a] 
coImage f xs ys = [ x | x <- xs, elem (f x) ys ] 

imagePairs :: (Eq a, Eq b) => [(a,b)] -> [a] -> [b] 
imagePairs f xs = nub [ y | (x,y) <- f, elem x xs]

coImagePairs :: (Eq a, Eq b) => [(a,b)] -> [b] -> [a] 
coImagePairs f ys = [ x | (x,y) <- f, elem y ys]




injective :: Eq b => (a -> b) -> [a] ->  Bool
injective f [] = True
injective f (x:xs) = 
   notElem (f x) (image f xs) && injective f xs

surjective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
surjective f xs [] = True
surjective f xs (y:ys) = 
   elem y (image f xs) && surjective f xs ys 

c2f, f2c :: Int -> Int
c2f x = div (9 * x) 5 + 32
f2c x = div (5 * (x - 32)) 9 

succ1 :: Integer -> Integer
succ1 = \ x -> if x < 0 
                  then error "argument out of range" 
                  else x+1

succ2 :: Integer -> [Integer]
succ2 = \ x -> if x < 0 then [] else [x+1]

pcomp :: (b -> [c]) -> (a -> [b]) -> a -> [c]
pcomp g f = \ x -> concat [ g y | y <- f x ]

succ3 :: Integer -> Maybe Integer
succ3 = \ x -> if x < 0 then Nothing else Just (x+1)

mcomp :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
mcomp g f = (maybe Nothing g) . f

part2error :: (a -> Maybe b) -> a -> b 
part2error f = (maybe (error "value undefined") id) . f

fct2equiv :: Eq a => (b -> a) -> b -> b -> Bool
fct2equiv f x y = (f x) == (f y)
