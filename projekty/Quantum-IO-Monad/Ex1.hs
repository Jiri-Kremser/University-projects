-- G53NSC - Non-Standard Computation
-- excercise 1 
-- author: Jiri Kremser

module Ex1 where
import System.Random
import List

-- 1)
data Bit = O | I deriving (Show, Eq)

-- 2)
int2Bits :: Int -> [Bit]
int2Bits 0 = [O]
int2Bits 1 = [I]
int2Bits n = (if even n then O else I) : int2Bits (n `div` 2)

-- 3)
bits2Int :: [Bit] -> Int
bits2Int s = bits2Int' s 1
	where 	bits2Int' [] _ = 0
		bits2Int' (x:xs) n 
			| x == O = bits2Int' xs (2 * n)
			| x == I = n + bits2Int' xs (2 * n)


-- 4)
-- n = imput number
-- log2 n = number of binary expansion of n
--
-- Asuming the complexities of mod and div functions are in class O(2^log2 n), 
-- div operation is executed only once => 1 * O(2^log2 n) = O(2^log2 n)
-- mod operation is executed |_ n/2 _| - 1 times, where |_ x.yz _| = x => (|_ n/2 _| - 1) * O(2^log2 n) = O(n*2^log n)
-- (:) operation at the beginning has complexity O(1)
-- resulting complexity is then O(1) + O(2^log2 n) + O(n*2^log2 n) = O(2^log2 n) => member of EXPTIME
-- another improvment can be made by involving a variant of the sieve of Eratosthenes and try numbers only to sqrt(n)
-- If some number 'x' is not a factor of given 'n' then neither multiples of 'x' are not, so there is no need to test them.
-- If some number 'x' is a factor of given 'n', then also 'n' `div` 'x' is the factor of 'n'.
-- (`div` 2) could be implemented as binary shift using (bits2Int.tail.int2Bits) for instance (but this won't change the complexity class)

--basic version

factors1 :: (Integral a) => a -> [a]
factors1 n = n : [ x | x <- [ half,half-1..1 ], n `mod` x == 0 ] 
	where half = n `div` 2



--improved version
factors2 :: (Integral a) => a -> [a]
factors2 n = 1 : factors' [2..limit]
	where	limit = (floor.sqrt.fromIntegral) n
		factors' [] = [n]
		factors' (x:s) = if n `mod` x == 0 then (if x == z then x:factors' s else x:z:factors' s) else factors' (s \\ multiples)
			where	multiples = [x,2*x..limit]
				z = n `div` x

-- 5)
getString :: IO String
getString = do 
	c <- getChar
	if c=='\n' then return []
		else do cs <- getString
			return (c:cs)

putString :: String -> IO ()
putString [] = putChar '\n'
putString (x:xs) = do
	putChar x
	putString xs
		
-- 6)
echoString :: IO ()
echoString = do
	s <- getString
	putString s

coprimes :: (Integral a) => a -> [a]
coprimes n = coprimes' n [2..]
	where coprimes' n (x:xs) = if length (factors2 n `intersect` factors2 x) > 1 then coprimes' n xs else x : coprimes' n xs

-- x^y % z ( O(log n) )
modulo :: (Integral t, Integral a) => a -> t -> a -> a
modulo _ 0 _ = 1
modulo x y z = (if (odd y) then x * modulo ((x * x) `mod` z) (y `div` 2) z else modulo ((x * x) `mod` z) (y `div` 2) z) `mod` z

-- Use of Fermat's little theorem
fermatEq :: (Integral a) => a -> a -> Int
fermatEq x y = if modulo x (y-1) y == 1 then 1 else 0

-- n 		tested integer number
-- attempts 	number of attempts (the greater value the higher probability)
-- maxRandom	maximum value which can random generator generates (the greater value the greater coprime _can_ be selected)
test' :: (Integral a) => a -> Int -> Int -> [Int] -> IO Bool
test' n attempts maxRandom s = do
	if (attempts <= 0) then (if (all (==1) s) then return True else return False) else do
		rand <- randomRIO (0::Int, maxRandom)
		let coprime = coprimes n !! rand in test' n (attempts - 1) maxRandom ((fermatEq coprime n) : s)

-- 7)		
isProbablyPrime :: Integer -> IO Bool
isProbablyPrime n = test' n 30 90 []

-- 8)
-- Pierre de Fermat
-- let 'p' be any prime and 'a' be any coprime of 'p' than a ^ (p - 1) = 1
