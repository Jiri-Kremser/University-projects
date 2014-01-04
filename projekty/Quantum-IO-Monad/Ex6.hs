-- G53NSC - Non-Standard Computation
-- excercise 6
-- author: Jiri Kremser

module Ex6 where
import Ex5
--import Ex4
import Ex3
--import Ex2
import QIO.Qio
import QIO.QioSyn
import Data.Monoid


-- 1)
search4 :: (Bool, Bool) -> Bool
search4 (False, False) = True
search4 _ = False

search8 :: (Bool, Bool, Bool) -> Bool
search8 (False, True, True) = True
search8 _ = False

search16 :: (Bool, Bool, Bool, Bool) -> Bool
search16 (True, False, True, False) = True
search16 _ = False

cond2 :: (Qbit, Qbit) -> ((Bool, Bool) -> U) -> U
cond2 (q0, q1) f = cond q0 (\x -> cond q1 (\y -> f (x ,y)))

cond4 :: (Qbit, Qbit, Qbit, Qbit) -> ((Bool, Bool, Bool, Bool) -> U) -> U
--cond4 (q0, q1, q2, q3)

cond4 (q0, q1, q2, q3) f = cond q0 (\x -> cond q1 (\y -> cond q2 (\z -> cond q3 (\z' -> f (x , y, z, z' )))))

minusQ = superQ True
plusQ = superQ False

-- y = minusQ
v4 :: (Qbit, Qbit) -> Qbit -> ((Bool, Bool) -> Bool) -> U
v4 x y f = cond2 x (\x' -> if (f x') then (uX y) else mempty)
--mozna se to ma aplikovat na ty draty kde je x, a ne na y
--
--

--v4 :: (Qbit, Qbit) -> ((Bool, Bool) -> Bool) -> U
--v4 x@(x0,x1) f = cond2 x (\x' -> if (f x') then (unot x0 `mappend` unot x1) else mempty)

v8 :: (Qbit, Qbit, Qbit) -> Qbit -> ((Bool, Bool, Bool) -> Bool) -> U
v8 x y f = cond3 x (\x' -> if (f x') then (uX y) else mempty)
 
v16 :: (Qbit, Qbit, Qbit, Qbit) -> Qbit -> ((Bool, Bool, Bool, Bool) -> Bool) -> U
v16 x y f = cond4 x (\x' -> if (f x') then (unot y) else mempty)

isFalse4 :: (Bool, Bool) -> Bool
isFalse4 (b0, b1) = b0 && b1

isFalse8 :: (Bool, Bool, Bool) -> Bool
isFalse8 (b0, b1, b2) = b0 && b1 && b2

isFalse16 :: (Bool, Bool, Bool, Bool) -> Bool
isFalse16 (b0, b1, b2, b3) = b0 && b1 && b2 && b3


w'4 :: (Qbit, Qbit) -> Qbit -> U
w'4 x y = v4 x y isFalse4

w'8 :: (Qbit, Qbit, Qbit) -> Qbit -> U
w'8 x y = v8 x y isFalse8

w'16 :: (Qbit, Qbit, Qbit, Qbit) -> Qbit -> U
w'16 x y = v16 x y isFalse16

w4 :: (Qbit, Qbit) -> Qbit -> U
w4 x@(x0,x1) y = hadamard x0 `mappend` hadamard x1 `mappend` w'4 x y `mappend` hadamard x1 `mappend` hadamard x0

w8 :: (Qbit, Qbit, Qbit) -> Qbit -> U
w8 x@(x0,x1,x2) y = hadamard x0 `mappend` hadamard x1 `mappend` hadamard x2 `mappend` w'8 x y `mappend` hadamard x2 `mappend` hadamard x1 `mappend` hadamard x0

w16 :: (Qbit, Qbit, Qbit, Qbit) -> Qbit -> U
w16 x@(x0,x1,x2,x3) y = hadamard x0 `mappend` hadamard x1 `mappend` hadamard x2 `mappend` hadamard x3 `mappend` w'16 x y `mappend` hadamard x3 `mappend` hadamard x2 `mappend` hadamard x1 `mappend` hadamard x0

theta :: Int -> Float
theta n = asin (1/sqrt (fromIntegral n))

gIterationsNum :: Float -> Int
gIterationsNum theta = round ((pi/(theta*4)) - (1/2))

grover4 :: (Qbit, Qbit) -> Qbit -> ((Bool, Bool) -> Bool) -> U
grover4 x y f = grover4' x y f ((gIterationsNum.theta) 4)
	where	grover4' x y f 0 = mempty
		grover4' x y f n = v4 x y f `mappend` w4 x y `mappend` grover4' x y f (n-1)

grover8 :: (Qbit, Qbit, Qbit) -> Qbit -> ((Bool, Bool, Bool) -> Bool) -> U
grover8 x y f = grover8' x y f ((gIterationsNum.theta) 8)
        where   grover8' x y f 0 = mempty
                grover8' x y f n = v8 x y f `mappend` w8 x y `mappend` grover8' x y f (n-1)

grover16 :: (Qbit, Qbit, Qbit, Qbit) -> Qbit -> ((Bool, Bool, Bool, Bool) -> Bool) -> U
grover16 x y f = grover16' x y f ((gIterationsNum.theta) 16)
        where   grover16' x y f 1 = mempty
                grover16' x y f n = v16 x y f `mappend` w16 x y `mappend` grover16' x y f (n-1)

groverTest = do	x0 <- plusQ
		x1 <- plusQ
		y <- minusQ
		applyU(grover4 (x0, x1) y search4)
		b0 <- measQbit x0
		b1 <- measQbit x1
--		b2 <- measQbit y
		return (b0,b1)
--		return b2

groverTest2 = do	x0 <- plusQ
			x1 <- plusQ
			x2 <- plusQ
			x3 <- plusQ
			y <- minusQ
			applyU(grover16 (x0, x1, x2, x3) y search16)
			b0 <- measQbit x0
			b1 <- measQbit x1
			b2 <- measQbit x2
			b3 <- measQbit x3
			return (b0,b1,b2,b3)

groverTest3 = do	x0 <- plusQ
			x1 <- plusQ
			x2 <- plusQ
			y <- minusQ
			applyU(grover8 (x0, x1, x2) y search8)
			b0 <- measQbit x0
			b1 <- measQbit x1
			b2 <- measQbit x2
			return (b0,b1,b2)


tester = do	x0 <- mkQbit False
		x1 <- mkQbit False
		y <- minusQ
		applyU(w'4 (x0, x1) y)
--		b0 <- measQbit x0
--		b1 <- measQbit x1
		b4 <- measQbit y
--		return (b0,b1)
		return b4

-- 2)

-- 3)

