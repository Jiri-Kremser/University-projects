-- G53NSC - Non-Standard Computation
-- excercise 3 
-- author: Jiri Kremser

module Ex3 where
import Ex2
import QIO.Qio
import QIO.QioSyn
import QIO.QioClass
import Data.Monoid
import Complex

data QubitState = QS CC CC  deriving (Show)
data PolarCoordinates = PC RR RR deriving (Show)

-- 3)
-- toPolar qu0 = PC 0.0 NaN (phi can be any value from [0,2pi])
-- toPolar qu1 = PC 3.1415927 0.0 (phi \in [0,2pi])
-- toPolar quMinus = PC 1.5707961 3.1415927
-- toPolar quPlus = PC 1.5707961 0.0
qu0 :: QubitState
qu0 = (QS 1 0)

qu1 :: QubitState
qu1 = (QS 0 1)

quPlus :: QubitState
quPlus = (QS (1/sq) (1/sq)) where sq = sqrt 2

quMinus :: QubitState
quMinus = (QS (1/sq) (-1/sq)) where sq = sqrt 2

-- 1)
toPolar :: QubitState -> PolarCoordinates
toPolar (QS 1 _) = PC 0 0
toPolar (QS a b) = PC (realPart theta) ((imagPart.log) (b / (sin (theta / 2))))
	where theta = 2*(acos a)

-- 2)
toQubit :: PolarCoordinates -> QubitState
toQubit (PC theta phi) = QS (cos (theta/2) :+ 0) ((realPart$ ((exp ((phi:+0) * (0:+1))) * ((sin ((theta)/2)):+0))) :+0)

-- 4)
hdmrdRot :: Rotation
hdmrdRot (a, b)
	| a && b = -x
	| otherwise = x
	where x =  1 / (sqrt 2)

hadamard :: Qbit -> U
hadamard q = rot q hdmrdRot


-- 5)
plus :: QIO Qbit
plus = do 	q <- mkQbit False
		applyU $ rot q hdmrdRot
		return q

-- 6)
minus :: QIO Qbit
minus = do	q <- mkQbit True
		applyU $ rot q hdmrdRot
		return q

-- 7)
-- All 4 states have the same probability to occur
-- ( because [1/sqrt(2)]^2 == [-1/sqrt(2)]^2 )
plusMinus :: QIO (Bool, Bool)
plusMinus = do	pl <- plus
		min <- minus
		a <- measQbit pl
		b <- measQbit min
		return (a,b)

-- 8)
uX :: Qbit -> U
uX = unot

uY :: Qbit -> U
uY q = rot q rotY
	where	rotY (False, True) = 0 :+ (-1)
		rotY (True, False) = 0 :+ 1
		rotY _ = 0

uZ :: Qbit -> U
uZ q = rot q rotZ
	where	rotZ (True, True) = 1
		rotZ (False, False) = (-1)
		rotZ _ = 0

-- 9)
composedUnot :: Bool -> QIO Bool
composedUnot b = do	q <- mkQbit b
			applyU $ (hadamard `mappend` uZ `mappend` hadamard) q
			val <- measQbit q
			return val

-- 10) It is the unitary not
--
-- let a = 1 / sqrt(2)
--     H = matrix for Hadamard rotation
--     uZ = matrix for Pauli rotation (axis Z)
--
--               [a  a]   [1  0]     [a  a]     [a -a]   [a  a]     [0  1]
-- H . uZ . H = ([a -a] . [0 -1])  . [a -a]  =  [a  a] . [a -a]  =  [1  0] = unot = uX
--
