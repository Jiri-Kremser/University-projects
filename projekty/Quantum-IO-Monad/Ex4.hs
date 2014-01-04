-- G53NSC - Non-Standard Computation
-- excercise 4
-- author: Jiri Kremser

module Ex4 where
import Ex3
import Ex2
import System.Random
import QIO.Qio
import QIO.QioSyn
--import QIO.QioClass
import Data.Monoid
import Complex


-- 1)
data Setting = A | B | C deriving Show
type Particle = (Bool, Bool, Bool)


randomSetting ::IO Setting 
randomSetting = do	rand <- randomRIO (0::Int, 2)
			return (if rand == 0 then A else (if rand == 1 then B else C))


bell :: QIO (Qbit, Qbit)
bell = do	q1 <- mkQbit False
		q2 <- mkQbit False
		applyU (hadamard q1)
		applyU (controlledX q1 q2)
		return (q1, q2)

measureBell :: QIO (Bool, Bool)
measureBell = do 	(q1, q2) <- bell
			b1 <- measQbit q1
			b2 <- measQbit q2
			return (b1, b2)

cSource :: IO (Particle, Particle)
cSource = do 	a <- randomIO
		b <- randomIO
		c <- randomIO
		return ((a, b, c), (a, b, c))


u120 :: Rotation
u120 (a, b) = if (a == b) then c else (if b then -s else s)
	where 	c = cos (pi/3)
		s = sin (pi/3)


measureAngle :: Setting -> Qbit -> U
measureAngle A q = mempty
measureAngle B q = rot q u120
measureAngle C q = (rot q u120) `mappend` (rot q u120)


qDetector :: Setting -> Qbit -> QIO Bool
qDetector s q = do	applyU (measureAngle s q)
			measQbit q

cDetector :: Setting -> Particle -> IO Bool
cDetector A (a, _, _) = return a
cDetector B (_, b, _) = return b
cDetector C (_, _, c) = return c


qSource :: QIO (Qbit, Qbit)
qSource = bell


testC :: Setting -> IO Bool
testC s = do 	(c1, c2) <- cSource
		b1 <- cDetector s c1
		b2 <- cDetector s c2
		return (b1 == b2)


testQ :: Setting -> QIO Bool
testQ s = do 	(q1, q2) <- qSource
		b1 <- qDetector s q1
		b2 <- qDetector s q2
		return (b1 == b2)

cExperiment :: (Setting, Setting ) -> IO Bool
cExperiment (s1, s2 ) = do 	(c1, c2) <- cSource
				b1 <- cDetector s1 c1
				b2 <- cDetector s2 c2
				return (b1 == b2)

qExperiment :: (Setting, Setting ) -> QIO Bool
qExperiment (s1, s2 ) = do 	(q1, q2) <- qSource
				b1 <- qDetector s1 q1
				b2 <- qDetector s2 q2
				return (b1 == b2)


experiment :: IO (Bool, Bool)
experiment = do 	s1 <- randomSetting
			s2 <- randomSetting
			c <- cExperiment (s1, s2 )
			q <- run (qExperiment (s1, s2 ))
			return (c, q)


-- 2)
experiments :: Int -> IO (Float, Float)
experiments n = do	(sum1, sum2) <- experiments' n (0, 0)
			return (sum1/ (fromIntegral n), sum2/ (fromIntegral n))
		where	experiments' 0 (sum1, sum2) = return (sum1, sum2)
			experiments' x (sum1, sum2) = do	(c, q) <- experiment
								experiments' (x-1) (sum1 + (if c then 1else 0), sum2 + (if q then 1else 0))

-- 3) There are no local hidden variables and locality doesn't hold.
--    Measurement of first particle from quantum source will affect the measurement 
--    of the second particle however large is the distance between them. They are correlated.
--
--result:
-- *Ex4> experiments 100000
-- (0.66953,0.50123)
--  quantum,classical
						

-- 4)
share :: Qbit -> QIO Qbit
share q = do	q' <- mkQbit False
		applyU (controlledX q q')
		return q'

ghz :: Int -> QIO [Qbit]
ghz n = if n < 3 then ghz 3 else 
	do	(q1,q2) <- bell
		ghz' n q1 q2
		where 	ghz' 2 q1 q2 = return (q1:q2:[])
			ghz' n q1 q2 = do	rest  <- ghz' (n-1) q1 q2
						q <- share q1
						return (q:rest)
							
measureGhz :: [Qbit] -> QIO [Bool]
measureGhz [] = return []
measureGhz (x:xs) = do	rest <- measureGhz xs
			bit <- measQbit x
			return (bit:rest)

testGhz :: Int -> QIO [Bool]
testGhz n = do	ghzQbits <- ghz n
		measureGhz ghzQbits


-- 5)
-- |psi> = alpha |0> + beta |1>
-- beta^2 = 1/10
-- beta = 1/sqrt(10)
-- alpha = sqrt (1 - beta^2)
--
-- toPolar (QS (3/sqrt(10)) (1/sqrt(10)))
-- PC 0.64350057 0.0
--
-- => theta = 0.64350057
--
--result:
--  *Ex4> sim oneTenth 
--  [(True,9.999983e-2),(False,0.90000015)]
oneTenth :: QIO Bool
oneTenth = do	q <- mkQbit False
		applyU (rot q expPauliY)
		measQbit q
			where 	expPauliY (a,b) = if (a == b) then c else (if b then s else -s)
				c = cos (theta/2)
				s = sin (theta/2)
				theta = 0.64350057

-- 6) 
falseProb :: Float -> QIO Bool
falseProb prob = do	q <- mkQbit False
			applyU (rot q expPauliY)
			measQbit q
				where 	expPauliY (a,b) = (if (a == b) then c else (if b then s else -s)) :+ 0
					c = cos (theta/2)
					s = sin (theta/2)
					theta = getTheta $ toPolar (QS alpha (sqrt (1 - prob) :+0 ))
					alpha = sqrt(prob) :+ 0
					getTheta (PC theta _) = theta


-- 7)
-- let a = 1/sqrt(2)
-- a|00> + a|11>
bell00 ::  QIO (Qbit, Qbit)
bell00 = bell' (False, False)

-- a|00> - a|11>
bell10 :: QIO (Qbit, Qbit)
bell10 = bell' (True, False)

-- a|01> + a|10>
bell01 :: QIO (Qbit, Qbit)
bell01 = bell' (False, True)

-- a|01> - a|10>
bell11 :: QIO (Qbit, Qbit)
bell11 = bell' (True, True)

bell' :: (Bool, Bool) -> QIO (Qbit, Qbit)
bell' (a,b) = do	q1 <- mkQbit a
                	q2 <- mkQbit b
                	applyU (hadamard q1)
                	applyU (controlledX q1 q2)
                	return (q1, q2)

-- 8)
bellU :: Qbit -> Qbit -> U
bellU q1 q2 = (controlledX q1 q2) `mappend` (hadamard q1)

bellMeasurement :: (Qbit, Qbit) -> QIO (Bool, Bool)
bellMeasurement (q1, q2) = do	applyU (bellU q1 q2)
				b1 <- measQbit q1
				b2 <- measQbit q2
				return (b1, b2)

testBellMeasurement :: (Bool, Bool) -> QIO (Bool, Bool)
testBellMeasurement x = do	(q1, q2) <- bell' x
				bellMeasurement (q1, q2)
