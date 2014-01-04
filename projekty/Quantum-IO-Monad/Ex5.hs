-- G53NSC - Non-Standard Computation
-- excercise 5
-- author: Jiri Kremser

module Ex5 where
import Ex4
import Ex3
import Ex2
import QIO.Qio
import QIO.QioSyn
import Data.Monoid


-- 1)
sdcAlice :: (Bool, Bool) -> Qbit -> QIO Qbit
sdcAlice (b1,b2) q3 = do	applyU ((if b2 then unot q3 else mempty) `mappend` (if b1 then uZ q3 else mempty))
				return q3

sdcBob :: Qbit -> Qbit -> QIO (Bool, Bool)
sdcBob = curry bellMeasurement

superdense :: (Bool, Bool) -> QIO (Bool, Bool)
superdense (b1, b2) = do	(q1, q2) <- bell00
				q1' <- sdcAlice (b1, b2) q1
				sdcBob q1' q2

-- 2)
qtAlice :: Qbit -> Qbit -> QIO (Bool, Bool)
qtAlice = sdcBob

qtBob :: (Bool, Bool) -> Qbit -> QIO Qbit
qtBob = sdcAlice

teleport :: Qbit -> QIO Qbit
teleport q1 = do	(q2, q3) <- bell00
			x <- qtAlice q1 q2
			qtBob x q3

testTeleport b = do 	(q,_) <- bell11
			q' <- teleport q
			b' <- measQbit q'
			return b'

-- 3)
deutsch :: (Bool -> Bool) -> QIO Bool
deutsch f = do	q1 <- superQ False
		q2 <- superQ True
		applyU((cond q1 (\x -> if (f x) then unot q2 else mempty)) `mappend` hadamard q1)
		b <- measQbit q1
		return b

superQ :: Bool -> QIO Qbit
superQ b = do	q <-mkQbit b
		applyU(hadamard q)
		return q

-- 4) number of constant functions of type " _ -> a " = card(a) = 2 for a=Bool
--    number of balanced funcfions of type (a, b, c) -> d = x \choose y
--    where x = card(a) * card(b) * card(c)
--          y = x / card (d)
--          => 8 \choose 4 = (8*7*6*5)/4! = 70 for a=b=c=d=Bool
constantZero :: (Bool, Bool, Bool) -> Bool
constantZero _ = False

constantOne :: (Bool, Bool, Bool) -> Bool
constantOne _ = True

balanced1 :: (Bool, Bool, Bool) -> Bool
balanced1 (b, _, _) = b

balanced2 :: (Bool, Bool, Bool) -> Bool
balanced2 (b, _, _) = not b

balanced3 :: (Bool, Bool, Bool) -> Bool
balanced3 (_, b, _) = not b

balanced4 :: (Bool, Bool, Bool) -> Bool
balanced4 (_, _, b) = b

balanced5 :: (Bool, Bool, Bool) -> Bool
balanced5 (b1, b2, _) = (b1 && b2) || (not b1 && not b2)

balanced6 :: (Bool, Bool, Bool) -> Bool
balanced6 (b1, _, b2) = (not b1 && b2) || (b1 && not b2)


-- 5)
deutschJozsa :: ((Bool, Bool, Bool) -> Bool) -> QIO (Bool, Bool, Bool)
deutschJozsa f = do	q0 <- superQ False
			q1 <- superQ False
			q2 <- superQ False
			q3 <- superQ True
			applyU(cond3 (q0,q1,q2) (\x -> if f x then unot q3 else mempty) `mappend` hadamard q0  `mappend` hadamard q1  `mappend` hadamard q2)
			b0 <- measQbit q0
			b1 <- measQbit q1
			b2 <- measQbit q2
			return (b0, b1, b2)


cond3 :: (Qbit, Qbit, Qbit) -> ((Bool, Bool, Bool) -> U) -> U
cond3 (q0, q1, q2) f = cond q0 (\x -> cond q1 (\y -> cond q2 (\z -> f (x , y, z ))))

