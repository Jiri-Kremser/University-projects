-- G53NSC - Non-Standard Computation
-- excercise 2 
-- author: Jiri Kremser

module Ex2 where
import Ex1
import QIO.QioSyn
import QIO.QioClass
import Data.Monoid


-- Part I - Uiversality
-- 1)
nor::(Bit,Bit) -> Bit
nor (O,O) = I
nor _ = O

-- 2)
neg'::Bit -> Bit
neg' n = nor (n,n)

or'::(Bit,Bit) -> Bit
or' = neg'.nor

and'::(Bit, Bit) -> Bit
and' (x,y) = (neg'.or') (neg' x, neg' y)

-- 3)
fredkin::(Bit, Bit, Bit) -> (Bit, Bit, Bit)
fredkin (I,O,I) = (I,I,O)
fredkin (I,I,O) = (I,O,I)
fredkin x = x

-- 4)
fand::(Bit, Bit) -> Bit
fand (x,y) = ((\(_,_,n) -> n).fredkin) (x,y,O)

for::(Bit, Bit) -> Bit
for (x,y) = ((\(_,_,n) -> n).fredkin) (x,I,y)

fneg::Bit -> Bit
fneg x = ((\(_,_,n) -> n).fredkin) (x,O,I)

-- 5)
foo:: (Bit,Bit,Bit,Bit) -> Bit
foo (w,x,y,z) = foldl (curry for) O ( (foldl (curry fand) I (fneg w:fneg x:fneg y:z:[])) :
				      (foldl (curry fand) I (fneg w:fneg x:y:fneg z:[])) :
				      (foldl (curry fand) I (fneg w:x:fneg y:fneg z:[])) :
				      (foldl (curry fand) I (w:fneg x:fneg y:fneg z:[])) :[])


-- Part II - Reversible Computation
wordSize :: Int
wordSize = 32

-- 1)
int2bits'::Int->[Bool]
int2bits' x = map (\bit -> if bit == O then False else True) $ take wordSize $ int2Bits x ++ repeat O

bits2qbits::[Bool]->QIO [Qbit]
bits2qbits [] = return []
bits2qbits (x:xs) = do 	rest <- bits2qbits xs
			bit <- mkQbit x 
			return (bit : rest)

int2qbits::Int-> QIO [Qbit]
int2qbits = bits2qbits.int2bits'

-- 2)
qbits2int::[Qbit] -> QIO Int
qbits2int [] = return 0
qbits2int s = do  bits <- qbits2bits s
		  return $ bits2Int (map (\x->if x then I else O) bits)

qbits2bits [] = return []
qbits2bits (x:xs) = do  rest <- qbits2bits xs
			bit <- measQbit x
			return (bit : rest)


int2int :: Int -> QIO Int
int2int n = do 	qn <- int2qbits n
                x <- qbits2int qn
		return x

controlledX :: Qbit -> Qbit -> U
controlledX r1 r2 = cond r1 (\x -> if x then unot r2 else mempty)


toffoli :: Qbit -> Qbit -> Qbit -> U
toffoli r1 r2 r3 = cond r1 (\x -> (if x then (controlledX r2 r3 ) else mempty))

-- 3)
fullAdder :: Qbit -> Qbit -> Qbit -> Qbit -> U
fullAdder r1 r2 r3 r4 = (toffoli r2 r3 r4) `mappend` (controlledX r2 r3) `mappend` (toffoli r1 r3 r4) `mappend` (controlledX r1 r3)

-- 4)
adder :: [Qbit] -> [Qbit] -> [Qbit] -> Qbit -> U
adder a b o overflow = adder' a b o overflow (wordSize-1)
        where   adder' a b o overflow (-1) = mempty
                adder' a b o overflow n =  adder' a b o overflow (n-1) `mappend` (fullAdder (o!!n) (a!!n) (b!!n) (if n == wordSize-1 then overflow else o!!(n + 1))) 

-- 5)
undoCarry :: Qbit -> Qbit -> Qbit -> Qbit -> U
undoCarry r1 r2 r3 r4 = (controlledX r1 r3) `mappend` (toffoli r1 r3 r4) `mappend` (controlledX r2 r3) `mappend` (toffoli r2 r3 r4) `mappend` (controlledX r2 r3) `mappend` (controlledX r1 r3)


undoCarryAll :: [Qbit] -> [Qbit] -> [Qbit] -> Qbit -> U
undoCarryAll a b o overflow = uca a b o overflow (wordSize-1)
	where	uca a b o overflow (-1) = mempty
		uca a b o overflow n =  uca a b o overflow (n-1) `mappend` (undoCarry (o!!n) (a!!n) (b!!n) (if n == wordSize-1 then overflow else o!!(n + 1)))


letBits :: [Bool] -> ([Qbit] -> U ) -> U
letBits bs fbs = letBits' bs []
   	where 	letBits' [] bs = fbs bs
            	letBits' (b:bs) bs' = ulet b (\x -> letBits' bs (bs' ++ [x]))

reversibleAdder :: [Qbit] -> [Qbit] -> Qbit -> U
reversibleAdder a b overflow = letBits (int2bits' 0) (\o -> (adder `mappend` undoCarryAll) a b o overflow)

-- 6)
add :: Int -> Int -> QIO Int
add x1 x2 = do		a <- int2qbits x1
			b <- int2qbits x2
			overflow <- mkQbit False
			applyU (reversibleAdder a b overflow)
			qbits2int b


-- 7)
{-
make2Complement :: [Qbit] -> QIO [Qbit]
make2Complement s = do	bits <- qbits2bits s
			complement <- bits2qbits (map not bits)
			one <- int2qbits 1
			overflow <- mkQbit False
			applyU (reversibleAdder one complement overflow)
			return complement

sub :: Int -> Int -> QIO Int
sub x1 x2 = do	a <- int2qbits x1
		aux <- int2qbits x2
		b <- make2Complement aux
		overflow <- mkQbit False
		applyU (reversibleAdder a b overflow)
		qbits2int b
-}

sub :: Int -> Int -> QIO Int
sub x1 x2 = do	a <- int2qbits x2
		b <- int2qbits x1
		overflow <- mkQbit False
		applyU $ (urev (reversibleAdder a b overflow))
		qbits2int b

