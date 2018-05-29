import System.Random
import System.IO
import Data.Char

modulo :: Int-> Int ->Int -> Int
modulo _ 0 _ = 1
modulo base exp modu = ((modulo base (exp-1) modu) * base) `mod` modu

getGCD :: Bool -> (Int, StdGen) ->Int -> Int -> Int
getGCD True (_,_) _ p = p
getGCD False (p, newGen) f temp = getGCD ((gcd p f) == 1) (randomR (10,100) newGen) f p

getPrime :: Bool -> (Int, StdGen) -> Int -> Int
getPrime True (_,_) p = p
getPrime False (p, newGen) temp = getPrime (prime p 11) (randomR (10,100) newGen) p

gcd_e a b
	|b==0 = (1,0)
	|otherwise =
		let
			(q,r)= ((a `div` b),(a `mod` b))
			(s,t)= gcd_e b r
		in (t, s - q*t)

invrs a m = x `mod` m
	where (x,_) = gcd_e a m

find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        
prime n a
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
 
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = divMod n 2
                x2 = sq x

mulMod a b c = (b * c) `mod` a
squareMod a b = (b * b) `mod` a
powMod m = pow' (mulMod m) (squareMod m)

main=do
	handle<- openFile "publicKey.txt" WriteMode
	handle' <- openFile "privateKey.txt" WriteMode
	gen<-getStdGen
	let (rndNum, newG) = randomR (10,100) gen
	let p 
		|(prime rndNum 11)==True = rndNum 
		|otherwise = getPrime False (rndNum,newG) rndNum
	gen <- newStdGen
	let (rndNum, newG) = randomR (10,100) gen
	let q 
		|(prime rndNum 11)==True = rndNum 
		|otherwise = getPrime False (rndNum,newG) rndNum
	let 	n=p*q
		f=(p-1)*(q-1)
		(rndNum, newG) = randomR (1,f) gen
	let e
		|gcd rndNum f == 1 = rndNum
		|otherwise = getGCD False (rndNum,gen) f rndNum
	let d = invrs e f
	hPutStr handle' $ (show d) ++ " " ++ (show n) 
	hPutStr handle $ (show e) ++ " " ++ (show n) 
	hClose handle
	hClose handle'