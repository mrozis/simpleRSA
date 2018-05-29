import System.IO
import Data.Char

mapModulo a b (x:[]) = (modulo x a b:[])
mapModulo a b (x:xs) = (modulo x a b:mapModulo a b xs)


modulo :: Int-> Int ->Int -> Int
modulo _ 0 _ = 1
modulo base exp modu = ((modulo base (exp-1) modu) * base) `mod` modu


main = do
	handle <- openFile "privateKey.txt" ReadMode
	handle1 <- openFile "cipher.txt" ReadMode
	handle' <- openFile "plain.txt" WriteMode
	y <- hGetContents handle
	c <- hGetContents handle1
	let (d:n:ys) = words y
	let	d'=read d
		n'=read n
		c'=map read $ words c
		plain = map chr $ mapModulo d' n' c'
	hPutStr handle' $ plain
	hClose handle
	hClose handle1
	hClose handle'