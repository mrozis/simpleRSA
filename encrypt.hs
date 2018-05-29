import System.IO
import Data.Char

createCipher :: [Int] -> String
createCipher (y:[]) = show y
createCipher (y:ys) = (show y) ++ " " ++ (createCipher ys)


modulo :: Int-> Int ->Int -> Int
modulo _ 0 _ = 1
modulo base exp modu = ((modulo base (exp-1) modu) * base) `mod` modu

mapModulo a b (x:[]) = (modulo x a b:[])
mapModulo a b (x:xs) = (modulo x a b:mapModulo a b xs)


main = do
	handle <- openFile "publicKey.txt" ReadMode
	handle' <- openFile "cipher.txt" WriteMode
	z <- hGetContents handle
	let (e:n:zs) = words z
	putStrLn "Give message for encryption"
	plain <- getLine
	let	ordPlain = map ord plain
		e'=read e
		n'=read n
		c= mapModulo e' n' ordPlain	
	hPutStr handle' $ createCipher c
	hClose handle
	hClose handle'
	