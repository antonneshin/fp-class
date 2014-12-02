import Control.Monad.Reader
import System.Environment



do_smth :: [Char] -> Integer -> Integer
do_smth str = 
	let s = span (/='=') str
	    n = (\ x -> read x::Integer) $ drop 1 $ snd $ s
	    do_s = fst $ s
 	in (if do_s=="summand" then (+n) else (if do_s=="multiplier" then (*n) else (if do_s=="divisor" then (`div` n) else error "error in DO_SMTH")) )



calcOperations :: [Integer -> Integer] -> Reader Integer [Integer]
calcOperations ops = do
 num <- ask
 return $ fst $ foldl (\(ls, n) op -> (ls ++ [op n],n)) ([],num) ops



readConf :: FilePath -> FilePath -> IO ()
readConf fconf flist = do
	conf <- fmap lines (readFile fconf)
	list <- fmap lines (readFile flist)
	let func = map do_smth conf
	    cop = calcOperations func
            num = foldl (\ ls x -> ls ++ [read x::Integer]) [] list
	mapM_ (print . runReader cop) num


main = do
  [fconf, flist] <- getArgs
  readConf fconf flist
