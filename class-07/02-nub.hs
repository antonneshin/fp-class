{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list xs = length $ foldl(\ list a -> if (a `elem` list) then list else list++[a] ) [] xs

nub_seq :: Eq a => Seq.Seq a -> Int
nub_seq xs = let (_ Seq.:>yy)=Seq.viewr $ Seq.scanl (\ sec a -> if (length $ Seq.elemIndicesL a sec ) == 0 then (sec Seq.|>a) else sec ) (Seq.fromList []) xs
             in Seq.length yy

nub_arr :: Array Int Int -> Int
nub_arr xs = nub_list $ elems xs

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
