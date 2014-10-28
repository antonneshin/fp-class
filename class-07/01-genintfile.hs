{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}
import Data.List
import Control.Monad
import System.IO
import System.Random
import System.Environment

makeStr :: (Show a, RandomGen g, Random a) => a -> a -> Int -> g -> [Char]
makeStr start stop lenstr gen = foldl (\ list a -> list ++ (show a) ++ " " ) [] (take lenstr $ randomRs (start, stop) gen)


makeFile :: (Enum b, Num b, Show a, Random a) => FilePath -> a -> a -> Int -> b -> StdGen -> IO ()
makeFile fname start stop lenstr lenf gen = writeFile fname $ fst $ foldl (\ (list, (start, stop, lenstr, gen) ) a -> (list++(makeStr start stop lenstr gen)++"\n" , (start, stop, lenstr, (snd $( random gen :: (Integer, StdGen)))) ) ) ([], (start, stop, lenstr, gen)) [1..lenf]
 

main = do
  [fname, p1, p2, lstr, lsf] <- getArgs
  gen <- newStdGen
  let
    start=read p1::Int
    stop=read p2::Int
    lenstr=read lstr::Int
    lenf=read lsf::Int
  makeFile fname start stop lenstr lenf gen
