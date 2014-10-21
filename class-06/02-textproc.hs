{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
{-
import System.Random
import System.Environment
import System.IO
import Data.Char
import System.Directory
-}

import System.Random
import System.Environment
import Data.List
import Data.Char
import System.IO
import System.Directory


gentextf :: FilePath -> Int -> IO ()
gentextf fname g = do
  let
    len = randomR (2,100) (mkStdGen g)
    ls = foldl (\ (list, gg) a -> (list ++ (take (fst $ randomR (1,100) gg) $ randomRs ('a','z') gg)++"\n", mkStdGen (fst $ randomR (1,100) gg)) ) ([], snd len) (take (fst len) $ repeat [])
  mapM_ (\x -> appendFile fname (x++"\n")) (lines $ fst $ ls)
  



getNumber :: IO Integer
getNumber = do
  line <- getLine
  return $ read line



myzip :: FilePath -> FilePath -> IO ()
myzip f1 f2 = do
  cont1 <-readFile f1
  cont2 <- readFile f2
  let l1 = lines cont1
      l2 = lines cont2
  mapM_ putStrLn $  zipWith (\ a b -> a++b) l1 l2
  mapM_ putStrLn $ if (length cont1)> (length cont2) then (drop (length l2) l1) else (drop (length l1) l2) 
  


appS fname line = do 
  writeFile "000" line
  cont <- readFile fname
  appendFile "000" cont
  removeFile fname
  renameFile "000" fname

cnt_line :: FilePath -> IO ()
cnt_line fname = do
  contents <- readFile fname
  print $ length $ lines $ contents

maindo answer 
  | answer==1 = do
    [name] <- getArgs
    cnt_line name
  | answer==21 = do
    [name, line] <-getArgs
    appendFile name line
  | answer==22 = do
    [name, line] <-getArgs
    appS name line
  | answer==3 = do  
    [name] <- getArgs
    contents <- readFile name
    putStr $ map toUpper contents
  | answer==4 = do
    [name1, name2] <- getArgs
    myzip name1 name2 
  | answer==5 = do
    [name, n] <- getArgs
    gentextf name (read n)

main = do
  putStrLn "выберете действие:"
  putStrLn "1) подсчёт количества строк в заданном текстовом файле;"
  putStrLn "21) добавление заданной строки в конец заданного файла;"
  putStrLn "22) добавление заданной строки в начало заданного файла;"
  putStrLn "3) преобразование всех буквенных символов заданного файла к верхнему регистру (результат выводится на консоль);"
  putStrLn "4) построчное слияние двух заданных файлов (каждая строка первого файла соединяется с соответствующей строкой второго файла);"
  putStrLn "5) генерация случайного текстового файла (случайность должна ограничиваться максимальным количеством строк в файле и символов в строке)."
  answer <- getNumber
  maindo answer 
	
