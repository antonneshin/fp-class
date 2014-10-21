{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}
import System.Random
import System.Environment
import Data.List
import Data.Char
import System.IO
import System.Directory

gendecp fname g = do
  let
    len = randomR (1,100) (mkStdGen g)
    ls = foldl (\ (list, gg) a -> ( list++[( fst $ random gg::(Int, StdGen) , fst $ random (snd $ random gg::(Int, StdGen))::(Int,StdGen) )] ,{-2-} snd $ random (snd $ random gg::(Int, StdGen))::(Int,StdGen) ) ) ([], {-snd-} len) (take (fst len) $ repeat [])
  mapM_ (\x -> appendFile fname (x++"\n")) (lines $ fst $ ls)


main = do
  [name, g] <- getArgs
  gendecp name (read g)
