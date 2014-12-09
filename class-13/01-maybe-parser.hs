{-
   Тип Parser может быть определён следуюшим образом:
-}
import Control.Monad
import Data.Maybe

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser(\s -> Just(x,s))
  p >>= q = Parser(\s -> let v = apply p s in
		         case v of 
                            Just (x, s') -> apply (q x) s' 
			    Nothing -> Nothing )
  fail _ = Parser(\s -> Nothing)

instance MonadPlus Parser where
  mzero = Parser(\s -> Nothing)
  p `mplus` q = Parser (\ s -> let ps = apply p s
				in  if (isNothing ps) then (apply q s) else ps)
