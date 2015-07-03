{-# LANGUAGE ViewPatterns #-}

import Data.Bits
--import Data.Maybe
--import Data.Either
import qualified Data.Map as M
import Data.Sequence as S

mask  0 = 0xC800
mask  1 = 0xE400
mask  2 = 0x7200
mask  3 = 0x3100
mask  4 = 0x8C80
mask  5 = 0x4E40
mask  6 = 0x2720
mask  7 = 0x1310
mask  8 = 0x08C8
mask  9 = 0x04E4
mask 10 = 0x0272
mask 11 = 0x0131
mask 12 = 0x008C
mask 13 = 0x004E
mask 14 = 0x0027
mask 15 = 0x0013


flipt t p = t `xor` mask p

flipts :: Int -> [Int]
flipts t = map (flipt t) [0..15] 

type Results = M.Map Int Int
type Todo = S.Seq Int

initialResult = M.fromList [(0x0000, 0), (0xFFFF, 0)]

busca :: Int -> Results -> Todo -> Maybe Int
busca t m w = case M.lookup t m of 
    Just v -> Just v
    Nothing -> case expandBusca m w of 
                    Just (m', w') -> busca t m' w'
                    Nothing  -> Nothing

expandBusca m _ | M.null m = Just (initialResult, S.fromList $ flipts 0x0000 ++ flipts 0xFFFF)
expandBusca m s@(viewl -> EmptyL) = Nothing
expandBusca m (viewl -> (l :< ls))  = case M.lookup l m of
    Nothing -> error  $ "Should have found something. m: " ++ show m ++ " l:" ++ show l 
    Just v -> Just (M.union m $ M.fromList $ Prelude.zip (repeat $ v + 1) ns, ls >< (S.fromList ns))
        where ns = flipts l
              --ls' = ls :: Seq Int


--main = mapM putStrLn (map show (zip (['0'..'9'] ++ ['A'..'F']) [0..0xf]))


main = putStrLn $ show $ busca (mask 4) M.empty S.empty



--main = putStrLn $ prettify $ myGraph (genNodes "bwbw\nbwbw\nbwbw\nbwbw")






