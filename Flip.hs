{-# LANGUAGE ViewPatterns #-}

import Data.Bits
import Debug.Trace
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

busca1 :: Int -> Results -> Todo -> Maybe Int
busca1 t m w = case M.lookup t m of 
    Just v -> Just v
    Nothing -> case expandBusca m w of 
                    Just (m', w') -> busca1 t m' w'
                    Nothing  -> Nothing

busca :: Int -> Maybe Int
busca t = busca1 t M.empty S.empty

expandBusca m _ | M.null m = Just (initialResult, S.fromList [0x0000, 0xFFFF])
expandBusca m s@(viewl -> EmptyL) = Nothing
expandBusca m (viewl -> (l :< ls))  = case M.lookup l m of
    Nothing -> error  $ "Should have found something. m: " ++ show m ++ " l:" ++ show l  ++ " s: " ++ show ls
    Just v -> Just (M.union m m2, ls >< (S.fromList ns))
        where ns = [f | f <- flipts l, M.notMember f m]
              m2 = M.fromList $ Prelude.zip ns (repeat $ v + 1)



parseTab :: String -> Int
parseTab ls = shift (parseL (concat $ lines ls)) (-1)
    where parseC :: Char -> Int
          parseC c = case c of
            'b' -> 1
            'w' -> 0
            x -> error $ "Ilegal character " ++ show x
          parseL l = foldl (\x c -> shift (x .|. (parseC c)) 1) 0x0000 l



--main = putStrLn $ show 0x9d98 ++ " " ++ (show $ parseTab "bwwb\nbbwb\nbwwb\nbwww")




main = do 
    c <- getContents
    let p = parseTab c
    case busca p of
        Nothing -> putStrLn "Impossible"
        Just x -> putStrLn $ show x


--a = 0xa :: Int
--b = 0xa0 :: Int

--main = putStrLn $ show $ shift (a .|. b) 1


--main = putStrLn $ show $ busca 0x9D98

--main = putStrLn $ prettify $ myGraph (genNodes "bwbw\nbwbw\nbwbw\nbwbw")






