-- 
-- -- 
-- -- main = do
-- --   putStrLn "Whats' your name"
-- --   name <- getLine
-- --   putStrLn("Hello "++ name) 
--   
-- -- addme :: Int -> Int -> Int
-- -- 
-- -- addme x y = x + y
-- 
-- -- addTuples :: (Int , Int ) -> (Int , Int ) -> (Int , Int )
-- -- addTuples (x,y) (x2,y2) = (x+x2, y+y2)
-- 
-- -- 
-- -- isOdd :: Int -> Bool
-- -- isOdd n
-- --   | n `mod` 2 == 0 = False
-- --   | otherwise = True
-- --   
-- -- isEven n = n `mod` 2 == 0
-- 
-- ------- Guards__--------
-- 
-- -- getListItems :: String -> String
-- -- 
-- -- getListItems [] = "yoour list is empty"
-- -- getListItems all@(x:xs) = "the first letter is " ++ [x]
-- -- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- -- about possible missing cases in pattern-matching definitions
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- 
-- -- see https://wiki.haskell.org/Safe_Haskell
-- {-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}
-- 
-- module Assignment2 (encodeWord , encodeWords , encodeText ,
--                     decodeText ,
--                     decodeTextWithTree ,
--                     ramify ,
--                     tabulate ,
--                     tree) where
-- 
-- import Types
-- import Data.List
-- 
-- ---------------------------------------------------------------------------------
-- ---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
-- ---------------------------------------------------------------------------------
-- 
-- {- Question 1 -}
-- encodeWord :: Table -> String -> Code
-- encodeWord table []= []
-- encodeWord table xs = intercalate shortGap [ code | a <- xs , Just code <- [lookup a table]]  
-- 
-- encodeWords :: Table -> [String] -> Code
-- encodeWords table word = intercalate mediumGap (map (encodeWord table) word) 
-- 
-- 
-- encodeText :: Table -> String -> Code
-- encodeText table = encodeWords table . words
-- 
-- 
-- -- Helper function: splits a list by a separator (using reverse technique)
-- split :: Eq a => [a] -> [a] -> [[a]]
-- split s [] = []
-- split s xs = reverse (map reverse (split' s (reverse xs)))
--   where
--     split' s [] = []
--     split' s ys = 
--         let (chunk, rest) = breakFromStart s ys
--         in if null chunk && null rest
--            then []
--            else if null chunk
--                 then split' s rest
--                 else chunk : split' s rest
--     
--     breakFromStart s [] = ([], [])
--     breakFromStart s list
--         | take (length s) list == s = ([], drop (length s) list)
--         | otherwise = 
--             let (chunk, rest) = breakFromStart s (tail list)
--             in (head list : chunk, rest)
-- 
-- 
-- {- Question 2 -}
-- decodeText :: Table -> Code -> String
-- decodeText table code = 
--     let reverseTable = [(cd, ch) | (ch, cd) <- table]
--         wordCodes = filter (not . null) (split mediumGap code)
--         decodeWord wordCode = 
--             let charCodes = filter (not . null) (split shortGap wordCode)
--                 decodeChar charCode = 
--                     case lookup charCode reverseTable of
--                         Just ch -> ch
--                         Nothing -> '?'
--             in map decodeChar charCodes
--         decodedWords = map decodeWord wordCodes
--     in unwords decodedWords
-- {- Question 3 -}
-- -- decodeTextWithTree :: Tree -> Code-> String
-- -- decodeTextWithTree = undefined
-- decodeTextWithTree :: Tree -> Code -> String
-- decodeTextWithTree tree code = 
--     let wordCodes = filter (not . null) (split mediumGap code)
--         decodeWord wordCode = 
--             let charCodes = filter (not . null) (split shortGap wordCode)
--             in map (decodeChar tree) charCodes
--         decodedWords = map decodeWord wordCodes
--     in unwords decodedWords
--   where
--     -- Decode a single character code by traversing the tree
--     decodeChar :: Tree -> Code -> Char
--     decodeChar Empty _ = '?'
--     decodeChar (Branch Nothing _ _) [] = '?'
--     decodeChar (Branch (Just ch) _ _) [] = ch
--     decodeChar (Branch _ left right) (Beep:Silence:rest) = 
--         -- dit to left
--         decodeChar left rest
--     decodeChar (Branch _ left right) (Beep:Beep:Beep:Silence:rest) = 
--         -- dah to right
--         decodeChar right rest
--     decodeChar _ _ = '-'  
-- {- Question 4 -}
-- ramify :: Table -> Tree
-- ramify table = foldr addChar (Branch Nothing Empty Empty) table
--   where
--     -- add char ad code to the tree
--     insertChar :: (Char, Code) -> Tree -> Tree
--     insertChar (char, code) tree = insert tree code char
--     
--     insert :: Tree -> Code -> Char -> Tree
--     insert Empty [] char = Branch (Just char) Empty Empty
--     insert Empty code char = 
--         Branch Nothing (insertLeft code char) (insertRight code char)
--       where
--         insertLeft (Beep:Silence:rest) char = insert Empty rest char
--         insertLeft _ _ = Empty
--         insertRight (Beep:Beep:Beep:Silence:rest) char = insert Empty rest char
--         insertRight _ _ = Empty
--     
--     insert (Branch maybeChar left right) [] char = 
--         Branch (Just char) left right
--     insert (Branch maybeChar left right) (Beep:Silence:rest) char = 
--         -- dit add to left subtree
--         Branch maybeChar (insert left rest char) right
--     insert (Branch maybeChar left right) (Beep:Beep:Beep:Silence:rest) char = 
--         -- dah add to right subtree
--         Branch maybeChar left (insert right rest char)
--     
--     insert tree _ _ = tree  
--     
--     
-- {- Question 5 -}
-- tabulate :: Tree -> Table
-- tabulate tree = tabulateHelper tree []
--   where
--     tabulateHelper :: Tree -> Code -> Table
--     tabulateHelper Empty _ = []
--     tabulateHelper (Branch Nothing left right) code = 
--         tabulateHelper left (code ++ dit) ++ tabulateHelper right (code ++ dah)
--     tabulateHelper (Branch (Just ch) left right) code = 
--         (ch, code) : tabulateHelper left (code ++ dit) ++ tabulateHelper right (code ++ dah)
-- 
-- -- {- Question 6 -}
-- -- brackets :: Bracket -> String
-- -- brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
-- -- brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"
-- -- 
-- -- tree :: String -> Maybe Bracket
-- -- tree = undefined
-- -- 
-- -- isWellBracketed :: String -> Bool
-- -- isWellBracketed xs = case tree xs of
-- --                       Nothing -> False
-- --                       Just _  -> True
-- {- Question 6 -}
-- brackets :: Bracket -> String
-- brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
-- brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"
-- 
-- tree :: String -> Maybe Bracket
-- tree xs = case parseOne xs of
--             Just (bracket, "") -> Just bracket  -- Successfully parsed entire string
--             _ -> Nothing  -- Failed to parse or leftover characters
--   where
--     -- Try to parse a single bracket tree and return remaining string
--     parseOne :: String -> Maybe (Bracket, String)
--     parseOne ('(':rest) = parseRound rest []
--     parseOne ('{':rest) = parseCurly rest []
--     parseOne _ = Nothing
--     
--     -- Parse a Round bracket and its contents
--     parseRound :: String -> [Bracket] -> Maybe (Bracket, String)
--     parseRound (')':rest) acc = Just (Round (reverse acc), rest)
--     parseRound xs acc = 
--         case parseOne xs of
--             Nothing -> Nothing
--             Just (bracket, rest) -> parseRound rest (bracket : acc)
--     
--     -- Parse a Curly bracket and its contents
--     parseCurly :: String -> [Bracket] -> Maybe (Bracket, String)
--     parseCurly ('}':rest) acc = Just (Curly (reverse acc), rest)
--     parseCurly xs acc = 
--         case parseOne xs of
--             Nothing -> Nothing
--             Just (bracket, rest) -> parseCurly rest (bracket : acc)
-- 
-- isWellBracketed :: String -> Bool
-- isWellBracketed xs = case tree xs of
--                       Nothing -> False
--                       Just _  -> True




















module Assignment2 (encodeWord , encodeWords , encodeText ,
                    decodeText ,
                    decodeTextWithTree ,
                    ramify ,
                    tabulate ,
                    tree) where

import Types
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
encodeWord :: Table -> String -> Code
encodeWord table xs = intercalate shortGap [ code | a <- xs , Just code <- [lookup a table]]  

encodeWords :: Table -> [String] -> Code
encodeWords table word = intercalate mediumGap (map (encodeWord table) word) 


encodeText :: Table -> String -> Code
encodeText table = encodeWords table . words


split :: Eq a => [a] -> [a] -> [[a]]
split i input = reverse (map reverse (splitReversed' i (reverse input)))
  where
    splitReversed' i [] = []
    splitReversed' i ys = 
        let (bit, rest) = found i ys
        in if null bit && null rest
          then []
           else if null bit
                then splitReversed' i rest
                else bit : splitReversed' i rest
    
    found i [] = ([], [])
    found i (x:xs)
        | take (length i) (x:xs) == i = ([], drop (length i) (x:xs))
        | otherwise = 
            let (bit, rest) = found i xs
            in (x : bit, rest)
            
            
{- Question 2 -}
decodeText :: Table -> Code -> String
decodeText table code = 
    let reverseTable = map (\(c, cd) -> (cd, c)) table
        wordCodes =(split mediumGap code)
        decodeWord wordCode = 
            let charCodes = (split shortGap wordCode)
                decodeChar charCode = 
                    case lookup charCode reverseTable of
                        Just ch -> ch
                        Nothing -> '?'
            in map decodeChar charCodes
        decodedWords = map decodeWord wordCodes
    in unwords decodedWords

    
{- Question 3 -}
decodeTextWithTree :: Tree -> Code -> String
decodeTextWithTree tree code = 
    let wordCodes = (split mediumGap code)
        decodeWord wordCode = 
            let charCodes =(split shortGap wordCode)
            in map (decodeChar tree) charCodes
        decodedWords = map decodeWord wordCodes
    in unwords decodedWords
  where
    decodeChar :: Tree -> Code -> Char
    decodeChar Empty _ = '?'
    decodeChar (Branch Nothing _ _) [] = '?'
    decodeChar (Branch (Just char) _ _) [] = char
    decodeChar (Branch _ left right) (Beep:Silence:rest) = 
        -- dit to left
        decodeChar left rest
    decodeChar (Branch _ left right) (Beep:Beep:Beep:Silence:rest) = 
        -- dah to right
        decodeChar right rest
    decodeChar _ _ = '-' 

    
{- Question 4 -}
ramify :: Table -> Tree
ramify table = foldr addChar (Branch Nothing Empty Empty) table
  where
    addChar :: (Char, Code) -> Tree -> Tree
    addChar (char, code) tree = add tree code char
    
    add :: Tree -> Code -> Char -> Tree
    add Empty [] char = Branch (Just char) Empty Empty
    add Empty code char = 
        Branch Nothing (addLeft code char) (addRight code char)
      where
        addLeft (Beep:Silence:rest) char = add Empty rest char
        addLeft _ _ = Empty
        addRight (Beep:Beep:Beep:Silence:rest) char = add Empty rest char
        addRight _ _ = Empty
    
    add (Branch maybeChar left right) [] char = 
        Branch (Just char) left right
    add (Branch maybeChar left right) (Beep:Silence:rest) char = 
        -- dit add to left subtree
        Branch maybeChar (add left rest char) right
    add (Branch maybeChar left right) (Beep:Beep:Beep:Silence:rest) char = 
        -- dah add to right subtree
        Branch maybeChar left (add right rest char)
    
    add tree _ _ = tree  
    
    
{- Question 5 -}
tabulate :: Tree -> Table
tabulate tree = tHelper tree []
  where
    tHelper :: Tree -> Code -> Table
    tHelper Empty _ = []
    tHelper (Branch Nothing left right) code = 
        tHelper left (code ++ dit) ++ tHelper right (code ++ dah)
    tHelper (Branch (Just ch) left right) code = 
        (ch, code) : tHelper left (code ++ dit) ++ tHelper right (code ++ dah)


{- Question 6 -}
brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree xs = case parseOne xs of
            Just (bracket, "") -> Just bracket
            _ -> Nothing
  where
    parseOne :: String -> Maybe (Bracket, String)
    parseOne ('(':rest) = parseRound rest []
    parseOne ('{':rest) = parseCurly rest []
    parseOne _ = Nothing
    parseRound :: String -> [Bracket] -> Maybe (Bracket, String)
    parseRound (')':rest) acc = Just (Round (reverse acc), rest)
    parseRound xs acc = 
        case parseOne xs of
            Nothing -> Nothing
            Just (bracket, rest) -> parseRound rest (bracket : acc)
    parseCurly :: String -> [Bracket] -> Maybe (Bracket, String)
    parseCurly ('}':rest) acc = Just (Curly (reverse acc), rest)
    parseCurly xs acc = 
        case parseOne xs of
            Nothing -> Nothing
            Just (bracket, rest) -> parseCurly rest (bracket : acc)

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True