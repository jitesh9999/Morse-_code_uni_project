

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
