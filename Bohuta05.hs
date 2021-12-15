{-# OPTIONS_GHC -Wall #-}
module Bohuta05 where

  import Data.Char(isSpace, isDigit, isLetter)

  type Name       = String
  type Attributes = [(String, String)]
  data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

  spaces :: String -> String
  spaces s
    | null s = ""
    | isSpace(head s) = spaces (tail s)
    | otherwise = s

  manyT :: String -> (String,String)
  manyT s
    | null s = ([],[])
    | head s == '>' || head s == '<' = ([], s)
    | otherwise = (head s: fst (manyT (tail s)), snd (manyT (tail s)))

  value :: String -> (String,String)
  value s
    | null s = ([], [])
    | head s == '"' = ([], s)
    |otherwise  = (head s: fst (value (tail s)), snd (value (tail s)))

  manyN :: String -> (String,String)
  manyN s
    | null s = ([], [])
    | isLetter (head s) || isDigit (head s) || head s == '.' || head s == '-' = (head s: fst (manyN (tail s)), snd (manyN (tail s)))
    | otherwise = ([], s)

  name :: String ->  Maybe(String,String)
  name s = if null s ||  isLetter (head s) == False  then Nothing else Just(manyN s)

  text :: String ->  Maybe(String,String)
  text  s = if null s || head s == '<' || head s == '>' then Nothing else Just(manyT s)

  fullValue :: String ->  Maybe(String,String)
  fullValue s
    | null s || head s /= '"' = Nothing
    | null (snd  (value (tail s))) == False && head (snd  (value (tail s))) == '"' = Just(fst(value (tail s)), tail (snd (value (tail s))))
    | otherwise = Nothing

  atrEqual :: String -> Maybe String
  atrEqual s = if null s || head s /= '=' then Nothing  else Just (tail s)

  getEqual :: String -> String -> Maybe ((String,String),String)
  getEqual m s = case atrEqual (spaces s) of
    Just y -> case fullValue (spaces y) of 
      Just (h,t) -> Just ((m, h), spaces t) 
      _ -> Nothing 
    _ -> Nothing 

  attrib :: String -> Maybe ((String,String),String)
  attrib s = case name s of
    Just (h,t) -> getEqual h t
    _ -> Nothing

  helpManyAtt :: Attributes -> String -> (Attributes, String)
  helpManyAtt arr s = case attrib s of
    Just (a,r) -> helpManyAtt (a:arr) r
    _ -> (arr, s)

  manyAtt :: String -> Maybe (Attributes,String)
  manyAtt s = Just (helpManyAtt [] s)

  begTag :: String -> Maybe ((String,Attributes),String)
  begTag s = if null s || head s /= '<' then Nothing else case name (tail s) of
    Just (h,t) -> case manyAtt (spaces t) of
      Just (atr, rest) -> if null rest || head rest /= '>' then Nothing else Just ((h, atr), (tail rest))
      _ -> Nothing 
    _ -> Nothing  

  endTag :: String -> Maybe (String,String)
  endTag  ('<': t) = endTag t
  endTag ('/': t) = case name t of
    Just (h,y) -> if null y || head y /= '>' then Nothing else Just (h, tail y)
    _ -> Nothing
  endTag _ = Nothing   

  -- ������ 6.a -----------------------------------------
  element :: String -> Maybe (XML,String)
  element s = case begTag s of
    Just((elements, atr), rest) -> case manyXML rest of 
      Just (xmls, rest1) -> case endTag rest1 of
        Just (tag, rest2) -> if elements == tag then Just (Element elements atr xmls, rest2) else Nothing 
        _ -> Nothing 
      _ -> Nothing  
    _ -> Nothing 

  -- ������ 6.b -----------------------------------------
  xml :: String -> Maybe (XML,String)
  xml s = case element s of
    Just (xml0, rest) -> Just (xml0,  rest)
    _ -> case text s of
      Just (inner, rest) -> Just(Text inner,  rest)
      _ -> Nothing

  -- ������ 6.c -----------------------------------------
  inset :: [XML] -> Maybe ([XML],String) -> Maybe ([XML],String)
  inset xmls rest = case rest of
    Just (a,b) -> Just (xmls ++ a, b)
    _ -> Nothing

  manyXML :: String -> Maybe ([XML],String)
  manyXML ('<':'/':rest) = Just([], '<':'/':rest)
  manyXML s = case xml s of
    -- Just (Text xmls, '<':'/':rest) -> case manyXML ('<':'/':rest) of
    --   Just (a:b, rest1) -> inset (Text xmls:(a:b)) (manyXML rest1)
    --   Just (xs, rest2) -> Just(Text xmls:xs, rest2)
    --   _ -> Nothing
    -- Just (Text _, _) -> Nothing 
    Just (xmls, rest) -> case manyXML rest of
      Just (a:b, rest1) -> inset (xmls:(a:b)) (manyXML rest1)
      Just (xs, rest2) -> Just(xmls:xs, rest2)
      _ -> Nothing
    _ -> Nothing

  -- ������ 7 -----------------------------------------
  fullXML :: String -> Maybe XML
  fullXML s = case element (spaces s) of
    Just(elem0, rest) -> if spaces rest == "" then Just elem0 else Nothing 
    _ -> Nothing 

  -- ������ ���� -------------------------------------------
  -- ����� ����� XML-��'���� (��� �������)
  stst1, stst2, stst3 :: String
  stst1 = "<a>A</a>"
  stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
  stst3 = "<a>\
        \<b>\
          \<c att=\"att1\">text1</c>\
          \<c att=\"att2\">text2</c>\
        \</b>\
        \<b>\
          \<c att=\"att3\">text3</c>\
          \<d>text4</d>\
        \</b>\
      \</a>"

  -- ���������� ������ ���������� XML-��'����
  x1, x2, x3 :: XML
  x1 = Element "a" [] [Text "A"]
  x2 = Element "a"
              [("x","1")]
              [Element "b" [] [Text "A"],
              Element "b" [] [Text "B"]]
  x3 = Element "a"
              []
              [Element "b"
                      []
                      [Element "c"
                                [("att","att1")]
                                [Text "text1"],
                        Element "c"
                                [("att","att2")]
                                [Text "text2"]],
              Element "b"
                      []
                      [Element "c"
                                [("att","att3")]
                                [Text "text3"],
                        Element "d"
                                []
                                [Text "text4"]]]

  casablanca :: String
  casablanca
    = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942</year>\n</film>\n\n\n"

  casablancaParsed :: XML
  casablancaParsed
    = Element "film"
              [("title","Casablanca")]
              [Text "\n  ",
              Element "director" [] [Text "Michael Curtiz"],
              Text "\n  ",
              Element "year" [] [Text "1942"],
              Text "\n"]



