module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush, star, carpet, oana, magic :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

type TState
 = (TurtleState, Colour)


colours :: [Colour]
colours
 = [black, blue, green, cyan, red, magenta, yellow, white]

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (x, y, z)
 = x
-- |Returns the base string for the given system.
base :: System -> String
base (x, y, z)
 = y

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (x, y, z)
 = z


-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar ch (r : rs)
 | ch == a = b
 | otherwise = lookupChar ch rs
 where
  (a, b) = r

-- |Expand a command once using the given set of rules.


expandOne :: Rules -> String -> String
expandOne r (x : []) = lookupChar x r
expandOne r (x : xs) = (lookupChar x r) ++ (expandOne r xs)

expandOne2 :: Rules -> String -> String
-- Another implementation for function expandOne using list comprehension
expandOne2 r (l : ls) = concat [lookupChar x r | x <- (l : ls)]


-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand r x n
 | n == 0 = x
 | n == 1 = expandOne r x
 | otherwise = expand r (expandOne r x) (n - 1)

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move ch t x
 | ch == 'L' = ((a, b), (c + x))
 | ch == 'R' = ((a, b), (c - x))
 | otherwise = ((a + cos alfa, b + sin alfa), c)
 where
 ((a, b), c) = t
 alfa = c * (pi / 180)

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
trace :: String -> Float -> Colour -> [ColouredLine]
trace s ang col = trace' ((0.0, 0.0), 90.0) s []
 where
 trace' :: TurtleState -> String -> [TurtleState] -> [ColouredLine]
 trace' t [] list = []
 trace' t (x : xs) list
  | x == 'F' = (posi, posf, col) : trace' (posf, th) xs list
  | x == 'L' || x == 'R' = trace' (posi, c) xs list
  | x == '[' = trace' t xs (t : list)
  | x == ']' = trace' l xs ls
  where
  (posf, c) = move x t ang
  (l : ls) = list
  (posi, th) = t

trace2 :: String -> Float -> Colour -> [ColouredLine]
-- multicolour the graphic
trace2 s ang col = trace2' (((0.0, 0.0), 90.0), col) s [] (cycle (col : colours))
 where
 trace2' :: TState -> String -> [TState] -> [Colour] -> [ColouredLine]
 trace2' t [] list cl = []
 trace2' t (x : xs) list cl
  | x == 'F' = (posi, posf, m) : trace2' ((posf, th), clr) xs list cl
  | x == 'L' || x == 'R' = trace2' ((posi, c), clr) xs list cl
  | x == '[' = trace2' ((posi, th), m')  xs (t : list) cl
  | x == ']' = trace2' l xs ls ms
  where
  (posf, c) = move x (posi, th) ang
  (l : ls) = list
  ((posi, th), clr) = t
  (m : (m' : ms)) = cl

trace3 :: String -> Float -> [Colour] -> [ColouredLine]
-- colour the graphic and fading to black the last colour of the list
trace3 s ang col = trace3' ((0.0, 0.0), 90.0) s []
 where
 trace3' :: TurtleState -> String -> [TurtleState] -> [ColouredLine]
 trace3' t [] list = []
 trace3' t (x : xs) list
  | (x == 'F') && (lngt > lg)
   = (posi, posf, col !! lg) : trace3' (posf, th) xs list
  | (x == 'F') && (lngt <= lg) && (c1 - d < 0) && (c2 - d < 0) && (c3 - d < 0)
   = (posi, posf, black) : trace3' (posf, th) xs list
  | (x == 'F') && (lngt <= lg)
   = (posi, posf, (c1 - d, c2 - d, c3 - d)) : trace3' (posf, th) xs list
  | x == 'L' || x == 'R' = trace3' (posi, c) xs list
  | x == '[' = trace3' (posi, th)  xs (t : list)
  | x == ']' = trace3' l xs ls
  where
  (posf, c) = move x t ang
  (l : ls) = list
  (posi, th) = t
  lngt = length col
  (c1, c2, c3) = col !! (lngt - 1)
  d = 0.15 * (fromIntegral (lg - lngt))
  lg = length list


lengthlist :: String -> Float -> Int
-- computes the number of levels of a graphic
lengthlist s ang = maximum (lengthlist' ((0.0, 0.0), 90.0) s [])
 where
 lengthlist' :: TurtleState -> String -> [TurtleState] -> [Int]
 lengthlist' t [] list = []
 lengthlist' t (x : xs) list
  | x == 'F' = m : lengthlist' (posf, th) xs list
  | x == 'L' || x == 'R' = lengthlist' (posi, c) xs list
  | x == '[' = lengthlist' t xs (t : list)
  | x == ']' = lengthlist' l xs ls
  where
  (posf, c) = move x t ang
  (l : ls) = list
  (posi, th) = t
  m = length list
  
listcolours :: Colour -> Colour -> Int -> [Colour]
-- builds a list of colours that fades one colour to the other
listcolours (a, b, c) (x, y, z) n
 = colors n
 where
 colors :: Int -> [Colour]
 colors 0 = []
 colors t = (r, s, p) : colors (t - 1)
  where
  t' = fromIntegral t
  n' = fromIntegral n
  d = n' - t'
  r = (a + (x - a) / n' * d)
  s = (b + (y - b) / n' * d)
  p = (c + (z - c) / n' * d)


trace4 :: String -> Float -> (Colour,Colour) -> [ColouredLine]
-- Pre: the string must contains "[" and "]"
-- colour the graphic and fading the first colour to the second colour
trace4 s ang col = trace' ((0.0, 0.0), 90.0) s [] (lengthlist s ang)
 where
 trace' :: TurtleState -> String -> [TurtleState] -> Int -> [ColouredLine]
 trace' t [] list n = []
 trace' t (x : xs) list n
  | x == 'F' = (posi, posf, (r, s, p)) : trace' (posf, th) xs list n
  | x == 'L' || x == 'R' = trace' (posi, c) xs list n
  | x == '[' = trace' (posi, th)  xs (t : list) n
  | x == ']' = trace' l xs ls n
  where
  (posf, c) = move x (posi, th) ang
  (l : ls) = list
  (posi, th) = t
  d = fromIntegral n
  ((c1, c2, c3), (c4, c5, c6)) = col
  y1 = (c4 - c1) / d
  y2 = (c5 - c2) / d
  y3 = (c6 - c3) / d
  nr = fromIntegral (length list)
  r = c1 + y1 * nr
  s = c2 + y2 * nr
  p = c3 + y3 * nr

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )


star
 = ( 72
   , "M-M-M-M-M"
   , [ ('M', "M-M++M+M-M-M")
     , ('+', "+")
     , ('-', "-")
     ]
   )

carpet
 = ( 90
   , "M"
   , [ ('M', "M+M-M-M-N+M+M+M-M")
     , ('N', "NNN")
     , ('+', "+")
     , ('-', "-")
     ]
   )

oana
 = (45
   , "M"
   , [ ('M', "M+[+M-]-[M+M]+[M]")
     , ('[', "[")
     , (']', "]")
     , ('+', "+")
     , ('-', "-")
     ]
   )

magic
 = (90
   , "M-M-M-M"
   , [ ('M', "M-M+M+M-M")
     , ('+', "+")
     , ('-', "-")
     ]
   )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]


lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem :: System -> Int -> Colour -> Colour -> IO ()
drawLSystem system n c1 c2
  = drawLines (trace3 (lSystem system n) (angle system) (listcolours c1 c2 nr))
  where
   nr = lengthlist (lSystem system n) (angle system)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)

drawLSystem3 :: System -> Int -> [Colour] -> IO ()
drawLSystem3 system n colour
  = drawLines (trace3 (lSystem system n) (angle system) colour)

drawLSystem4 :: System -> Int -> (Colour, Colour) -> IO ()
drawLSystem4 system n colour
  = drawLines (trace4 (lSystem system n) (angle system) colour)
