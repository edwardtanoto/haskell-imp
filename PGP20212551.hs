-- Edward Tanoto 20212551

posOnMap :: [[Char]] -> (Int,Int) -> Bool
posOnMap vss (x,y) = x>=0 && x<length(vss)
 && y>=0 && y<length(vss!!x)

-- Question 1
getAtPos :: [[Char]] -> (Int,Int) -> Char
getAtPos vss (x,y) | posOnMap vss (x,y) = (vss!!x)!!y
 | otherwise = '#' 

-- Question 2
setAtIndex :: [i] -> Int -> i  -> [i] 
setAtIndex [] _ _ = []
setAtIndex vs x y | x >= length vs = vs
setAtIndex vs x y = concat [(take x vs), [y] , (drop (x+1) vs)]

-- Question 3

blockAtPos :: [[Char]] -> (Int,Int) -> [[Char]]
blockAtPos [] (_,_) = []
blockAtPos vss (x,y) | x > length vss = vss 
                     | y > (length (vss!!x)) = vss 
blockAtPos vss (x,y) = concat [(take x vss), ([setAtIndex (vss!!x) y '#']), (drop (x+1) vss)] 

-- Question 4

getIndex :: [[Char]] -> (Int,Int) -> (Int,Int) 
getIndex vss (x,y) | ((getAtPos vss (x,y) == 'A' && posOnMap vss (x,y) == True) = (x,y)
                    |  posOnMap vss (x,y) == False = getIndex vss (x,y+1)  
                    |  otherwise = getIndex vss (x+1 ,0)

findStart :: [[Char]] -> (Int,Int) 
findStart vss = getIndex vss (0,0) 

legalPos :: [[Char]] -> (Int,Int) -> Bool
legalPos vss (x,y) = posOnMap vss (x,y) && getAtPos vss (x,y) /= '#'

-- Question 6 

movePos :: (Int,Int) -> Char -> (Int,Int)
movePos (x,y) 'N' = (x-1,y)
movePos (x,y) 'S' = (x+1,y)
movePos (x,y) 'E' = (x,y+1)
movePos (x,y) 'W' = (x,y-1)

legalMoves :: [[Char]] -> (Int,Int) -> [Char]
legalMoves nss (x,y) = concat[
        ([p | p <- ['N'], legalPos nss (movePos (x,y) 'N') == True]),
        ([q | q <- ['S'], legalPos nss (movePos (x,y) 'S') == True]), 
        ([r | r <- ['E'], legalPos nss (movePos (x,y) 'E') == True]),
        ([s | s <- ['W'], legalPos nss (movePos (x,y) 'W') == True]),
    ]

-- Question 7
valueAtPos :: [[Char]] -> (Int,Int) -> Int 
valueAtPos [] (_,_) = 0 
valueAtPos vss (x,y) | getAtPos vss (x,y) == '1' = 1
                     | getAtPos vss (x,y) == '2' = 2
                     | getAtPos vss (x,y) == '3' = 3
                     | otherwise = 0
