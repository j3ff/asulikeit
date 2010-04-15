module MyTree where
{-
    (
        joinTrees,
        reduceForest,
        forest
    ) where
-}

import Data.Tree
import Data.List
import Data.Maybe


pairs = [ ( "z", "q" ),
          ( "x", "l" ),
          ( "e", "h" ),
          ( "e", "j" ),
          ( "b", "a" ), 
          ( "d", "c" ), 
          ( "f", "e" ), 
          ( "f", "d" ), 
          ( "h", "g" ), 
          ( "j", "i" ), 
          ( "q", "V" ),
          ( "z", "Y" ),
          ( "d", "b" ) ]

forest = map makeTree pairs

reduceForest :: Forest String -> Forest String
reduceForest f =
    let
        groups = groupBy sameParent ( sortBy compareParent f )
        uniteSiblings = foldl1' joinTrees'
        unitedSiblings = map uniteSiblings groups
    in
        reduceForest' unitedSiblings
        
reduceForest' :: Forest String -> Forest String
reduceForest' [] = []
reduceForest' [x] = [x]
reduceForest' ( x : xs ) =
    let
        m = map ( joinTrees x ) xs
        hasJoin = find isJust m
        r = resolver m xs
    in
        if isJust hasJoin 
        then reduceForest' r
            else x : reduceForest' r
        

resolver :: [ Maybe ( Tree String ) ] -> [ Tree String ] -> [ Tree String ]
resolver [] _ = []
resolver _ [] = []
resolver ( Nothing : ms ) ( t : ts ) = ( t : ( resolver ms ts ) )
resolver ( m : ms ) ( t : ts ) = ( ( fromJust m ) : ts )

compareParent :: Tree String -> Tree String -> Ordering
compareParent x y = compare ( rootLabel x ) ( rootLabel y )


sameParent :: Tree String -> Tree String -> Bool
sameParent x y = ( rootLabel x ) == ( rootLabel y )

makeTree :: ( String, String ) -> Tree String
makeTree ( parent, child ) = Node parent [ Node child [] ]

joinTrees :: Tree String -> Tree String -> Maybe ( Tree String )
joinTrees a b
    | ( findInTree ( rootLabel b ) a ) = Just ( joinTrees' a b )
    | ( findInTree ( rootLabel a ) b ) = Just ( joinTrees' b a )
    | otherwise = Nothing

joinTrees' :: Tree String -> Tree String -> Tree String
joinTrees' a b
    | ( rootLabel a ) == ( rootLabel b) =
        Node  ( rootLabel a ) ( ( subForest a ) ++ ( subForest b ) )
    | otherwise =
        Node ( rootLabel a ) ( joinForest b ( subForest a ) )

joinForest :: Tree String -> Forest String -> Forest String
joinForest t [] = [ t ]
joinForest t f
    | ( findInTree ( rootLabel t ) ( head f ) ) =  
        ( joinTrees' ( head f ) t ) : ( tail f ) 
    | otherwise = 
        ( head f ) : ( joinForest t ( tail f ) ) 
        

findInTree :: String -> Tree String -> Bool
findInTree s t =
    isJust ( find ( \x -> s == x ) ( flatten t ) )

