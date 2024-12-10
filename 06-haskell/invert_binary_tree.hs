data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)


invertTree :: Tree a -> Tree a
invertTree Empty = Empty
invertTree (Node value left right) =
    Node value (invertTree right) (invertTree left)


printTree :: Show a => Tree a -> IO ()
printTree tree = printTree' tree 0
  where
    printTree' Empty _ = return ()
    printTree' (Node value left right) level = do
        printTree' right (level + 1)
        putStrLn $ replicate (5 * level) ' ' ++ show value 
        printTree' left (level + 1)

main :: IO ()
main = do
    let tree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)
    putStrLn "Original Tree:"
    printTree tree
    print $ invertTree tree 
    putStrLn "Tree Structure:"
    printTree $ invertTree tree
