import qualified Data.Tree
import qualified Control.Exception

data Tree =
     Leaf | Node Int (Tree, Int) (Tree, Int)

type HeightedTree = (Tree, Int)

height :: Tree -> Int
height Leaf = 1
height (Node _ (t1, h1) (t2, h2)) = max h1 h2 + 1

isEmpty :: Tree -> Bool
isEmpty Leaf = True
isEmpty _ = False

foldTree :: b -> (Int -> b) -> (b -> b -> b -> b) -> Tree -> b
foldTree base _ _ Leaf = base
foldTree base nodeF combinator (Node n (t1, h1) (t2, h2)) =
  combinator (nodeF n) (recur t1) (recur t2)
  where
    recur t = foldTree base nodeF combinator t

inTree :: Int -> Tree -> Bool
inTree x = foldTree False (\n -> x == n) (\x y z -> x || y || z)

allT pred = foldTree True pred (\x y z -> x && y && z)

-- This function does not use the same structure as fold
isBst :: Tree -> Bool
isBst Leaf = True
isBst (Node n (t1, h1) (t2, h2)) =
  allT (< n) t1 &&
  allT (> n) t2 &&
  isBst t1 &&
  isBst t2

createTree :: Int -> Tree -> Tree -> Tree
createTree n t1 t2 = Node n (t1, height t1) (t2, height t2)

bf :: Tree -> Int
bf Leaf = 0
bf (Node _ (_, h1) (_, h2)) = h2 - h1

isBalanced :: Tree -> Bool
isBalanced t = case t of
  Leaf -> True
  Node n (t1, h1) (t2, h2) -> isBalanced t1 && isBalanced t2 && abs(bf(t)) <= 1

isAVL t = isBalanced t && isBst t

search :: Int -> Tree -> Bool
search x Leaf = False
search x (Node n (t1, h1) (t2, h2)) = if x == n then True
                                    else if x < n then search x t1
                                         else search x t2

insertBst :: Int -> Tree -> Tree
insertBst x Leaf = createTree x Leaf Leaf
insertBst x (Node n (t1, h1) (t2, h2)) =
  if x == n
  then createTree n t1 t2 -- Do nothing
  else if x < n then createTree n (insertBst x t1) t2
       else createTree n t1 (insertBst x t2)

rebalance :: Tree -> Tree
rebalance t =
  if abs (bf t) <= 1 then t
  else Control.Exception.assert (abs (bf t) == 2) (case t of
         Node n (tl, _) (tr, _) ->
           if bf t == 2 then
             if bf tr /= -1 then leftRotate t
             else leftRotate $ createTree n tl (rightRotate tr)
           else
             if bf tl /= 1 then rightRotate t
             else rightRotate $ createTree n (leftRotate tl) tr)

insertAvl :: Int -> Tree -> Tree
insertAvl x Leaf = insertBst x Leaf
insertAvl x (Node n (t1, h1) (t2, h2)) =
  rebalance $ if x == n then createTree n t1 t2
              else if x < n then createTree n (insertAvl x t1) t2
                   else createTree n t1 (insertAvl x t2)

leftRotate :: Tree -> Tree
leftRotate (Node x (aTree, _) (yTree, _)) =
  case yTree of
    Leaf -> error "Invalid LeftRotate"
    Node y (bTree, _) (cTree, _) ->
      createTree y (createTree x aTree bTree) cTree

rightRotate :: Tree -> Tree
rightRotate (Node y (xTree, _) (cTree, _)) =
  case xTree of
    Node x (aTree, _) (bTree, _) ->
      createTree x aTree (createTree y bTree cTree)

deleteAvl :: Int -> Tree -> Tree
deleteAvl x Leaf = error "Not in tree"
deleteAvl x (Node n (t1, h1) (t2, h2)) =
  let t' = if x == n then deleteAvlAtRoot (Node n (t1, h1) (t2, h2))
        else if x < n then createTree n (deleteAvl x t1) t2
        else createTree n t1 (deleteAvl x t2)
  in rebalance t'

deleteAvlAtRoot :: Tree -> Tree
deleteAvlAtRoot (Node _ (Leaf, _) (Leaf, _)) = Leaf
deleteAvlAtRoot (Node _ (Leaf, _) (t2, _)) = t2
deleteAvlAtRoot (Node _ (t1, _) (t2, _)) =
  let (predecessor, t1') = removePred t1
  in rebalance $ createTree predecessor t1' t2

removePred :: Tree -> (Int, Tree)
removePred (Node n (Leaf, _) (Leaf, _)) = (n, Leaf)
removePred (Node n (t1, _) (Leaf, _)) = (n, t1)
removePred (Node n (t1, _) (t2, _)) =
    let (p, t') = removePred t2
    in (p, rebalance $ createTree n t1 t')

toDataTree Leaf = Data.Tree.Node "Leaf" []
toDataTree (Node x (t1, _) (t2, _)) = Data.Tree.Node (show x) [toDataTree t1, toDataTree t2]

drawTree = Data.Tree.drawTree . toDataTree

insertManyBst = foldr insertBst Leaf
insertManyAvl = foldr insertAvl Leaf

x = putStrLn . drawTree . insertManyAvl
y = putStrLn . drawTree . insertManyBst
rnums = [1 .. 1000000]

main = do
  print $ height $ insertManyAvl rnums
  print $ height $ insertManyBst rnums
  print $ all (\n -> search n (insertManyAvl rnums)) rnums
