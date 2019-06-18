import System.Random
import TernaryTree
import ExampleTree
import State

randomTree :: (RandomGen g) => TernaryTree a -> State g (TernaryTree Int)
randomTree Empty = return Empty
randomTree (Tree _ t1 t2 t3) = do
  randomNumber <- getRandom (1, 6)
  t1' <- randomTree t1
  t2' <- randomTree t2
  t3' <- randomTree t3
  return (Tree randomNumber t1' t2' t3')

getRandom :: (RandomGen g, Random a) => (a, a) -> State g a
getRandom range = State (\gen -> randomR range gen)

main = do
  randomGen <- getStdGen
  let State stateFunc = randomTree exampleTree
  let (tree, _) = stateFunc randomGen
  putStrLn $ prettyShow tree

-- Com o uso da mônada State, o gerador é implicitamente passado como argumento e retornado a cada chamada de 
-- getRandom e randomTree, tornando o código mais simples e limpo.