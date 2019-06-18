import System.Random
import TernaryTree
import ExampleTree

randomGen = mkStdGen 0

randomTree :: (RandomGen g) => TernaryTree a -> g -> (TernaryTree Int, g)
randomTree Empty gen = (Empty, gen)
randomTree (Tree _ t1 t2 t3) gen = let (randomNumber, gen1) = randomR (1, 6) gen
                                       (         t1', gen2) = randomTree t1 gen1
                                       (         t2', gen3) = randomTree t2 gen2
                                       (         t3', gen4) = randomTree t3 gen3
                                   
                                   in  (Tree randomNumber t1' t2' t3', gen4)

main = putStrLn $ prettyShow $ fst $ randomTree exampleTree randomGen

-- Funciona, mas é preciso guardar e repassar o gerador a cada chamada de randomR e randomTree.