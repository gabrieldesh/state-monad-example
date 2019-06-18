import System.Random
import TernaryTree
import ExampleTree

randomGen = mkStdGen 0

randomTree :: TernaryTree a -> TernaryTree Int
randomTree Empty = Empty
randomTree (Tree _ t1 t2 t3) = let (randomNumber, _) = randomR (1, 6) randomGen
                               in  Tree randomNumber (randomTree t1)
                                                     (randomTree t2)
                                                     (randomTree t3)

main = putStrLn $ prettyShow $ randomTree exampleTree

-- Não funciona, pois randomR está sendo aplicada sempre aos mesmos argumentos, então retornará sempre o mesmo valor.