import GA

-- Function to initialize the TSP chromosome
initTSPChromo :: Int -> DefaultChromo [Int]
initTSPChromo nCities = DefaultChromo { fitness = 0.0
                                      , params = randomPath
                                      }
    where randomPath = shuffle [1 .. nCities]

-- Test running the genetic algorithm for TSP
main = runGeneticAlgorithm population 100 0.95 0.01
    where population = [ initTSPChromo 10 | _ <- [..100] ]
