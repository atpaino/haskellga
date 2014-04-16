module GA
(
 runGeneticAlgorithm,
 Chromosome
)

-- Defines the functions that a user-supplied Chromosome-deriving class
-- must implement
class Chromosome c where
    crossover :: c c -> c -- Crossover function
    mutate :: c -> c -- Crossover function
    computeFitness :: c -> Float
    fitness :: c -> float

-- Primary interface into Genetic Algorithm code, runs a genetic algorithm
-- for nGen generations on an initialized population using pCrossover and pMutation 
runGeneticAlgorithm :: (Chromosome a, Numeric b, Float c) => [a] -> b -> c -> c -> a
runGeneticAlgorithm pop nGen pCrossover pMutation
    | nGen <= 0 = head pop
    | otherwise = runGeneticAlgorithm sortedPop (nGen-1) pCrossover pMutation
    where compChromo = \chromo1 chromo2 -> fitness chromo1 > fitness chromo2
          updatePop = survivors $ mutateAll pMutation $ crossoverAll pCrossover pop
          sortedPop = sortBy compChromo updatePop

-- Performs crossover on complete population to produce an offspring equal in
-- size to the population, and then append this offpsring to the initial pop
crossoverAll :: (Float a, Chromosome b) => a -> [b] -> [b]
crossoverAll _ [] = []
crossoverAll pCrossover pop = pop ++ children
    where children = [ if rand < pCrossover then crossover (tS pop) (tS pop) else tS pop | _ <- pop ]
          tS = tournamentSelection

-- Mutates all members of the population using pMutation
mutateAll :: (Float a, Chromosome b) => a -> [b] -> [b]
mutateAll _ [] = []
mutateAll pMutation pop = [ if rand < pMutation then mutate ch else ch | ch <- pop ]

-- Performs tournament selection on the given population
tournamentSelection :: (Chromosome a) => [a] -> a
tournamentSelection [] = Nothing
tournamentSelection pop = maxBy (\ch -> fitness ch) $ takeRand 5 pop
