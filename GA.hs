module GA
( runGeneticAlgorithm
, Chromosome
) where

import Data.List
import Data.Random.Extras
import System.Random

-- Defines the functions that a user-supplied Chromosome-deriving class
-- must implement
class (Ord c) => Chromosome c where
    crossover :: c -> c -> c -- Crossover function
    mutate :: c -> c -- Crossover function
    computeFitness :: c -> Float

-- Define a default chromosome type of the form (fitness, params)
data DefaultChromo paramsType = DefaultChromo { fitness :: Float
                                              , params :: paramsType
                                              } deriving (Show, Eq)

-- Make chromosome comparable when it is of the form (fitness, params)
instance Eq p => Ord (DefaultChromo p) where
    compare (DefaultChromo fitness1 _) (DefaultChromo fitness2 _) = compare fitness1 fitness2

-- Primary interface into Genetic Algorithm code, runs a genetic algorithm
-- for nGen generations on an initialized population using pCrossover and pMutation 
runGeneticAlgorithm :: (Chromosome a) => [a] -> Int -> Float -> Float -> a
runGeneticAlgorithm pop nGen pCrossover pMutation
    | nGen <= 0 = head pop
    | otherwise = runGeneticAlgorithm sortedPop (nGen-1) pCrossover pMutation
    where popWithChildren = mutateAll pMutation $ crossoverAll pCrossover pop
          sortedPop = survivors $ sort popWithChildren

-- Performs crossover on complete population to produce an offspring equal in
-- size to the population, and then append this offpsring to the initial pop
crossoverAll :: (Chromosome a) => Float -> [a] -> [a]
crossoverAll _ [] = []
crossoverAll pCrossover pop = pop ++ children
    where children = [ if rand < pCrossover then crossover (tS pop) (tS pop) else tS pop | _ <- pop ]
          tS = tournamentSelection
          rand = randomRIO (1, 10)

-- Mutates all members of the population using pMutation
mutateAll :: (Chromosome a) => Float -> [a] -> [a]
mutateAll _ [] = []
mutateAll pMutation pop = [ if rand < pMutation then mutate ch else ch | ch <- pop ]
    where rand = randomRIO (1, 10)

-- Performs mu+lambda selection (taking the top half of the 
survivors :: (Chromosome a) => [a] -> [a]
survivors pop = take ((length pop) / 2) pop

-- Performs tournament selection on the given population
tournamentSelection :: (Chromosome a) => [a] -> a
tournamentSelection [] = Nothing
tournamentSelection pop = max $ sample 5 pop
