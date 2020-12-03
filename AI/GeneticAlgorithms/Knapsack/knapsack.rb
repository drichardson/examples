module Knapsack
    
    class Individual
        def initialize(items, fitness)
            self.items = items
            self.fitness = fitness
        end
        
        attr_accessor :items, :fitness
        
        def <=> (other)
            self.fitness <=> other.fitness
        end
    end
    
    class Solver
        
        def initialize(items, knapsackWeightLimit, generations, populationSize)
            @items = items
            @knapsackWeightLimit = knapsackWeightLimit
            @generations = generations
            @populationSize = populationSize
            @breedingPairsPerGeneration = populationSize / 4
        end
        
        def solve
            # An individual is a set if items that will fit in the knapsack. The population
            # are several different sets of items that will fit in the knapsack.
            # An individual is represented as an array of booleans. The array is the size of @items.
            # A true value means the individual contains that item. False means it does not.
            
            
            population = choosePopulationOfIndividualsAndEvaluateFitness
            
            @generations.times do |generation|
                puts "Generation #{generation}"
                bestFitIndividuals = selectBestFitIndividuals population
                offspring = breed bestFitIndividuals
                evaluateFitnessOfIndividuals offspring
                population = replaceLeastFitIndividualsWithNew population, offspring
            end
                        
            return @items.map {|item| item['name'] }, 1, 2
        end
        
        def choosePopulationOfIndividualsAndEvaluateFitness
            population = []
            
            @populationSize.times do
                weight = 0
                fitness = 0
                items = []
                @items.each do |item|
                    has = false
                    if rand > 0.5
                        if weight + item['weight'] <= @knapsackWeightLimit
                            has = true
                            weight += item['weight']
                            fitness += items['value']
                        end
                    end
                    items << has
                end
                
                population << Individual.new(items, fitness)
            end
            
            population
        end
        
        def selectBestFitIndividuals population
            population.sort.last(@breedingPairsPerGeneration * 2)
        end
        
        def breed individuals
            # Pair up individuals by randomly selecting 2 individuals from the array.
            
            # Cross each pair of individuals. Note: this needed be pairwise breeding, could use 3 or more as well.
            
            # Mutate offspring.
        end
        
    end
end