#!/usr/bin/ruby

require 'knapsack'
require 'yaml'

WeightLimit=200
Generations=10
PopulationSize=20

items = YAML::load( File.open( "items.yaml" ) )

recommendedItemsToKeep, totalValue, totalWeight = Knapsack::Solver.new(items, WeightLimit, Generations, PopulationSize).solve

puts "The solver recommends you keep these items: #{recommendedItemsToKeep.join(',')}"
puts "Total Weight: #{totalWeight}"
puts "Total Value: #{totalValue}"