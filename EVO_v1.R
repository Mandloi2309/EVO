library(IOHexperimenter)
initiate_parents <- function(dimension, population_size){
  # get first parent population
  population = matrix(data = NA, nrow = population_size, ncol = dimension)
  for(i in 1:(dim(population)[1])){population[i,] = sample(c(0, 1), dimension, TRUE)}
  return(population)
}

eval_population <- function(obj_func, population){
  # This function takes an evaluation function and
  # a population matrix and returns their fitness in a vector
  fitness = obj_func(population)
  return(fitness)
}

tournament <- function(obj_func, population, count=50){
  # get fitness score for each instance:
  fitness_vector = eval_population(obj_func, population)
  # get instance size:
  population_size = dim(population)[1]
  # Get the number of participants
  participants_count =  as.integer(population_size/10)
  # initalize new population matrix
  new_population = matrix(data = NA, nrow = count, ncol = dim(population)[2])
  # run tournaments:
  for(i in 1:count){
    # get sample:
    population_size = dim(population)[1]
    selected_rows = sample(x = 1:population_size, participants_count)
    selected_pops = population[selected_rows,]
    # get best from sample:
    winner_score = max(fitness_vector[selected_rows])
    winner = selected_pops[fitness_vector[selected_rows] == winner_score,]
    winner_index = selected_rows[fitness_vector[selected_rows] == winner_score]
    # if more than one winner select one randomly
    if (!is.null(dim(winner))){
      winnerid = sample(seq(1,dim(winner)[1]), size = 1)
      winner_index = selected_rows[fitness_vector[selected_rows] == winner_score][winnerid]
      winner = winner[winnerid,]
    }
    # insert best from sample into new_population
    new_population[i,] = winner
  }
  return(new_population)
  # runs a tournament and returns a the winners as a matrix
}

actual_cross_over <- function(parent_a, parent_b){
  # This function takes two parents as inout and performs crossover 
  # results two children
  child_matrix = matrix(NA, nrow = 2, ncol = length(parent_a))
  # select a random position to perform point crossover
  cross_over = sample(2:as.integer(length(parent_a) / 2), 1)
  # Child 1: Parent B till the crossover point and parent A from the cross over point to end
  child_matrix[1,] = c(parent_b[1:(cross_over - 1)],parent_a[cross_over:length(parent_a)])
  # Child 2: Parent A till the crossover point and parent B from the cross over point to end
  child_matrix[2,] = c(parent_a[1:(cross_over - 1)],parent_b[cross_over:length(parent_a)])
  return(child_matrix)
}

cross_over <- function(population, population_size){
  # This funcrion takes population as inout and performs crossover until
  # number of children equals the population size
  children = matrix(NA, nrow = population_size, ncol = dim(population)[2])
  j = 1
  #Selects two parents from the population randomly
  while(is.na(children[nrow(children)])){
    parent_a = population[sample(nrow(population), 1),]
    parent_b = population[sample(nrow(population), 1),]
    x =actual_cross_over(parent_a, parent_b)
    children[j:(j+1),] = actual_cross_over(parent_a, parent_b)
    j = j + 2
  }
  return(children)
}

mutate <- function(ind, mutation_rate){
  # Inout parameters ind: Individual from the population which will get mutated
  # with the probabilty given as mutation_rate
  # mutation_rate: is the probaility of a bit to be 1
  dim <- length(ind)
  mutations <- seq(0, 0, length.out = dim)
  while (sum(mutations) == 0){
    mutations <- sample(c(0, 1), dim, prob = c(1-mutation_rate, mutation_rate), replace = TRUE)
  }
  return(as.integer( xor(ind, mutations) ))
}

mutate_pop <- function(population, mutation_rate){
  # Mutates each individual using mutate function
  mutated_population = apply(population, 2, function(x) mutate(x, mutation_rate = mutation_rate))
  return(mutated_population)
}

GA <- function(IOHproblem){
  # Establish mutation rate
  # Mutation rate 1/100, 2/100, 10/100
  mutation_rate =  1 / IOHproblem$dimension 
  # Initiate initial parent population:
  parents = initiate_parents(IOHproblem$dimension, 100)
  # First selection of best parents
  best_parents = tournament(IOHproblem$obj_func, parents)
  iterations = 0
  if(iterations == 0){
    cat(paste('\rFunction: ',IOHproblem$function_id))
  }
  budget = 50000
  while((IOHproblem$target_hit() == 0) && (iterations < budget)){
    iterations = iterations + 100
    children = cross_over(best_parents, 100)
    parents = mutate_pop(children, mutation_rate = mutation_rate)
    best_parents = tournament(IOHproblem$obj_func, parents, 15) # Selection 15, 25
  }
  return(list(xopt = best_parents, fopt = IOHproblem$fopt))
}

benchmark_algorithm(GA, functions = seq(23),repetitions = 20, instances = 1, dimensions = 100,
                    algorithm.name = "karolis_tushar_GA_config_2_MR_1", data.dir = "./karolis_tushar_GA_config_2_MR_1")


#################################################
### Configurations tried:
## Config 1: Selection 25 MR: 1/100 
## Config 2: Selection 15 MR: 1/100 
## Config 3: Selection 25 MR: 2/100
## Config 4: Selection 15 MR: 2/100
## Config 5: Selection 25 MR: 10/100