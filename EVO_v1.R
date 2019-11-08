library(IOHexperimenter)

# get first parent population
initiate_parents <- function(dimension, population_size){
  # dimension == vector lenght / problem dimention
  #TODO
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
  # batch evalution or one-by-one?
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
    if (!is.null(dim(winner))){
      winnerid = sample(seq(1,dim(winner)[1]), size = 1)
      winner_index = selected_rows[fitness_vector[selected_rows] == winner_score][winnerid]
      winner = winner[winnerid,]
    }
    # insert best from sample into new_population
    new_population[i,] = winner
  }
  return(new_population)
  #runs a tournament and returns a the winners as a matrix
}

actual_cross_over <- function(parent_a, parent_b){
  child_matrix = matrix(NA, nrow = 2, ncol = length(parent_a))
  cross_over_type = c(1,2,3)
  COP = sample(c(1,2,3),1,prob = seq(0.3,3)) #1: One point, 2: Two point, 3: uniform crossover
  switch (COP,
    1 = { #One point-crossover
      cross_over = sample(2:as.integer(length(parent_a) / 2), 1)
      child_matrix[1,] = c(parent_b[1:(cross_over - 1)],parent_a[cross_over:length(parent_a)])
      child_matrix[2,] = c(parent_a[1:(cross_over - 1)],parent_b[cross_over:length(parent_a)])
    }
    2 = { #Two-point crossover
      cross_over = sample(2:as.integer(length(parent_a) / 2), 1)
      cross_over_2 = sample((cross_over+1):as.integer(length(parent_a)), 1)
      child_matrix[1,] = c(parent_b[1:(cross_over - 1)],parent_a[cross_over:(cross_over_2-1)],parent_b[cross_over_2:length(parent_a)])
      child_matrix[2,] = c(parent_a[1:(cross_over - 1)],parent_b[cross_over:(cross_over_2-1)],parent_a[cross_over_2:length(parent_a)])
    }
    3 = { #Uniform crossover
      cross_over = sample(1:length(parent_a),(length(parent_a)/2))
      child_matrix[1,] = c(parent_b[cross_over],parent_a[-cross_over])
      child_matrix[2,] = c(parent_a[cross_over],parent_b[-cross_over])
    }
  )
  return(child_matrix)
}

cross_over <- function(population, population_size){
  children = matrix(NA, nrow = population_size, ncol = dim(population)[2])
  j = 1
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
  dim <- length(ind)
  mutations <- seq(0, 0, length.out = dim)
  while (sum(mutations) == 0){
    mutations <- sample(c(0, 1), dim, prob = c(1-mutation_rate, mutation_rate), replace = TRUE)
  }
  return(as.integer( xor(ind, mutations) ))
}

mutate_pop <- function(population, mutation_rate){
  mutated_population = apply(population, 2, function(x) mutate(x, mutation_rate = mutation_rate))
  return(mutated_population)
}

#terminate <- function(target_hit, ){
#  # uses target_hit to evaluate if done
#}


GA <- function(IOHproblem){
  #establish mutation rate
  mutation_rate =  10 / IOHproblem$dimension #Mutation rate 1/100, 2/100, 10/100
  # initiate initial parent population:
  parents = initiate_parents(IOHproblem$dimension, 100)
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
    best_parents = tournament(IOHproblem$obj_func, parents, 25) # Selection 15, 25
  }
  return(list(xopt = best_parents, fopt = IOHproblem$fopt))
}

benchmark_algorithm(GA, functions = seq(23),repetitions = 20, instances = 1, dimensions = 100,
                    algorithm.name = "karolis_tushar_GA_config_5_MR_10", data.dir = "./karolis_tushar_GA_config_5_MR_10")

### Config 1: Selection 25 MR: 1/100 
### Config 2: Selection 15 MR: 1/100 
### Config 3: Selection 25 MR: 2/100
### Config 4: Selection 15 MR: 2/100
### Config 5: Selection 25 MR: 10/100


# TODO