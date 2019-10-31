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
  fitness = c()
  for(i in 1:(dim(population)[1])){
    fitness = c(fitness, obj_func(population[i,]))
  }
  return(fitness)
}

the_other_eval_function <- function(genes,geneLength){
  ### genes is a single instance with the function takes as an input for evaluating
  ### geneLength is the siz eof the isntance
  fitness = 0;
  for (i in 1:geneLength) {
    if (genes[i] == 1) {
      ++fitness;
    }
  }
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
  first_cross_over = sample(2:as.integer(length(parent_a) / 2), 1)
  child_matrix[1,] = c(parent_b[1:(first_cross_over - 1)],parent_a[first_cross_over:length(parent_a)])
  child_matrix[2,] = c(parent_a[1:(first_cross_over - 1)],parent_b[first_cross_over:length(parent_a)])
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
  as.integer( xor(ind, mutations) )
}

mutate_pop <- function(population, mutation_rate){
  mutated_population = apply(population, 2, function(x) mutate(x, mutation_rate = mutation_rate))
  return(mutated_population)
}

#terminate <- function(target_hit, ){
#  # uses target_hit to evaluate if done
#}


GA <- function(IOHproblem) {
  print(IOHproblem$function_id)
  #establish mutation rate
  mutation_rate =  1 / IOHproblem$dimension
  # initiate initial parent population:
  # population_score = eval_population()
  parents = initiate_parents(IOHproblem$dimension, 100)
  # find out target_hit function inner workings
  best_parents = tournament(IOHproblem$obj_func, parents)
  iterations = 0
  budget = 50000
  while((IOHproblem$target_hit() == 0) && (iterations < budget)){
    iterations = iterations + 1
    children = cross_over(best_parents, 100)
    parents = mutate_pop(children, mutation_rate = mutation_rate)
    best_parents = tournament(IOHproblem$obj_func, parents, 15)
    #IOHproblem$fopt = max(IOHproblem$obj_func(best_parents))
  }
  return(list(xopt = best_parents, fopt = IOHproblem$fopt))
}
benchmark_algorithm(GA, functions = seq(3),repetitions = 2, instances = rep(1, 23), dimensions = rep(100, 23), algorithm.name = "TestRun", cdat = T, data.dir = "./data")

# TODO
# Add budget!
