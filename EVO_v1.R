
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
    selected_rows = sample(x = 1:population_size, participants_count)
    # get best from sample:
    winner = max(fitness_vector[selected_rows])
    # insert best from sample into new_population
    new_population[i,] = population[winner,]
  }
  return(new_population)
  #runs a tournament and returns a the winners as a matrix
}

cross_over <- function(){
  #TODO
  #cross-over
}

mutate <- function(ind, mutation_rate){
  dim <- length(ind)
  mutations <- seq(0, 0, length.out = dim)
  while (sum(mutations) == 0){
    mutations <- sample(c(0, 1), dim, prob = c(1-mutation_rate, mutation_rate), replace = TRUE)
  }
  as.integer( xor(ind, mutations) )
}

terminate <- function(target_hit, ){
  # uses target_hit to evaluate if done
}


GA <- function(IOHproblem) {
  #establish mutation rate
  mutation_rate =  1 / IOHproblem$dimension
  # initiate initial parent population:
  parents = matrix()
  population_score = eval_population()
  # find out target_hit function inner workings
  while (IOHproblem$target_hit()) {
    best_parents = tournament()
    children = cross_over()
    children =  mutate(children)
    best_children = tournament()
    }
  list(xopt = best, fopt = fopt)
}


function (dimension, obj_func, target = NULL, lambda_ = 2, budget = NULL, 
          set_parameters = NULL) 
{
  if (is.null(budget)) 
    budget <- 100 * dimension
  parent <- sample(c(0, 1), dimension, TRUE)
  best <- parent
  r <- 2
  fopt <- obj_func(parent)
  budget <- budget - 1
  if (is.function(set_parameters)) 
    set_parameters(r)
  target_hit <- function() {
    if (is.null(target)) 
      return(FALSE)
    else return(target <= fopt)
  }
  while (budget > 0 && !target_hit()) {
    selected_r <- r
    selected_obj <- -Inf
    for (i in 1:lambda_) {
      offspring <- parent
      if (i <= lambda_/2) {
        mutation_rate = r/2/dimension
      }
      else {
        mutation_rate = 2 * r/dimension
      }
      offspring <- mutate(offspring, mutation_rate)
      v <- obj_func(offspring)
      if (v >= fopt) {
        fopt <- v
        best <- offspring
      }
      if (v >= selected_obj) {
        selected_obj = v
        selected_r = mutation_rate * dimension
      }
      budget <- budget - 1
      if (budget == 0) 
        break
    }
    parent <- best
    if (runif(1) > 0.5) {
      r = selected_r
    }
    else {
      if (runif(1) > 0.5) {
        r = r/2
      }
      else {
        r = 2 * r
      }
    }
    if (r < 2) 
      r = 2
    if (r > dimension/4) 
      r = dimension/4
    if (is.function(set_parameters)) 
      set_parameters(r)
  }
  list(xopt = best, fopt = fopt)
}