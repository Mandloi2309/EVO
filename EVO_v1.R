
# get first parent population
initiate_parents <- function(dimension, population_size){
  # dimension == vector lenght / problem dimention
  #TODO
  population = matrix(data = NA, nrow = population_size, ncol = dimension)
  for(i in 0:(dim(population)[1])){population[i,] = sample(c(0, 1), dimension, TRUE)}
  return(population)
}

eval_population <- function(){
  #TODO
  #EVAL FUNCTION ?
}

tournament <- function(){
  # batch evalution or one-by-one?
  eval_population()
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
    parents = combine()
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