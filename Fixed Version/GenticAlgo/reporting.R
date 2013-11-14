setClass("gen.report", representation(maxFit = "chromosome", fitness = "environment", mutation = "environment", crossover = "environment", elite = "environment"))
setMethod("initialize", 
           signature = "gen.report",
           definition = function(.Object, maxFit, fitness, mutation, crossover, elite) {
             .Object@maxFit <- maxFit
             .Object@fitness <- fitness
             .Object@mutation <- mutation
             .Object@crossover <- crossover
             .Object@elite <- elite
             .Object
           })

setClass("base.report", representation(gen = "numeric", currentGen.results = "ANY", goal.reached = "logical"))
         
report <- function(gen, currentGen.results, goal.reached){
  new("base.report", gen = gen, currentGen.results = currentGen.results, goal.reached = as.logical(goal.reached))
}

base.reporting.fn <- function(pop, mutation, cross, elite)
{
  fitness.stats <- create.fitness.stats(pop)
    
  #We combine these measures for efficiency, but it doesn't really make sense to keep them together, so we clear it
  maxFit = fitness.stats$maxFit
  fitness.stats$maxFit = NULL
    
  mutation.stats <- create.mutation.stats(mutation)
  crossover.stats <- create.crossover.stats(cross)
  elite.stats <- create.elite.stats(elite)
    
  new("gen.report", maxFit, fitness.stats, mutation.stats, crossover.stats, elite.stats)
}

create.fitness.stats <- function(pop)
{
  maxFit = pop[[1]]
  fitnesses = vector("list", length(pop))
  for (i in 1:length(pop))
  {
    if (maxFit@fitness$value < pop[[i]]@fitness$value)
    {
      maxFit = pop[[i]]
    }
    
    fitnesses[i] = pop[[i]]@fitness$value
  }
  
  fitnesses = as.vector(fitnesses, mode = "numeric")
  
  fitness.env = new.env()

  fitness.env$maxFit = maxFit
  fitness.env$max = max(fitnesses)
  fitness.env$min = min(fitnesses)
  fitness.env$SD = sd(fitnesses)
  fitness.env$mean = mean(fitnesses)
  fitness.env$quantile = quantile(fitnesses)
  fitness.env$median = fitness.env$quantile[3]
  fitness.env$skew = skewness(fitnesses) #requires package "moments"
  fitness.env$kurtosis = kurtosis(fitnesses)
  
  return(fitness.env)
}

create.mutation.stats <- function(mutation)
{
  numMutations = 0
  #Get total mutations
  for (i in 1:length(mutation))
  {
    numMutations = numMutations + length(returnList(mutation[[i]]))
  }

  mutation.counts = vector("list", numMutations)
  mutationIndexCounter = 1
  for (i in 1:length(mutation))
  {
    #We are just consider mutation of rest, p1 and p2 as the same, if we want to split them up we'll need to do so here
    for (j in 1:length(returnList(mutation[[i]])))
    {
      mutation.counts[mutationIndexCounter] = mutation[[i]][[j]]@mutation.count
      
      mutationIndexCounter = mutationIndexCounter + 1
    }
  }

  mutation.counts = as.vector(mutation.counts, mode = "numeric")
  
  mutation.env = new.env()
  mutation.env$max = max(mutation.counts)
  mutation.env$min = min(mutation.counts)
  mutation.env$SD = sd(mutation.counts)
  mutation.env$mean = mean(mutation.counts)
  mutation.env$quantile = quantile(mutation.counts)
  mutation.env$median = mutation.env$quantile[3]
  mutation.env$skew = skewness(mutation.counts)
  mutation.env$kurtosis = kurtosis(mutation.counts)
  
  return(mutation.env)
}

create.elite.stats <- function(elite)
{
  elite.maxFit = elite[[1]]
  if (length(elite) > 1)
  {
    elite.fitnesses = vector("list", length(elite))
    for (i in 1:length(elite))
    {
      if (elite.maxFit@fitness$value < elite[[i]]@fitness$value)
      {
        elite.maxFit = elite[[i]]
      }
      
      elite.fitnesses[i] = elite[[i]]@fitness$value
    }
  }
  
  elite.fitnesses = as.vector(elite.fitnesses, mode = "numeric")
  
  elitism.env = new.env()
  elitism.env$maxFit = elite.maxFit
  elitism.env$max = max(elite.fitnesses)
  elitism.env$min = min(elite.fitnesses)
  elitism.env$SD = sd(elite.fitnesses)
  elitism.env$mean = mean(elite.fitnesses)
  elitism.env$quantile = quantile(elite.fitnesses)
  elitism.env$median = elitism.env$quantile[3]
  elitism.env$skew = skewness(elite.fitnesses)
  elitism.env$kurtosis = kurtosis(elite.fitnesses)
  
  return(elitism.env)
}

create.crossover.stats <- function(cross)
{
  #TODO Get crossover points, not currently saved in cross object
  
  crossover.env = new.env()
  crossover.env$numCrossovers = length(returnList(cross))
  print(crossover.env$numCrossovers)
  
  return(crossover.env)
}

#REPORT ALL THE THINGS!!!
reportAll.reporting.fn <- function(GA.env)
{
  reportAll.reporting.fn <- function(pop, mutation, cross)
  {
    results = c(pop, mutation, cross)
    
    results
  }
}
         
print.report <- function(GA.env)
{
  cat("Generations:",length(GA.env$reported.data), "Goal Reached:", GA.env$reported.data[[length(GA.env$reported.data)]]@goal.reached)
}
