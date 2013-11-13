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
         
report <- function(gen, repr.results, goal.reached){
  c(gen, repr.results, goal.reached)
}

base.reporting.fn <- function(pop, mutation, cross, elite)
{
  fitness.stats <- create.fitness.stats(pop)
    
  #We combine these measures for efficiency, but it doesn't really make sense to keep them together, so we clear it
  maxFit = fitness.stats$maxFit
  fitness.stats$maxFit = NULL
    
  mutation.stats <- create.mutation.stats(mutation)
  crossover.stats <- create.crossover.stats(crossover)
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
  fitness.env$max = which.max(fitnesses)
  fitness.env$min = which.min(fitnesses)
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
  mutation.counts = vector("list", length(mutation))
  for (i in 1:length(mutation))
  {
    mutation.counts[i] = mutation[[i]]@returnList$mutation.count
  }

  mutation.env = new.env()
  
  #TODO problem with mutation.counts, need ot figure out what...
  if (FALSE)
  {
  
  mutation.env$max = which.max(mutation.counts)
  mutation.env$min = which.min(mutation.counts)
  mutation.env$SD = sd(mutation.counts)
  mutation.env$mean = mean(mutation.counts)
  mutation.env$quantile = quantile(mutation.counts)
  mutation.env$median = mutation.env$quantile[3]
  mutation.env$skew = skewness(mutation.counts)
  mutation.env$kurtosis = kurtosis(mutation.counts)
  }
  
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
  elitism.env$max = which.max(elite.fitnesses)
  elitism.env$min = which.min(elite.fitnesses)
  elitism.env$SD = sd(elite.fitnesses)
  elitism.env$mean = mean(elite.fitnesses)
  elitism.env$quantile = quantile(elite.fitnesses)
  elitism.env$median = elitism.env$quantile[3]
  elitism.env$skew = skewness(elite.fitnesses)
  elitism.env$kurtosis = kurtosis(elite.fitnesses)
  
  return(elitism.env)
}

create.crossover.stats <- function(crossover)
{
  numCrossover = 0
  #TODO Crossover stats are not being adequately saved, need to save.
  #for(i in 1:length(crossover))
  #{
  #  numCrossover = numCrossover + crossover[[i]]
  #}
  
  crossover.env = new.env()
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
  print(GA.env$reported.data)
}


#OLD Code for printing GA, will probably be removed
arg.list.name <- function(obj.name){
  length(grep("*.args", obj.name)) > 0
}

controller.name <- function(obj.name){
  length(grep("*.controller", obj.name)) > 0
}
#TODO - Update to make work!
print.ga.env <- function(env){
  for(obj.name in objects(env)){
    if(!arg.list.name(obj.name) && !controller.name(obj.name)){
      value <- env[[obj.name]]
      if(!is.function(value) && !is.environment(value))
        cat(obj.name, "=", value, "\n")
    }
  }
}