setClass("gen.report", representation(maxFit = "organism", minFit = "organism", fitness = "list", mutation = "list", crossover = "list", elite = "list", selection = "environment"))
setMethod("initialize", 
           signature = "gen.report",
           definition = function(.Object, maxFit, minFit, fitness, mutation, crossover, elite, selection=environment()) {
             .Object@maxFit <- maxFit
             .Object@minFit <- minFit
             .Object@fitness <- fitness
             .Object@mutation <- mutation
             .Object@crossover <- crossover
             .Object@elite <- elite
             .Object@selection <- selection
             .Object
           })

setClass("base.report", representation(gen = "numeric", currentGen.results = "ANY", goal.reached = "logical"))
         
report <- function(gen, currentGen.results, goal.reached){
  new("base.report", gen = gen, currentGen.results = currentGen.results, goal.reached = as.logical(goal.reached))
}

base.reporting.fn <- function(pop, mutation, cross, elite, selection=environment(), ...)
{
  fitness.stats = vector("list", length(pop))
  mutation.stats = vector("list", length(pop))
  crossover.stats = vector("list", length(pop))
  elite.stats = vector("list", length(pop))
  
  #Create stats for each population seperately
  for (i in 1:length(pop))
  {
    fitness.stats[[i]] <- create.fitness.stats(pop[[i]])
      
    #We combine these measures for efficiency, but it doesn't really make sense to keep them together, so we clear it
    maxFit = fitness.stats[[i]]$maxFit
    fitness.stats[[i]]$maxFit = NULL
    minFit = fitness.stats[[i]]$minFit
    fitness.stats[[i]]$minFit = NULL
      
    mutation.stats[[i]] <- create.mutation.stats(mutation[[i]])
    crossover.stats[[i]] <- create.crossover.stats(cross[[i]])
    if (!is.null(elite)) elite.stats[[i]] <- create.elite.stats(elite[[i]])
  } 
  #TODO - selection stats only works for 1 pop
  new("gen.report", maxFit, minFit, fitness.stats, mutation.stats, crossover.stats, elite.stats,selection)
}

create.fitness.stats <- function(pop)
{
  fitness.env = new.env()
  
  maxFit = pop[[1]]
  minFit = pop[[1]]
  fitnesses = vector("numeric", size(pop))
  
  #TODO - Handle frequencies for symbolic GAs other than binary
  frequency.of.0 = vector("numeric", size(pop))
  frequency.of.1 = vector("numeric", size(pop))
  
  fitness.env$entropy = 0
  
  for (i in 1:size(pop))
  {
    if (maxFit@fitness$value < pop[[i]]@fitness$value)
    {
      maxFit = pop[[i]]
    }
    if(minFit@fitness$value > pop[[i]]@fitness$value)
    {
      minFit = pop[[i]]
    }
    
    fitnesses[i] = pop[[i]]@fitness$value
    
    frequency.of.0[i] = sum(pop[[i]]@chromosome$genes==0)
    frequency.of.1[i] = sum(pop[[i]]@chromosome$genes==0)

    fitness.env$entropy = fitness.env$entropy + fitnesses[i]*log2(abs(fitnesses[i]))
  }
  fitness.env$test = fitnesses
  fitness.env$maxFit = maxFit
  fitness.env$minFit = minFit
  fitness.env$max = max(fitnesses)
  fitness.env$min = min(fitnesses)
  fitness.env$SD = sd(fitnesses)
  fitness.env$mean = mean(fitnesses)
  fitness.env$quantile = quantile(fitnesses)
  fitness.env$median = fitness.env$quantile[3]
  fitness.env$skew = skewness(fitnesses) #requires package "moments"
  fitness.env$kurtosis = kurtosis(fitnesses)
  fitness.env$frequency.of.0 = frequency.of.0
  fitness.env$frequency.of.1 = frequency.of.1
  fitness.env$diversity = frequency.of.0*frequency.of.1/(frequency.of.1+frequency.of.0)
  
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
  mutation.env$numMutations = numMutations
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
    
  }
  else if (length(elite) == 1)
  {
    elitism.env = new.env()
    elitism.env$maxFit = elite[[1]]
  }
  
  return(elitism.env)
}

create.crossover.stats <- function(cross)
{
  #TODO Get crossover points, not currently saved in cross object
  
  crossover.env = new.env()
  crossover.env$numCrossovers = length(returnList(cross))
  
  return(crossover.env)
}

#REPORT ALL THE THINGS!!!
reportAll.reporting.fn <- function(pop, mutation, cross, elite,...)
{
  results = c(pop, mutation, cross, elite)
    
  results
}
         
print.report <- function(GA.env)
{
  cat("Generations:",length(GA.env$reported.data)-1, "Goal Reached:", GA.env$reported.data[[length(GA.env$reported.data)]]@goal.reached)
}

print.fitness.stats <-function(GA.env)
{  
  cat("Fitness Stats:\n")
  for(i in 1:length(GA.env$reported.data))
  {
    if (!is.null(GA.env$reported.data[[i]]@currentGen.results)){
      fitness.env = GA.env$reported.data[[i]]@currentGen.results@fitness      
      cat("Max:",fitness.env$max, "Min:", fitness.env$min, "SD:", fitness.env$SD,"Mean:", fitness.env$mean, "Quantiles:",fitness.env$quantile,
          "Median:",fitness.env$median, "Skew:", fitness.env$skew,"Kurtosis:", fitness.env$kurtosis,"\n")
    }
  }
}

print.elite.stats <-function(GA.env)
{
  cat("Elite Stats:\n")
  for(i in 1:length(GA.env$reported.data))
  {
    if (!is.null(GA.env$reported.data[[i]]@currentGen.results)){
      elite.env = GA.env$reported.data[[i]]@currentGen.results@elite      
      cat("Max:",elite.env$max, "Min:", elite.env$min, "SD:", elite.env$SD,"Mean:", elite.env$mean, "Quantiles:",elite.env$quantile,
          "Median:",elite.env$median, "Skew:", elite.env$skew,"Kurtosis:", elite.env$kurtosis,"\n")
    }
  }
}

print.mutation.stats <-function(GA.env)
{
  cat("Mutation Stats:\n")
  for(i in 1:length(GA.env$reported.data))
  {
    if (!is.null(GA.env$reported.data[[i]]@currentGen.results)){
      mutation.env = GA.env$reported.data[[i]]@currentGen.results@mutation      
      cat("Max:",mutation.env$max, "Min:", mutation.env$min, "SD:", mutation.env$SD,"Mean:", mutation.env$mean, "Quantiles:",mutation.env$quantile,
          "Median:",mutation.env$median, "Skew:", mutation.env$skew,"Kurtosis:", mutation.env$kurtosis,"\n")
    }
  }
}

print.crossover.stats <-function(GA.env)
{
  
}
