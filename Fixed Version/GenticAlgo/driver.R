##### Genetic Algorithm

generational.ga <- function(GA.env){
  with(GA.env, {
    pop <- new.population(GA.env)
    add.population(reproduction.env(GA.env), pop)
    
    currentGen.results <- NULL #Our 0 generation has no results, but we want to be able to report anyway
    reported.data <- NULL
    for(gen in 0:max.gen){
      fitness.set <- evaluate(reproduction.env(GA.env)$pop, fitness.env$fitness.fn, fitness.env$decode.fn)
      
      #Check if we've met our goal yet
      if (!is.null(fitness.env(GA.env)$goal.fn))
        goal.reached = fitness.env(GA.env)$goal.fn(fitness.set)
      else
        goal.reached = FALSE
          
      #Save the data we've reported so far
      reported.data <- c(reported.data, report(gen, currentGen.results, goal.reached))
  
      if (verbose)
        print(max(fitness.set))
      
      #If we've met our goal: stop!
      if (goal.reached) break
      
      currentGen.results <- next.generation(GA.env)
    }
    if (verbose)
      print.report(environment())
    
  })
}

xover.count <- function(P, elite.count, xover.prob, xover=NULL){
  xover.count <- (P - elite.count) %/% 2
  if(is.null(xover))
    xover <- (runif(xover.count) < xover.prob)
  sum(xover)
}

mutate.only.count <- function(P, x, e){
  P - 2 * x - e
}

next.generation <- function(GA.env){  
  P <- size(reproduction.env(GA.env)$pop)

  #Find elites
  if(is.null(selection.env(GA.env)$select.elite))
    elite = NULL
  else
    elite <- selection.env(GA.env)$select.elite(reproduction.env(GA.env)$pop, fitness.env(GA.env)$fitness.fn)
    
  #Get number of each repro group
  elite.size <- if (!is.null(elite)) length(elite) else 0
  xover.size <- xover.count(P, elite.size, reproduction.env(GA.env)$xover.prob)
  mut.size <- mutate.only.count(P, xover.size, elite.size)

  #Find the chromosomes to be crossed
  p1.loc <- selection.env(GA.env)$select.chr(xover.size, reproduction.env(GA.env)$pop)
  p2.loc <- selection.env(GA.env)$selection.env$select.chr(xover.size, reproduction.env(GA.env)$pop)
  rest.loc <- selection.env(GA.env)$selection.env$select.chr(mut.size, reproduction.env(GA.env)$pop)

  elite <- if (!is.null(elite)) duplicate(elite) else NULL   
  p1 <- duplicate(reproduction.env(GA.env)$pop[p1.loc])
  p2 <- duplicate(reproduction.env(GA.env)$pop[p2.loc])
  rest <- duplicate(reproduction.env(GA.env)$pop[rest.loc])

  #Perform reproduction
  #Mutate
  restResults = reproduction.env(GA.env)$mutate(rest)
  p1Results = reproduction.env(GA.env)$mutate(p1)
  p2Results = reproduction.env(GA.env)$mutate(p2)
  #Store Mutation results
  m.results <- c(restResults, p1Results, p2Results)
  #CrossOver
  x.results <- chr.xover(p1, p2, reproduction.env(GA.env)$xover.swapMask)

  #Create the next population
  new.pop <- new.population(chromosomes = c(elite, p1, p2, rest))
    
  #Set the current population to the new population
  add.population(reproduction.env(GA.env), new.pop)
    
  #Report on the new population
  GA.env$reporting.fn(pop = new.pop, mutation = m.results, cross = x.results, elite = elite)
}

### Fitness functions
new.fitness.fn <- function(fitness.fn, ...){
  function(genes) fitness.fn(genes, ...)
}
