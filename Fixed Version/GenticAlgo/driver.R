##### Genetic Algorithm

generational.ga <- function(GA.env){
  with(GA.env, {
    pop <- new.population(GA.env)
    add.population(reproduction.env(GA.env), pop)
    
    repr.results <- NULL
    reported.data <- NULL
    for(gen in 0:max.gen){
      goal.reached <- evaluate(reproduction.env(GA.env)$pop, fitness.env$fitness.fn, fitness.env$decode.fn)
      reported.data <- c(reported.data, report(gen, repr.results, goal.reached))

      #TODO Check if goal has been reached
      #if (goal.reached)
      # break
      repr.results <- next.generation(GA.env)
    }
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
  #TODO - Rewrite this without the 'with' statement
  with(reproduction.env(GA.env), {

    P <- size(pop)

    #Find elites
    if(is.null(parent.env(environment())$selection.env$select.elite))
      elite = NULL
    else
      elite <- parent.env(environment())$select.elite(pop, fitness.env(parent.env(environment()))$fitness.fn)
      
    #Get number of each repro group
    elite.size <- if (!is.null(elite)) length(elite) else 0    
    xover.size <- xover.count(P, elite.size, xover.prob)
    mut.size <- mutate.only.count(P, xover.size, elite.size)

    #Find the chromosomes to be crossed
    p1.loc <- parent.env(environment())$selection.env$select.chr(xover.size, pop)
    p2.loc <- parent.env(environment())$selection.env$select.chr(xover.size, pop)
    rest.loc <- parent.env(environment())$selection.env$select.chr(mut.size, pop)

    elite <- if (!is.null(elite)) duplicate(elite) else NULL   
    p1 <- duplicate(pop[p1.loc])
    p2 <- duplicate(pop[p2.loc])
    rest <- duplicate(pop[rest.loc])

    #Perform reproduction
    m.results <- c(rest = mutate(rest), p1 = mutate(p1), p2 = mutate(p2))
    x.results <- chr.xover(p1, p2, xover.swapMask)

    #Create the next population
    new.pop <- new.population(chromosomes = c(elite, p1, p2, rest))
    
    #Set the current population to the new population
    add.population(environment(), new.pop)
    
    #Report on the new population
    parent.env(environment())$reporting.fn(pop = new.pop, mutation = m.results, cross = x.results)
  })
}

### Fitness functions

straight.decode <- function(chr){
  genes(chr)  
}

one.max.decode <- straight.decode

new.fitness.fn <- function(fitness.fn, ...){
  function(genes) fitness.fn(genes, ...)
}

#Default fitness function
one.max.fn <- function(genes){
  sum(genes)	
}

finite.min.fn <- function(genes, gene.max){
  length(genes) * gene.max - sum(genes)
}