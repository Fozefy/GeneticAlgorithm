##### Genetic Algorithm

generational.ga <- function(GA.env){
  with(GA.env, {
    pop <- new.population(GA.env)
    repr.results <- NULL
    for(gen in 0:max.gen){
      goal.reached <- evaluate(pop, fitness.env$fitness.fn, fitness.env$decode.fn)
      report(gen, pop, repr.stats(repr.results), goal.reached)
      #TODO Check if goal has been reached
      #if (goal.reached)
      # break
      repr.results <- next.generation(pop, GA.env)
      
      ####################What is this function supposed to be doing???
      pop <- population(repr.results)
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

next.generation <- function(popn, GA.env){
  add.population(reproduction.env(GA.env), popn)
  
  with(reproduction.env(GA.env), {

    P <- size(pop)

    elite.loc = NULL
    if(!is.null(parent.env(environment())$selection.env$select.elite))
      elite.loc = NULL  
    else
      elite.loc <- parent.env(environment())$select.elite(pop, fitness.env(parent.env(environment()))$fitness.fn)
    
    if(is.null(elite.loc))
    {
      elite.size <- 0
    }
    else
    {
      elite.size <- length(elite.loc)
    }
    
    xover.size <- xover.count(P, elite.size, xover.prob)
    mut.size <- mutate.only.count(P, xover.size, elite.size)

    p1.loc <- parent.env(environment())$selection.env$select.chr(xover.size, pop)
    p2.loc <- parent.env(environment())$selection.env$select.chr(xover.size, pop)
    rest.loc <- parent.env(environment())$selection.env$select.chr(mut.size, pop)
    
    elite <- if (!is.null(elite.loc)) duplicate(pop@chromosomes$values[elite.loc]) else NULL   
    p1 <- duplicate(pop[p1.loc])
    p2 <- duplicate(pop[p2.loc])
    rest <- duplicate(pop[rest.loc])
    
    m.results <- c(rest = mutate(rest), p1 = mutate(p1), p2 = mutate(p2))   ### need to create join for m.results
    x.results <- chr.xover(p1, p2)
    
    ###PROBLEM HERE --- new.population expects to be sent GA.env, what are we doing with chromosome list?
    new.pop <- new.population(c(elite, p1, p2, rest))
    
    #this function doesn't exist, what is it supposed to do?
    create.results(pop = new.pop, mutation = m.results, cross = x.results)
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