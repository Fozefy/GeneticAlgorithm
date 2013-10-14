##### Genetic Algorithm

generational.ga <- function(GA.env){
  with(GA.env, {
    pop <- new.population(GA.env)
    repr.results <- NULL
    for(gen in 0:max.gen){
      goal.reached <- evaluate(pop, fitness.env)
      report(gen, pop, repr.stats(repr.results), goal.reached)
      #Check if goal has been reached
      #if (goal.reached)
      # break
      repr.results <- next.generation(pop, GA.env)
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

next.generation <- function(popn, ga.env){
  add.population(reproduction.env(ga.env), popn)
  
  P <- size(reproduction.env(ga.env)$pop)
  #Need to rework selection and elitism
  if(is.null(ga.env$select.elite))
    elite.loc = NULL
  else
    elite.loc <- ga.env$select.elite(reproduction.env(ga.env)$pop, fitness.env(ga.env)$fitness.fn)
  with(reproduction.env(ga.env), {
    elite.size <- length(elite.loc)
    xover.size <- xover.count(P, elite.count, xover.prob)
    mut.size <- mutate.only.count(P, xover.size, elite.size)
    
    p1.loc <- select.chr(xover.size, pop)
    p2.loc <- select.chr(xover.size, pop)
    rest.loc <- select.chr(mut.size, pop)
    
    elite <- duplicate(pop[elite.loc])
    p1 <- duplicate(pop[p1.loc])
    p2 <- duplicate(pop[p2.loc])
    rest <- duplicate(pop[rest.loc])
    
    m.results <- c(rest = mutate(rest), p1 = mutate(p1), p2 = mutate(p2))   ### need to create join for m.results
    x.results <- cross(p1, p2)
    
    new.pop <- new.population(c(elite, p1, p2, rest))
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

one.max.fn <- function(genes){
  sum(genes)	
}

finite.min.fn <- function(genes, gene.max){
  length(genes) * gene.max - sum(genes)
}