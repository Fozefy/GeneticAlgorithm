#### Elitism

select.elite.population.fgen <- function(elite.fn, ...){
  function(pop, fitness.fn)
    elite.fn(pop = pop, fitness.fn = fitness.fn, ...)
}

#note: elite.selection only works with integer or floating point fitness ordered by <= or >=

elite.selection <- function(pop, fitness.fn, elite.size = 1, decreasing = TRUE, 
                            pop.fit = NULL, verbose = FALSE){
  P <- ifelse(is.null(pop.fit), size(pop), length(pop.fit))
  if(is.null(pop.fit))
    pop.fit <- fitness.fn(pop)
  pop.loc <- order(pop.fit, decreasing = decreasing)
  if (verbose) print(pop.fit)
  
  pop.loc[1:elite.size]
}

# elite.selection(elite.size = 2, pop.fit = runif(10))

truncation.selection <- elite.selection
