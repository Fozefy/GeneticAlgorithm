#### Elitism

select.elite.population.fgen <- function(elite.fn, ...){
  function(pop, fitness.fn)
    elite.fn(pop = pop, fitness.fn = fitness.fn, ...)
}

#note: elite.selection only works with integer or floating point fitness ordered by <= or >=

elite.selection <- function(pop, fitness.fn, elite.size = 1, decreasing = TRUE, pop.fit = NULL, verbose = FALSE){

  #TODO: Elitism very inefficient and won't work in all cases, but works in the basic case.....should be fixed
  #ignores decreasing
  pop.loc = vector("list", elite.size)
  
  pop.loc[[1]] = pop@chromosomes$values[[1]]
  for(i in 2:elite.size)
  {
    testFitness = pop@chromosomes$values[[i]]@fitness$value
    for (j in 1:i)
    {
      if (j == i)
      {
        #Got to an empty part of the list, just add it
        pop.loc[[j]] <- pop@chromosomes$values[[i]]
      }
      else if (testFitness > pop.loc[[j]]@fitness$value)
      {
        if (j < i)
        {
          #shift the numbers to keep the highest at the top
          for (k in j:(i-1))
          {
            pop.loc[[k+1]] = pop.loc[[k]]
          }          
        }
        pop.loc[[j]] <- pop@chromosomes$values[[i]]
      
      }
    }
  }
  
  for (i in elite.size:length(pop@chromosomes$values))
  {
    testFitness = pop@chromosomes$values[[i]]@fitness$value
    for (j in 1:elite.size)
    {
      if (testFitness > pop.loc[[j]]@fitness$value)
      {
        if (j < elite.size)
        {
          #shift the numbers to keep the highest at the top
          for (k in j:(elite.size-1))
          {
            pop.loc[[k+1]] = pop.loc[[k]]
          }          
        }
        pop.loc[[j]] <- pop@chromosomes$values[[i]]
      }
    }
  }
  
  #P <- ifelse(is.null(pop.fit), size(pop), length(pop.fit))
  #if(is.null(pop.fit))
  #  pop.fit <- fitness.fn(pop)
  #pop.loc <- order(pop.fit, decreasing = decreasing)
  #if (verbose) print(pop.fit)
  
  pop.loc[1:elite.size]
}

# elite.selection(elite.size = 2, pop.fit = runif(10))
