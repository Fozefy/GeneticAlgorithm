## Selection

select.population.fgen <- function(selection.fn, ...){
  function(selection.size, pop=NULL)
    selection.fn(selection.size = selection.size, pop = pop, ...)
}

#Uniform Selection
uniform.selection <- function(selection.size, pop.size)
{
  sample(1:pop.size, selection.size, replace = FALSE)
}

# tournament selection

new.tournaments.locations <- function(selection.size, tourn.size, pop.size){
  n <- selection.size; k <- tourn.size; P <- pop.size 
  tourn.loc <- matrix(nrow = k, ncol = n)
  for(i in 1:k){
    tourn.loc[i,] <- sample(1:P, n, replace=FALSE)
  }
  tourn.loc
}

new.tournaments.fitness <- function(selection.size, tourn.size, pop = NULL, tourn.loc = NULL, rprob = runif){
  n <- selection.size
  k <- tourn.size
  tourn.fit <- matrix(nrow = k, ncol = n)
  
  if(is.null(pop)){
    for(i in 1:k)
      tourn.fit[i,] <- rprob(n)
  } else {
    pop.fit <- getFitnesses(pop)
    for(i in 1:k){
      loc <- tourn.loc[i,]
      tourn.fit[i,] <- pop.fit[loc]
    }
  }
  
  tourn.fit
}

new.select.worse.matrix <- function(selection.size, tournament.size, prob.select.worse){
  n <- selection.size; k <- tournament.size; alpha <- prob.select.worse 
  select.worse <- matrix(data=FALSE, nrow = k, ncol = n)
  for(i in 2:k){
    select.worse[i,] <- (runif(n) < prob.select.worse)
  }
  select.worse
}

tournament.selection.vprint <- function(tourn.fit, select.worse, tourn.loc, selection.fit){
  print(tourn.fit)
  if(!is.null(select.worse))
    print(select.worse)
  print(tourn.loc)
  print(selection.fit)  
}

tournament.selection <- function(selection.size, pop = NULL, tourn.size = 2, prob.select.worse = 0, maximizing = TRUE,
                                 tourn.fit = NULL, tourn.loc = NULL, select.worse = NULL, pop.size = NULL, verbose = FALSE){
  n <- selection.size; k <- tourn.size 
  P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, length(pop))
  if(maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
  if(is.null(select.worse))
    select.worse <- new.select.worse.matrix(n, k, prob.select.worse)
  if(is.null(tourn.loc))
    tourn.loc <- new.tournaments.locations(n, k, P)
  if(is.null(tourn.fit))
    tourn.fit <- new.tournaments.fitness(n, k, pop, tourn.loc)
  
  selection.loc <- tourn.loc[1,]
  selection.fit <- tourn.fit[1,]
  
  for(i in 2:k){
    better <- xor((tourn.fit[i,] %>% selection.fit), select.worse[i,])
    selection.fit[better] <- tourn.fit[i,better]
    selection.loc[better] <- tourn.loc[i,better]
  }
  
  if(verbose)
    tournament.selection.vprint(tourn.fit, select.worse, tourn.loc, selection.fit)
  selection.loc
}

simple.tournament.selection <- function(selection.size, pop = NULL, tourn.size = 2, maximizing = TRUE,
                                        tourn.fit = NULL, tourn.loc = NULL, pop.size = NULL, verbose = FALSE){
  n <- selection.size; k <- tourn.size
  P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, length(pop)) 
  if(maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
  if(is.null(tourn.loc))
    tourn.loc <- new.tournaments.locations(n, k, P)
  if(is.null(tourn.fit))
    tourn.fit <- new.tournaments.fitness(n, k, pop, tourn.loc)
  
  selection.loc <- tourn.loc[1,]
  selection.fit <- tourn.fit[1,]
  
  for(i in 2:k){
    better <- (tourn.fit[i,] %>% selection.fit)
    selection.fit[better] <- tourn.fit[i,better]
    selection.loc[better] <- tourn.loc[i,better]
  }
  
  if(verbose)
    tournament.selection.vprint(tourn.fit, NULL, tourn.loc, selection.fit)
  selection.loc
}

# tournament.selection(8, tourn.size = 5, pop.size = 100,
#								tourn.fit = new.tournaments.fitness(8,5),
#								select.worse = create.select.worse.matrix(8,5,0.1)
#								verbose = TRUE)

cumulate.fitness <- function(fit){
  cumsum(fit / sum(fit))
}

fitnessProportional.setup <- function(pop){
  pop[["cumulative.fitness"]] <- cumulate.fitness(pop[["fitness"]])
}

cumulative.fitness <- function(pop){
  pop[["cumulative.fitness"]]
}

fp.selection.vprint <- function(pop.cumfit, select){
  print(pop.cumfit)
  print(select)	
}

fitnessProportional.selection <- function(n, pop, select=NULL, pop.cumfit = NULL, verbose = FALSE){
  if(is.null(pop.cumfit))
    pop.cumfit <- cumulative.fitness(pop)
  if(is.null(select))
    select <- runif(n)
  if(verbose) 
    fp.selection.vprint(pop.cumfit, select)
  findInterval(select, pop.cumfit, rightmost.closed=TRUE) + 1
}

# fitnessProportional.selection(6, NULL, pop.cumfit = cumulate.fitness(runif(10), verbose = TRUE))

rank.selection <- function(n, pop = NULL, select=NULL){
  
}

#Selecting a primary parent for each node based on an adj matrix
#We are selecting primary parent among the nodes and its direct neighbours of the node using the default selection.fn
spatial.selection <- function(pop, selection.fn, adjMatrix)
{
  selection.loc = NULL
  for (i in 1:length(pop))
  {
    if(is.null(adjMatrix))
    {
      selection.loc[[i]] = selection.fn(1, pop)
    }
    else
    {
      selection.loc[[i]] = selection.fn(1, pop[adjMatrix[i,]])
    }
  }
  selection.loc
}

spatial.child.selection.random <- function(pop, p1, p2, elite, maximizing)
{
  #P1 will hold the set of organisms to be used
  
  #We will choose which child to use based on which has the better fitness
  for (i in 1:length(p1))
  {
    #p2 will equal 0 if this was a mutation only node, no choice to make
    if (class(p2[[i]]) == "numeric")
    {
      if (sample(1:2) > 1)
      {
        #p2 has been randomly selected
        p1[[i]] = p2[[i]]
      }
    }
  }
  
  #Now we can add elites, we knock out other nodes if elite has better fitness
  if(!is.null(elite))
  {
    if(maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
    for(j in 1:length(elite))
    {    
      #Do a hard tournament, replace the node in elite's spot if elite is still better
      if (elite[[j]]@fitness$value %>% p1[[elite[[j]]@index]]@fitness$value)
      {
        p1[[elite[[j]]@index]] = elite[[j]]
      }
    }
  }
  
  return(p1)
}

#Choose node based on child with better fitness
spatial.child.selection.fitness <- function(pop, p1, p2, elite, maximizing)
{
  if(maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
  
  #P1 will hold the set of organisms to be used

  #We will choose which child to use based on which has the better fitness
  for (i in 1:length(p1))
  {
    #p2 will equal 0 if this was a mutation only node, no choice to make
    if (p2[[i]] != 0)
    {
      if (p2[[i]]@fitness$value %>% p1[[i]]@fitness$value)
      {
        #p2 has a better fitness, so we use that child
        p1[[i]] = p2[[i]]
      }
    }
  }
  
  #Now we can add elites, we knock out other nodes if elite has better fitness
  if(!is.null(elite))
  {    
    for(j in 1:length(elite))
    {    
      #Do a hard tournament, replace the node in elite's spot if elite is still better
      if (elite[[j]]@fitness$value %>% p1[[elite[[j]]@index]]@fitness$value)
      {
        p1[[elite[[j]]@index]] = elite[[j]]
      }
    }
  }
  
  return(p1)
}