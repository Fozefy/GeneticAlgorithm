## Selection

select.population.fgen <- function(selection.fn, ...){
  function(selection.size, pop=NULL)
    selection.fn(selection.size = selection.size, pop = pop, ...)
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
  P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, size(pop))
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
  P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, pop@pop.size) 
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

#Selecting partners based on an adj matrix
local.selection <- function(pop, selectionPartners, selection.fn, adjMatrix)
{
  selection.loc = NULL
  for (i in 1:length(selectionPartners))
  {
    #Create a subpopultion containing the possible mates
    subPop = new.population(organisms=pop[adjMatrix[,selectionPartners[[i]]]])
    
    selection.loc[[i]] = selection.fn(1, subPop)
    
    #The index returned was the one in the subPop, we need the index in the origional pop
    selection.loc[[i]] = subPop[[selection.loc[[i]]]]@index
  }
  selection.loc
}

#Select nodes which haven't been selected yet
select.unselected.nodes <- function(selection.size, pop, alreadySelected, selection.fn)
{
  subPop = new.population(organisms = pop[-alreadySelected])
  
  selection.loc = selection.fn(selection.size, subPop)
  
  for (i in 1:length(selection.loc))
  {
    selection.loc[[i]] = subPop[[selection.loc[[i]]]]@index
  }
  
  selection.loc
}

spatial.selection <- function(pop, p1, p1.loc, p2, p2.loc, rest, rest.loc, elite, fitness.env, otherPops, i, maximizing)
{
  #We needed the new pop to be based on a spatial relationship so they can't move around
  new.pop = duplicate(pop)

  #Put all of our reproduced organisms in the appropriate locations within the new population
  #We will choose which child to use at random (we  can't just choose the first as you then take first gene section from p1 always)
  for (j in 1:length(p1.loc))
  {
    new.pop[p1.loc[[j]]] = if (runif(1) > 0.5) p1[[j]] else p2[[j]]
  }
  
  new.pop[rest.loc] = rest

  #Find fitnesses for new.pop
  if(!is.null(otherPops))
  {
    fitness.env$fitness.set[[i]] <- evaluate(new.pop, fitness.env$fitness.fn, otherPops, fitness.env$externalConnectionsMatrix)
  }
  else
  {
    fitness.env$fitness.set <- evaluate(new.pop, fitness.env(GA.env)$fitness.fn)
  }
  
  #Now we can add elites, we knock out other nodes if elite has better fitness
  if(!is.null(elite))
  {
    if(selection.env(GA.env)$maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
    
    for(j in 1:length(elite))
    {
      if(!is.null(otherPops))
      {
        evaluate(elite[[j]], fitness.env$fitness.fn, new.pop, otherPops, fitness.env$externalConnectionsMatrix)
      }
      else
      {
        #We have a spatial GA, but without a second pop (no coevolution)
        evaluate(elite[[j]], fitness.env$fitness.fn, new.pop)            
      }
      
      #Do a hard tournament, replace the node in elite's spot if elite is still better
      if (elite[[j]]@fitness$value %>% new.pop[[elite[[j]]@index]]@fitness$value)
      {
        new.pop[[elite[[j]]@index]] = elite[[j]]
        
        if(!is.null(otherPops))
        {
          fitness.env$fitness.set[[i]][[elite[[j]]@index]] = elite[[j]]@fitness$value
        }
        else
        {
          fitness.env$fitness.set[[elite[[j]]@index]] = elite[[j]]@fitness$value
        }
      }
    }
  }
  
  return(new.pop)
}