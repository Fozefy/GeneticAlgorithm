## Selection

select.population.fgen <- function(selection.fn, ...){
  function(selection.size, pop=NULL, stats=NULL)
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
                                 tourn.fit = NULL, tourn.loc = NULL, select.worse = NULL, pop.size = NULL, verbose = FALSE,stats=NULL){
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
                                        tourn.fit = NULL, tourn.loc = NULL, pop.size = NULL, verbose = FALSE,stats=NULL){
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
  cumsum(fit)/ sum(fit)
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

setup.fitnessProportional.withFitScaling <- function(expRank)
{
  fitnessProportional.withFitScaling.selection <- function(n, pop, select=NULL, pop.cumfit = NULL, verbose = FALSE,stats=NULL){
    if(is.null(pop.cumfit))
    {
      fit=NULL
      for (i in 1:length(pop))
      {
        fit[[i]] = pop[[i]]@fitness$value
      }
    }

    sortOrganisms = data.frame(1:length(pop),fit,0)
    sortOrganisms = sortOrganisms[order(sortOrganisms[2], decreasing = FALSE),]

    fi = 0
    fitSum = sum(fit)
    for (i in 1:length(pop))
    {
      fi = fi + (i*sortOrganisms[i,2])/fitSum
    }
    b = (mean(fit) * (expRank - fi))/((length(pop) + 1)/2 - expRank)
    
    gi = NULL

    for (i in 1:length(pop))
    {
      gi[[i]] = pop[[i]]@fitness$value + b
    }    

    gi[gi < 0] = 0
    pop.cumfit = cumulate.fitness(gi)

    selection.loc = fitnessProportional.selection(n, pop, select, pop.cumfit, verbose)
    
    if(!is.null(stats))
    {
      sortOrganisms[3] = 1:length(pop) #Add rank index
      sortOrganisms = sortOrganisms[order(sortOrganisms[1], decreasing = FALSE),]

      selected.ranks = sortOrganisms[selection.loc,3]
      
      stats$avgRank = c(stats$avgRank,mean(selected.ranks))
      stats$varRank = c(stats$varRank,var(selected.ranks))
    }
    
    selection.loc
  }
}

setup.fitnessProportional.withSigmaScaling <- function(nu)
{
  fitnessProportional.withFitScaling.selection <- function(n, pop, select=NULL, pop.cumfit = NULL, verbose = FALSE,stats=NULL){
    if(is.null(pop.cumfit))
    {
      fit=NULL
      for (i in 1:length(pop))
      {
        fit[[i]] = pop[[i]]@fitness$value
      }
    }
    
    sortOrganisms = data.frame(1:length(pop),fit,0)
    sortOrganisms = sortOrganisms[order(sortOrganisms[2], decreasing = FALSE),]
    
    b = nu*sd(fit)-mean(fit)
    
    gi = NULL
    for (i in 1:length(pop))
    {
      gi[[i]] = pop[[i]]@fitness$value + b
    }    
    
    gi[gi < 0] = 0
    pop.cumfit = cumulate.fitness(gi)
    
    selection.loc = fitnessProportional.selection(n, pop, select, pop.cumfit, verbose)
    
    if(!is.null(stats))
    {
      sortOrganisms[3] = 1:length(pop) #Add rank index
      sortOrganisms = sortOrganisms[order(sortOrganisms[1], decreasing = FALSE),]
      
      selected.ranks = sortOrganisms[selection.loc,3]
      
      stats$avgRank = c(stats$avgRank,mean(selected.ranks))
      stats$varRank = c(stats$varRank,var(selected.ranks))
    }
    
    selection.loc
  }
}

fitnessProportional.selection <- function(n, pop, select=NULL, pop.cumfit = NULL, verbose = FALSE, ...){
  if(is.null(pop.cumfit))
  {
    fit=NULL
    for (i in 1:length(pop))
    {
      fit[[i]] = pop[[i]]@fitness$value
    }
    sortOrganisms = data.frame(1:length(pop),fit,0)
    sortOrganisms = sortOrganisms[order(sortOrganisms[2], decreasing = FALSE),]
    
    pop.cumfit <- cumulate.fitness(sortOrganisms[i,2])
  }
  
  if(is.null(select))
    select <- runif(n)
  if(verbose) 
    fp.selection.vprint(pop.cumfit, select)
 
  findInterval(select, pop.cumfit, rightmost.closed=TRUE) + 1
}

# truncated geometric (using resampling)
rtgeom <- function(selection.size,popSize, p = 0.5){
  ceiling(log(1-runif(selection.size)*(1-(1-p)^popSize))/log(1-p))
}

rank.selection.withExp <-function(rate = 1/30)
{
  rank.selection <- function(selection.size, pop = NULL, select=NULL, maximizing = TRUE, pop.size = NULL, stats=NULL){
    n <- selection.size;
    P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, length(pop))
  
    #Gather fitnesses to be sorted
    fit=NULL
    for (i in 1:P)
    {
      fit[[i]] = pop[[i]]@fitness$value
    }
    
    sortOrganisms = data.frame(1:length(pop),fit)
    sortOrganisms = sortOrganisms[order(sortOrganisms[2], decreasing = TRUE),]
    
    selected.ranks = rtgeom(selection.size, P, rate)

    selection.loc = sortOrganisms[selected.ranks,1]

    if(!is.null(stats))
    {
      stats$avgRank = c(stats$avgRank,mean(selected.ranks))
      stats$varRank = c(stats$varRank,var(selected.ranks))
    }
    
    selection.loc
  }
}

rank.selection.linear <- function(expRank, popSize)
{
  m.prime = (6/(popSize + 1))*(expRank - (popSize+1)/2)
  rank.selection <- function(selection.size, pop = NULL, select=NULL, maximizing = TRUE, pop.size = NULL, stats=NULL)
  {
    P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, length(pop))

    selected.rand = runif(selection.size)
    
    selected.ranks = ceiling(((m.prime - 1)*P + 1 + sqrt( ((m.prime +1)*P - 1)^2 - 4*m.prime*P*(P-1)*selected.rand))/(2*m.prime))

    #Gather fitnesses to be sorted
    fit=NULL
    for (i in 1:P)
    {
      fit[[i]] = pop[[i]]@fitness$value
    }
 
    sortOrganisms = data.frame(1:length(pop),fit)
    sortOrganisms = sortOrganisms[order(sortOrganisms[2], decreasing = FALSE),]
    
    selection.loc = NULL
    for(i in 1:selection.size)
    {
      selection.loc[i] = sortOrganisms[selected.ranks[i],1]
    }
    
    if(!is.null(stats))
    {
      stats$avgRank = c(stats$avgRank,mean(selected.ranks))
      stats$varRank = c(stats$varRank,var(selected.ranks))
    }

    selection.loc
    
  }
}


sigmaScaling.formula <- function(nu, n, sortedOrganisms)
{
  fbar = sum(sortedOrganisms[2])/n
  fbar.sqr = sum(sortedOrganisms[2]^2)/n
  frs = 2/(n*(n+1)) * sum(sortedOrganisms[3]*sortedOrganisms[2])
  frs.sqr = 2/(n*(n+1)) * sum(sortedOrganisms[3]*sortedOrganisms[2]^2)

  print(fbar)
  print(fbar.sqr)
  print(frs)
  print(frs.sqr)
  
  a = -1*(nu^2+1)* ((n+1)*(frs - fbar)/(n-1))^2
  b = (n+1)/(n-1) * (nu^2 * (frs.sqr - fbar.sqr) - 2*(nu^2 + 1)*(frs - fbar)*fbar)
  c = nu^2 * fbar  - nu^2 * fbar.sqr - fbar.sqr
  
  answerWithPlus = (-b + sqrt(b^2 - 4*a*c))/(2*a)
  answerWithNegative = (-b - sqrt(b^2 - 4*a*c))/(2*a)
  
  print(answerWithPlus)
  print(answerWithNegative)
  
  if(answerWithPlus >= 1 && answerWithPlus > 0)
  {
    return(answerWithPlus)
  }
  else if(answerWithNegative >= 1 && answerWithNegative > 0)
  {
    return(answerWithNegative)
  }
  else if (answerWithNegative > 1 || answerWithPlus > 1)
  {
    print("Sigma Over 1")
    return(1)
  }
  else
  {
    print("Both Sigma under/equal 0!")
    return(0)
  }
}


rank.selection.linear.sigmascaling <- function(nu)
{
  rank.selection <- function(selection.size, pop = NULL, select=NULL, maximizing = TRUE, pop.size = NULL, stats=NULL)
  {
    P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, length(pop))
    
    #Gather fitnesses to be sorted
    fit=NULL
    for (i in 1:P)
    {
      fit[[i]] = pop[[i]]@fitness$value
    }
    
    sortOrganisms = data.frame(1:length(pop),fit)
    sortOrganisms = sortOrganisms[order(sortOrganisms[2], decreasing = FALSE),]
    sortOrganisms[3] = 1:length(pop) #Add rank index
    
    m.prime = sigmaScaling.formula(nu, P, sortOrganisms)
        
    selected.rand = runif(selection.size)
    
    selected.ranks = ceiling(((m.prime - 1)*P + 1 + sqrt( ((m.prime +1)*P - 1)^2 - 4*m.prime*P*(P-1)*selected.rand))/(2*m.prime))
    
    selection.loc = NULL
    for(i in 1:selection.size)
    {
      selection.loc[i] = sortOrganisms[selected.ranks[i],1]
    }
    
    if(!is.null(stats))
    {
      stats$avgRank = c(stats$avgRank,mean(selected.ranks))
      stats$varRank = c(stats$varRank,var(selected.ranks))
    }
    
    selection.loc
    
  }
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
      selection.loc[[i]] = adjMatrix[i,selection.loc[[i]]]
    }
  }

  selection.loc
}

#Select the child chosen for the spatial slot at random, elite takes slot only if better
spatial.child.selection.random <- function(pop, p1, p2, elite, maximizing, fitness.env)
{
  #Fitness of chosen organisms
  fitness.set = c(1)

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
    
    fitness.set[[i]] = p1[[i]]@fitness$value
  }
  
  #Now we can add elites, we knock out other nodes if elite has better fitness
  if(!is.null(elite))
  {
    if(maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
    for(j in 1:length(elite))
    {    
      #Do a hard tournament, replace the node in elite's spot if elite is still better
      if (elite[[j]]@fitness$value %>% p1[[elite[[j]]@index$value]]@fitness$value)
      {
        p1[[elite[[j]]@index$value]] = elite[[j]]
        fitness.set[[elite[[j]]@index$value]] = elite[[j]]@fitness$value
      }
    }
  }
  
  fitness.env$fitness.set[[pop@popNum]] = fitness.set
  
  return(p1)
}

#Select child at random and keep elite regardless of fitness at slot
spatial.child.selection.random.hardElite <- function(pop, p1, p2, elite, maximizing, fitness.env)
{
  #Fitness of chosen organisms
  fitness.set = c(1)
  
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
    
    fitness.set[[i]] = p1[[i]]@fitness$value
  }
  
  #Now we can add elites, we knock out other nodes if elite has better fitness
  if(!is.null(elite))
  {
    if(maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
    for(j in 1:length(elite))
    {    
      #Always maintain elites
      p1[[elite[[j]]@index$value]] = elite[[j]]
      fitness.set[[elite[[j]]@index$value]] = elite[[j]]@fitness$value
    }
  }
  
  fitness.env$fitness.set[[pop@popNum]] = fitness.set
  
  return(p1)
}

#Choose node based on child with better fitness
spatial.child.selection.fitness <- function(pop, p1, p2, elite, maximizing, fitness.env)
{
  if(maximizing) {`%>%` <- `>`} else {`%>%` <- `<`}
  
  #Fitness of chosen organisms
  fitness.set = c(1)
  
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
    
    fitness.set[[i]] = p1[[i]]@fitness$value
  }
  
  #Now we can add elites, we knock out other nodes if elite has better fitness
  if(!is.null(elite))
  {    
    for(j in 1:length(elite))
    {    
      #Do a hard tournament, replace the node in elite's spot if elite is still better
      if (elite[[j]]@fitness$value %>% p1[[elite[[j]]@index$value]]@fitness$value)
      {
        p1[[elite[[j]]@index$value]] = elite[[j]]
        fitness.set[[elite[[j]]@index$value]] = elite[[j]]@fitness$value
      }
    }
  }
  
  fitness.env$fitness.set[[pop@popNum]] = fitness.set
  
  return(p1)
}
