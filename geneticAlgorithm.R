############
### Input GA parameters

# GA.parameters: fitness.fn, goal.fn, epsilon, max.gen
# GA.parameters: 

# chr.encode
#     chr.encode.type: character = {"binary", "spin", "symbolic", "seq", "integer", "dist", "real"}
# 		chr.mode: character = {"numeric", "character", "logical", "complex", "raw", "list", "expression", "name"}
# 		chr.length
# pop.size
# select.elite 
#	 elite.size
# select.chr
#	 tournament
#		 tournament.size	
# mutate
#		mutate.type: character = {"binary", "boolean", "complement", "spin", 
#											 "symbolic", "integer", "creap", "uniform", "gaussian"} 
# 		prob.mutation
# xover
#		xover.type: character = {"1pt", "2pt", "kpt", "uniform") 
# 		xover.prob

# Control Parameters: max.gen, report.stats, environments  
# Encode Environment:  chr.length, pop.size, new.population
#   |-- Selection Environment: select.elite, select.chr
#      |-- Reproduction Environment: mutate, xover, xover.prob
#   |-- Fitness Environment: fitness.fn, goal.fn, epsilon

new.GA.env <- function(pop.size = 100, verbose = TRUE,
                       GA.base.args = new.GA.base.args(), encoding.args = new.encoding.args(), mutation.args = new.mutation.args(),
                       fitness.args = new.fitness.args(goal = encoding.args$chr.length), xover.args = new.xover.args(), selection.args = new.selection.args(),
                       reporting.fn = base.reporting.fn){
  GA.env <- new.env()
  setup.GA.envTree(GA.env)
  setup.GA.base.env(GA.env, GA.base.args, pop.size, verbose)
  setup.encoding.env(GA.env, encoding.args)
  setup.reproduction.env(GA.env, mutation.args, xover.args)
  setup.selection.env(GA.env, selection.args)
  setup.fitness.env(GA.env, fitness.args)
  setup.reporting(GA.env, reporting.fn)
  GA.env
}

#Convenience functions for accessing inner environments
encoding.env <- function(GA){GA$encoding.env}
fitness.env <- function(GA){GA$fitness.env}
selection.env <- function(GA){GA$selection.env}
reproduction.env <- function(GA){GA$reproduction.env}

#Create all of the GA's inner environments
setup.GA.envTree <- function(GA.env) {
  GA.env$encoding.env <- new.env(parent = GA.env)
  GA.env$fitness.env <- new.env(parent = GA.env)
  GA.env$selection.env <- new.env(parent = GA.env)
  GA.env$reproduction.env <- new.env(parent = GA.env)
}

#Helper function for setting up environments - adds a list of args to env
add.to.env <- function(env, arg.list){
  arg.names <- names(arg.list)
  for(name in arg.names){
    env[[name]] <- arg.list[[name]]   
  }
}

#######################################Create the GA's arguements
new.GA.base.args <- function(max.gen = 100, numPop = 1){
  max.gen; numPop
  as.list(environment())
}

new.encoding.args <- function(chr.length = 30, chr.encode.type = "binary", 
                              gene.alphabet = NULL, rdist = NULL, rdist.args = NULL, 
                              gene.mode = NULL, seq.args = NULL){
  chr.length; chr.encode.type; gene.alphabet; rdist; rdist.args; gene.mode; seq.args
  as.list(environment())
}

new.fitness.args <- function(fitness.fn = one.max.fn, fitnessFn.args = NULL, goal.fn = NULL, goal = 30, epsilon = 0)
{
  fitness.fn; fitnessFn.args; goal.fn; goal; epsilon
  as.list(environment())
}

new.xover.args <- function (xover.prob = 0.8, xover.type = "uniform", xover.alpha = 0.3, xover.k = 2)
{
  xover.prob; xover.type; xover.alpha
  as.list(environment())
}

new.mutation.args <- function(gene.delta = NULL, gene.sd = NULL, prob.mutation = 2, mutation.type = "binary", not.equal.op = `!=`){
  gene.delta; gene.sd
  as.list(environment())
}

new.selection.args <- function(selection.type = "simple.tournament", tourn.size = 2, prob.select.worse = 0, maximizing = TRUE, elitism = TRUE, elite.size = 2, elite.fn = truncation.selection){
  selection.type; tourn.size; prob.select.worse; maximizing; elitism; elite.size; elite.fn
  as.list(environment())
}

##################Setup the GA environments as per the appropriate args
setup.GA.base.env <- function(GA.env, GA.base.args = list(), pop.size = 100, verbose){
  add.to.env(GA.env, GA.base.args)
  GA.env$pop.size = pop.size
  GA.env$verbose = verbose
}

setup.encoding.env <- function(GA.env, encoding.args = new.encoding.args()){
  add.to.env(encoding.env(GA.env), encoding.args)
  with(encoding.env(GA.env), {
    if(encoding.args$chr.encode.type == "seq" || encoding.args$chr.encode.type == "integer"){
      gene.values <- do.call(seq, seq.args)
      gene.max <- max(gene.values)
      gene.min <- min(gene.values)
    }
    
    new.genes.fn <- switch(encoding.args$chr.encode.type,
                           binary   = new.genes.binary,
                           spin 		= new.genes.spin,
                           symbolic	= new.genes.fgen(new.genes.symbolic, gene.alphabet),
                           seq 		= new.genes.fgen(new.genes.symbolic, gene.values),
                           integer 	= new.genes.fgen(new.genes.symbolic, gene.values),
                           dist 		= new.genes.fgen(new.genes.dist, rdist, rdist.args),
                           real 		= new.genes.fgen(new.genes.dist, rdist, rdist.args),
                           ... 		= simpleError(paste("chr.encode.type must be one of 'binary', 'spin' ", 
                                                     "'symbolic', 'seq', 'integer', 'dist' or 'real'.",
                                                     "Instead it is '", chr.encode.type, "'", sep = "")))
    
  })	
}

setup.fitness.env <- function(GA.env, fitness.args){
  add.to.env(fitness.env(GA.env), fitness.args)
  
  if (is.null(fitness.env(GA.env)$goal.fn))
  {
    GA.env$fitness.env$goal.fn <- simpleGoal(fitness.env(GA.env)$goal, fitness.env(GA.env)$epsilon, selection.env(GA.env)$maximizing)
  }
}

setup.reproduction.env <- function(GA.env, mutation.args = new.mutation.args(), xover.args = new.xover.args()){
  add.to.env(reproduction.env(GA.env), mutation.args)
  setup.mutation(reproduction.env(GA.env))
  
  add.to.env(reproduction.env(GA.env), xover.args)
  setup.xover(reproduction.env(GA.env))
}

setup.mutation <- function(reproduction.env){
  with(reproduction.env, {
    if(prob.mutation > 1) 
      prob.mutation <- prob.mutation / encoding.env(parent.env(environment()))$chr.length
    
    alleles.fn <- switch(mutation.type, 
                         binary 		= alleles.binary,
                         boolean 		= alleles.boolean,
                         complement 	= alleles.fgen(alleles.complement, encoding.env(GA.env)$gene.alphabet),
                         spin 			= alleles.spin,
                         symbolic 	= alleles.fgen(alleles.symbolic, encoding.env(GA.env)$gene.alphabet),
                         integer 		= alleles.fgen(alleles.integer, encoding.env(GA.env)$gene.values),
                         creap 		= alleles.fgen(alleles.creap, encoding.env(GA.env)$gene.max, encoding.env(GA.env)$gene.min),
                         uniform 		= alleles.fgen(alleles.uniform, encoding.env(GA.env)$gene.delta),
                         gaussian 	= alleles.fgen(alleles.gaussian, encoding.env(GA.env)$gene.sd),
                         ...			= simple.error(paste("mutation.type must be one of 'binary', 'boolean', 'complement', ", 
                                                    "'spin', 'symbolic', 'integer', 'creap', 'uniform' or 'gaussian'.",
                                                    "Instead it is '", mutation.type, "'", sep = "")))
    
    mutate <- mutate.fgen(environment())
  })
}

setup.xover <- function(reproduction.env){
  with(reproduction.env, {
    xsm.fgen <- xover.swapMask.fgen
    xover.swapMask <- switch(xover.type, 
                             one.pt  = xover.mask.1point,
                             two.pt	= xsm.fgen(xover.mask.kpoint, 2),
                             k.pt 		= xsm.fgen(xover.mask.kpoint, xover.k),
                             uniform 	= xsm.fgen(xover.mask.uniform, xover.alpha),
                             ...		= simple.error(paste("xover.type must be one of '1pt', '2pt', 'kpt' or 'uniform'. ", 
                                                       "Instead it is '", xover.type, "'", sep = "")))
    
    xover <- xover.fgen(xover.swapMask)
  })
}

setup.selection.env <- function(GA.env, selection.args = new.selection.args()){
  
  add.to.env(selection.env(GA.env), selection.args)
  with(selection.env(GA.env), {

    #Setup.elitism      
    if(elitism)
      select.elite <- select.elite.population.fgen(elite.fn = elite.fn, elite.size = elite.size, maximizing = maximizing)
    else
      select.elite <- NULL
    
    select.fgen <- select.population.fgen
    select.chr <- switch(selection.type, 
                         simple.tournament  = select.fgen(simple.tournament.selection, tourn.size, maximizing),
                         tournament  			= select.fgen(tournament.selection, tourn.size, prob.select.worse, maximizing),
                         fps 						= fitnessProportional.selection,
                         rank 						= rank.selection,  # not yet implemented ... just a stub
                         ...						= simple.error(paste("select.chr.type must be one of ", 
                                                       "'simple.tournament', 'tournament', 'fps' or 'rank'. ", 
                                                       "Instead it is '", select.chr.type, "'", sep = "")))
    
    selection.env
  })
}

setup.reporting <- function(GA.env, reporting.fn){
  GA.env$reporting.fn = reporting.fn
}

##### Genetic Algorithm
generational.ga <- function(GA.env){
  with(GA.env, {
    pop = vector("list", GA.env$numPop)
    for (i in 1:GA.env$numPop)
    {
      pop[i] <- new.population(GA.env)
    }
    add.population(reproduction.env(GA.env), pop)
    
    currentGen.results <- NULL #Our 0 generation has no results, but we want to be able to report anyway
    reported.data <- NULL
    for(gen in 0:max.gen){
      fitness.set = vector("list", GA.env$numPop)
      if (GA.env$numPop > 1)
      {
        for (i in 1:GA.env$numPop)
        {
          if (i == 1)
          {
            otherPops = pop[2:GA.env$numPop]
          }
          else if (i == GA.env$numPop)
          {
            otherPops = pop[1: (i - 1)]
          }
          else
          {
            otherPops = c(pop[1:(i-1)],pop[(i+1):GA.env$numPop])
          }
          
          fitness.set[[i]] <- evaluate(reproduction.env(GA.env)$pop[[i]], fitness.env$fitness.fn, otherPops)
        }
      }
      else
      {
        fitness.set[[1]] <- evaluate(reproduction.env(GA.env)$pop[[i]], fitness.env$fitness.fn)
      }
      
      #Check if we've met our goal yet
      if (!is.null(fitness.env(GA.env)$goal.fn))
        goal.reached = fitness.env(GA.env)$goal.fn(fitness.set)
      else
        goal.reached = FALSE
          
      #Save the data we've reported so far
      reported.data <- c(reported.data, report(gen, currentGen.results, goal.reached))
  
      if (verbose) #TODO - Figure out what to print for multiple pops, just print first for now
        print(if (selection.env(GA.env)$maximizing)max(fitness.set[[1]]) else min(fitness.set[[1]]))
      
      #If we've met our goal: stop!
      if (goal.reached) break
      
      if (gen != max.gen)
        currentGen.results <- next.generation(GA.env)
    }
    
    if (verbose)
      print.report(environment())
    
  })
}

next.generation <- function(GA.env){
  new.pop = vector("list", GA.env$numPop)
  mutation.results = vector("list", GA.env$numPop)
  xover.results = vector("list", GA.env$numPop)
  elite = vector("list", GA.env$numPop)
  
  for (i in 1:GA.env$numPop)
  {
    P <- size(reproduction.env(GA.env)$pop[[i]])
  
    #Find elites
    if(is.null(selection.env(GA.env)$select.elite))
      elite[[i]] = NULL
    else
      elite[[i]] <- selection.env(GA.env)$select.elite(reproduction.env(GA.env)$pop[[i]])
      
    #Get number of each repro group
    elite.size <- if (!is.null(elite[[i]])) length(elite[[i]]) else 0
    xover.size <- xover.count(P, elite.size, reproduction.env(GA.env)$xover.prob)
    mut.size <- mutate.only.count(P, xover.size, elite.size)
  
    #Find the chromosomes to be crossed
    p1.loc <- selection.env(GA.env)$select.chr(xover.size, reproduction.env(GA.env)$pop[[i]])
    p2.loc <- selection.env(GA.env)$select.chr(xover.size, reproduction.env(GA.env)$pop[[i]])
    rest.loc <- selection.env(GA.env)$select.chr(mut.size, reproduction.env(GA.env)$pop[[i]])
  
    elite[[i]] <- if (!is.null(elite[[i]])) duplicate(elite[[i]]) else NULL   
    p1 <- duplicate(reproduction.env(GA.env)$pop[[i]][p1.loc])
    p2 <- duplicate(reproduction.env(GA.env)$pop[[i]][p2.loc])
    rest <- duplicate(reproduction.env(GA.env)$pop[[i]][rest.loc])
  
    #Perform reproduction
    #Mutate
    restResults = reproduction.env(GA.env)$mutate(rest)
    p1Results = reproduction.env(GA.env)$mutate(p1)
    p2Results = reproduction.env(GA.env)$mutate(p2)
    #Store Mutation results
    mutation.results[[i]] <- c(restResults, p1Results, p2Results)
    #CrossOver
    xover.results[[i]] <- chr.xover(p1, p2, reproduction.env(GA.env)$xover.swapMask)
    xover.results[[i]]@returnList$p1 = p1.loc
    xover.results[[i]]@returnList$p2 = p2.loc
    #Create the next population
    new.pop[i] <- new.population(organisms = c(elite[[i]], p1, p2, rest))
  }
  
  #Set the current population to the new population
  add.population(reproduction.env(GA.env), new.pop)
    
  #Report on the new population
  GA.env$reporting.fn(pop = new.pop, mutation = mutation.results, cross = xover.results, elite = elite, GA.env = GA.env)
}
