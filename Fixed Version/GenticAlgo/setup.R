############
### Input GA parameters

# GA.parameters: fitness.fn, decode.fn, goal.fn, epsilon, max.gen
# GA.parameters: 

# chr.encode
#   	chr.encode.type: character = {"binary", "spin", "symbolic", "seq", "integer", "dist", "real"}
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
#   |-- Fitness Environment: fitness.fn, decode.fn, goal.fn, epsilon

new.GA.env <- function(pop.size = 100,
                       GA.base.args = new.GA.base.args(), encoding.args = new.encoding.args(),
                       mutation.args = new.mutation.args(),
                       fitness.args = new.fitness.args(),
                       xover.prob = 0.8, xover.type = "uniform", xover.args = c(xover.alpha = 0.3),
                       selection.args = new.selection.args(),
                       reporting.fn = base.reporting.fn){
  GA.env <- new.env()
  setup.GA.envTree(GA.env)
  setup.GA.base.env(GA.env, GA.base.args)
  setup.encoding.env(GA.env, encoding.args)
  setup.fitness.env(GA.env, fitness.args)
  add.to.env(GA.env, GA.base.args)
  add.to.env(GA.env, mutation.args)
  add.to.env(GA.env, xover.args)
  add.to.env(GA.env, selection.args)
  add.to.env(GA.env, pop.size)
  GA.env$pop.size <- pop.size
  setup.elitism(GA.env)
  setup.reproduction.env(GA.env, mutation.args)
  setup.selection.env(GA.env, selection.args)
  setup.reporting(GA.env, reporting.fn)
  GA.env
}

control.env <- function(GA){GA$control.env}
# `control.env<-` <- function(GA, value){GA$control.env <- value}
encoding.env <- function(GA){GA$encoding.env}
# `encoding.env<-` <- function(GA, value){GA$encoding.env <- value}
fitness.env <- function(GA){GA$fitness.env}
# `fitness.env<-` <- function(GA, value){GA$fitness.env <- value}
selection.env <- function(GA){GA$selection.env}
# `selection.env<-` <- function(GA, value){GA$selection.env <- value}
reproduction.env <- function(GA){GA$reproduction.env}
# `reproduction.env<-` <- function(GA, value){GA$reproduction.env <- value}

add.population <- function(env, popn){
  env$pop <- popn
}

setup.GA.envTree <- function(GA.env) {
  GA.env$encoding.env <- new.env(parent = GA.env)
  GA.env$fitness.env <- new.env(parent = GA.env)
  GA.env$selection.env <- new.env(parent = GA.env)
  GA.env$reproduction.env <- new.env(parent = GA.env)
}

new.GA.base.args <- function(max.gen = 100){
  max.gen
  as.list(environment())
}

setup.GA.base.env <- function(GA.env, GA.base.args = list()){
  add.to.env(GA.env, GA.base.args)
}

new.encoding.args <- function(chr.length = 30, chr.encode.type = "binary", 
                              gene.alphabet = NULL, rdist = NULL, rdist.args = NULL, 
                              gene.mode = NULL, seq.args = NULL){
  chr.length; chr.encode.type; gene.alphabet; rdist; rdist.args; gene.mode; seq.args
  as.list(environment())
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
    
    new.chromosome <- function(chr.length){
      new("chromosome", chr.length, new.genes.fn)
    }
    
  })	
}

new.fitness.args <- function(fitness.fn = one.max.fn, fitnessFn.args = NULL,
                             decode.fn = identity, decodeFn.args = NULL,
                             goal.fn = NULL, goalFn.args = NULL,
                             epsilon = 0){
  fitness.fn; fitnessFn.args; decode.fn; decodeFn.args; goal.fn; goalFn.args; epsilon
  as.list(environment())
}

setup.fitness.env <- function(GA.env, fitness.args){
  add.to.env(fitness.env(GA.env), fitness.args)
}

setup.reproduction.env <- function(GA.env, mutation.args = new.mutation.args(),
                                   xover.prob = 0.8, xover.type = "uniform", xover.args = c(xover.alpha = 0.3)){
  add.to.env(reproduction.env(GA.env), mutation.args)
  setup.mutation(GA.env)
  
  add.to.env(reproduction.env(GA.env), xover.args)
  setup.xover(reproduction.env(GA.env))
}

new.mutation.args <- function(gene.delta = NULL, gene.sd = NULL, prob.mutation = 2, mutation.type = "binary", not.equal.op = `!=`){
  gene.delta; gene.sd
  as.list(environment())
  
}

setup.mutation <- function(GA.env){
  with(reproduction.env(GA.env), {
    if(is.integer(prob.mutation)) 
      prob.mutation <- prob.mutation / chr.length
    
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

mutate.fn <- function(reproduction.env){reproduction.env$mutate}
prob.mutation <- function(rep.env){rep.env$prob.mutation}
new.alleles <- function(rep.env){rep.env$alleles.fn}

new.xover.args <- function(xover.alpha = NULL, xover.k = NULL){
  xover.alpha; xover.k
  as.list(environment())
}

choose.xover.swapMask <- function(reproduction.env){
  with(reproduction.env, {
    xsm.fgen <- xover.swapMask.fgen
    switch(xover.type, 
           one.pt	= xover.mask.1point,
           two.pt	= xsm.fgen(xover.mask.kpoint, 2),
           k.pt 		= xsm.fgen(xover.mask.kpoint, xover.k),
           uniform 	= xsm.fgen(xover.mask.uniform, xover.alpha),
           ...		= simple.error(paste("xover.type must be one of '1pt', '2pt', 'kpt' or 'uniform'. ", 
                                     "Instead it is '", xover.type, "'", sep = "")))
  })
}

setup.xover <- function(reproduction.env){
  with(reproduction.env, {
    xover.swapMask <- choose.xover.swapMask(environment())
    xover <- xover.fgen(xover.swapMask)
  })
}

xover.fn <- function(GA.env){GA.env$xover}


setup.selection.env <- function(GA.env, selection.args = new.selection.args()){
  
  add.to.env(selection.env(GA.env), selection.args)
  with(selection.env(GA.env), {
  
    if(is.integer(prob.mutation)) prob.mutation <- prob.mutation / chr.length
    selection.env <- environment()
    add.to.env(selection.env, selection.args)
    setup.elitism(selection.env)
      
    select.fgen <- select.population.fgen
    select.chr <- switch(selection.type, 
                         simple.tournament  = select.fgen(simple.tournament.selection, fitness.env(parent.env(environment())), tourn.size, decreasing, `%>%`),
                         tournament  			= select.fgen(tournament.selection, fitness.env(parent.env(environment())), tourn.size, prob.select.worse, 
                                                      decreasing, `%>%`),
                         fps 						= fitnessProportional.selection,
                         rank 						= rank.selection,  # not yet implemented ... just a stub
                         ...						= simple.error(paste("select.chr.type must be one of ", 
                                                       "'simple.tournament', 'tournament', 'fps' or 'rank'. ", 
                                                       "Instead it is '", select.chr.type, "'", sep = "")))
    
    selection.env
  })
}

add.to.env <- function(env, arg.list){
  arg.names <- names(arg.list)
  for(name in arg.names){
    env[[name]] <- arg.list[[name]]
    
  }
}

arg.list.name <- function(obj.name){
  length(grep("*.args", obj.name)) > 0
}

controller.name <- function(obj.name){
  length(grep("*.controller", obj.name)) > 0
}

print.ga.env <- function(env){
  for(obj.name in objects(env)){
    if(!arg.list.name(obj.name) && !controller.name(obj.name)){
      value <- env[[obj.name]]
      if(!is.function(value) && !is.environment(value))
        cat(obj.name, "=", value, "\n")
    }
  }
}


new.selection.args <- function(selection.type = "simple.tournament", tourn.size = 2, prob.select.worse = 0, decreasing = TRUE, `%>%` = `>`, elitism = TRUE, elite.size = 2){
  selection.type; tourn.size; prob.select.worse; decreasing; `%>%`; elitism; elite.size
  as.list(environment())
}

new.elite.args <- function(elite.size = 2, decreasing = TRUE){
  elite.size; decreasing	
  as.list(environment())
}

setup.elitism <- function(GA.env){
  with(GA.env, {
    select.fgen <- select.elite.population.fgen
    if(elitism)
      select.elite <- select.fgen(elite.fn = elite.selection, elite.size = elite.size, decreasing = decreasing)
    else
      select.elite <- NULL
  })
}

setup.reporting <- function(GA.env, reporting.fn){
    GA.env$reporting.fn = reporting.fn
}