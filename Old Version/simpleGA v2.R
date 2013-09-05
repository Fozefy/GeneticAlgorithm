addLeadingZeros <- function(value, max = 1){
	max.digits <- ceiling(log(max+1, 10))
	formatC(value,width = max.digits, flag="0")
}

addLeadingBlanks <- function(max = 0){
	if(max == 0)
		""
	else{
		max.digits <- ceiling(log(max+1, 10)) + 3
		formatC(" ",width = max.digits, flag=" ")
	}
}

extend.env <- function(env, var.list){
	var.names <- names(var.list)
	for(name in names(var.list)){
		value <- var.list[[name]] 
		env[[name]] <- var.list[[name]]
	}
	env
}


### population definition and creation section

### Fitness Class (needed for multi-objective, stochastic and other complex fitnesses)

setClass("fitness", representation(value = "numeric"))
setMethod("initialize", 
			  signature = "fitness",
          definition = function(.Object, chr = NULL, fitness.fn = function(chr) NULL, ...) {
            .Object@value <- fitness.fn(chr)
            .Object
          })


### Chromosome Class

setClass("chromosome", representation(chr.genes = "environment", fitness = "environment"))
setMethod("initialize", 
	signature = "chromosome",
   definition = function(.Object, chr.length, 
          						rdist = function(n) sample(c(0,1), n, replace=TRUE),
          						fitness.fn = function(chr) {numeric(0)},
          						decode.fn = identity,
          						 ...) {
		genes.env <- new.env()
		genes.env$genes <- rdist(chr.length)
		.Object@chr.genes <- genes.env
		
		fitness.env <- new.env()
		fitness.env$value <- fitness.fn(decode.fn(.Object))
		.Object@fitness <- fitness.env
		
		.Object
	}
)

new.chromosome <- function(GA.env){
	new("chromosome", GA.env$chr.length, rdist = GA.env$new.genes.fn)
}
						  
chromosome <- function(object){object@chr.genes[["genes"]]}
setGeneric("chromosome")
genes <- function(object){object@chr.genes[["genes"]]}
setGeneric("genes")

setMethod("[", 
	signature = c("chromosome"),
	definition = function(x,i,j,...,drop){
		chromosome(x)[i]
	}
)

setMethod("[[", 
	signature = c("chromosome"),
	definition = function(x,i,j,...,drop){
		chromosome(x)[[i]]
	}
)

setMethod("[[<-", 
	signature = c("chromosome", "ANY", "ANY"),
	definition = function(x, i, value){
		x@chr.genes[["genes"]][[i]] <- value
		x
	}
)

setMethod("[<-", 
	signature = c("chromosome", "ANY", "ANY"),
	definition = function(x, i, value){
		x@chr.genes[["genes"]][i] <- value
		x
	}
)


setMethod("length", 
	signature = c("chromosome"),
	definition = function(x){
		length(chromosome(x))
	}
)

setGeneric("mode")
setMethod("mode", 
	signature = c("chromosome"),
	definition = function(x){
		mode(chromosome(x))
	}
)

unevaluated <- function(obj){
	length(fitness(obj)) == 0	
}

setGeneric("unevaluated")

setMethod("unevaluated", 
	signature = c("list"),
	definition = function(obj){
		n <- length(obj)
		answer <- vector("logical", n)
		for(i in 1:n){
			answer[[i]] <- unevaluted(obj[[i]])
		}
	}
)

is.multiobjective <- function(obj){
	length(fitness(obj)) > 1
}

setGeneric("is.multiobjective")

setMethod("is.multiobjective", 
	signature = c("list"),
	definition = function(obj){
		is.multiobjective(obj[[1]])
	}
)

fitness <- function(obj) {
	(obj@fitness)$value
}

setGeneric("fitness")

setMethod("fitness", 
	signature = c("list"),
	definition = function(obj){
		n <- length(obj)
		if(is.multiobjective(obj))
			fit.vector <- vector("list", n)
		else
			fit.vector <- vector("numeric", n)
		for(i in 1:n){
			fit.vector[[i]] <- fitness(obj[[i]])
		}
		fit.vector	
	}
)

`fitness<-` <- function(chr, value){
	chr@fitness$value <- value
	chr@fitness
}

setGeneric("fitness<-")

new.genes.fgen <- function(new.chromosome.fn, ...){
	function(chr.length) new.chromosome.fn(chr.length, ...)
}

new.genes.symbolic <- function(chr.length = 1, alphabet=c('A','C','G','T')){
	sample(alphabet,chr.length, replace=TRUE)
}

new.genes.binary <- function(chr.length = 1){
	new.genes.symbolic(chr.length, c(0,1))
}

new.genes.spin <- function(chr.length = 1){
	new.genes.symbolic(chr.length, c(-1,1))
}

new.genes.seq <- function(chr.length = 1, seq.args){
	new.genes.symbolic(chr.length, alphabet = do.call(seq, seq.args))
}

new.genes.integer <- new.genes.seq

new.genes.dist <- function(chr.length = NULL, rdist=rnorm, rdist.args = list()){
	if(!is.null(chr.length))
		rdist.args <- c(chr.length, rdist.args)
	do.call(rdist, rdist.args)
}

new.genes.real <- new.genes.dist

duplicate <- function(obj, ...){
	obj  
	"Generic base function; does nothing."
}

setGeneric("duplicate")

setMethod("duplicate",
	signature = c("environment"),
	definition = function(obj, ...){
		env <- new.env() 
		for(name in objects(obj)){
			env[[name]] <- obj[[name]]
		}
		env
	}
)

setMethod("duplicate",
	signature = c("chromosome"),
	definition = function(obj, ...){
		obj@chr.genes <- duplicate(obj@chr.genes)
		obj@fitness <- duplicate(obj@fitness)
		obj
	}
)

get.open.delimiter <- function(gene.brackets){
	if(is.null(gene.brackets))
		""
	else if(length (gene.brackets) > 1)
		gene.brackets[[1]] 
	else if(nchar(gene.brackets) == 0)
		""
	else if(nchar(gene.brackets) == 1)
		gene.brackets
	else
		substr(gene.brackets, 1, 1)
}

get.close.delimiter <- function(gene.brackets){
	if(is.null(gene.brackets))
		""
	else if(length (gene.brackets) > 1)
		gene.brackets[[2]] 
	else if(nchar(gene.brackets) == 0)
		""
	else if(nchar(gene.brackets) == 1)
		gene.brackets
	else
		substr(gene.brackets, 2, 2)
}

ga.print <- function(obj, ...) cat(obj, ...)
setGeneric("ga.print")

setMethod("ga.print",
	signature = c("chromosome"),
	definition = function(obj, max = 0, gene.sep = " ", genes.delimiters = NULL, 
									fitness.sep = "", fitness.delimiters = NULL, ...){
		genes.delimiter.open <- get.open.delimiter(genes.delimiters)
		genes.delimiter.close <- get.close.delimiter(genes.delimiters)
		fitness.delimiter.open <- get.open.delimiter(fitness.delimiters)
		fitness.delimiter.close <- get.close.delimiter(fitness.delimiters)
		
		cat("genes =", genes.delimiter.open)
		cat(genes(obj), sep=gene.sep)
		cat(genes.delimiter.close)
		
		if(!unevaluated(obj)){
			cat("; fit =", fitness.delimiter.open)
			cat(fitness(obj), sep = fitness.sep)
			cat(fitness.delimiter.close)
		}
	}
)

setMethod("ga.print",
	signature = c("list"),
	definition = function(obj, line.numbers = TRUE, ...) {
		n <- length(obj)
		for(i in 1:n){
			if(line.numbers)
				cat("[", addLeadingZeros(i, n), "] ", sep = "") 
			ga.print(obj[[i]], max = n, ...)
			cat("\n")	
		}	
	}
)


#### Population Class

setClass("population", 
			 representation(chromosomes = "environment", 
			 					  fitness.cache = "environment",
			 					  pop.size = "numeric"))

setMethod("initialize", 
	signature = "population",
	definition = function(.Object, GA.env = NULL, chromosomes = NULL, ...) {
		chr.env <- new.env()
		if(is.null(chromosomes)){
			if(!is.null(GA.env))
				chromosomes <- create.random.chromosomes(GA.env)
			else
				simple.error("Either GA.env or chromosomes must be defined. Both are NULL.")
		}
		chr.env$values <- chromosomes
		.Object@chromosomes <- chr.env
            
		fit.cache <- new.env()
		fit.cache$values <- numeric(0)
		.Object@fitness.cache <- new.env()
            
		.Object@pop.size <- length(chromosomes)
            
		.Object
	}
)

create.random.chromosomes <- function(GA.env){
	P <- GA.env$pop.size
	chromosomes <- vector("list", P)
	for(i in 1:P){
		chromosomes[[i]] <- new.chromosome(GA.env)
	}
	chromosomes
}

new.population <- function(GA.env){
	new("population", GA.env)	
}

chromosomes <- function(object){object$values}
setGeneric("chromosomes")

setMethod("chromosomes", 
	signature = c("population"),
	definition = function(object){
		chromosomes(object@chromosomes)
	}
)

setMethod("fitness", 
	signature = c("population"),
	definition = function(obj){
		fitness(chromosomes(obj))
	}
)

fitness.values <- function(object){object$values}
setGeneric("fitness.values")

setMethod("fitness.values", 
	signature = c("environment"),
	definition = function(object){
		object$values
	}
)

setMethod("fitness.values", 
	signature = c("population"),
	definition = function(object){
		fitness.values(object@fitness.cache)
	}
)

setMethod("[", 
	signature = c("population"),
	definition = function(x,i,j,...,drop){
		chromosomes(x)[i]
	}
)

setMethod("[[", 
	signature = c("population"),
	definition = function(x,i,j,...,drop){
		chromosomes(x)[[i]]
	}
)

setMethod("[[<-", 
	signature = c("population", "ANY", "ANY"),
	definition = function(x, i, value){
		x@chromosomes[["values"]][[i]] <- value
		x
	}
)

setMethod("[<-", 
	signature = c("population", "ANY", "ANY"),
	definition = function(x, i, value){
		x@chromosomes[["values"]][i] <- value
		x
	}
)

`fitness.cache<-` <- function(pop, value){
	pop@fitness.cache[["values"]] <- value
	pop@fitness.cache
}

size <- function(pop){
	length(chromosomes(pop))
}
setGeneric("size")
setMethod("size",
	signature = c("population"),
	definition = function(pop){
		pop@pop.size
	}
)

setMethod("duplicate",
	signature = c("list"),
	definition = function(obj, ...){
		for(i in 1:length(obj)){
			obj[[i]] <- duplicate(obj[[i]])
		}
		obj
	}
)

setMethod("duplicate",
	signature = c("population"),
	definition = function(obj, ...){
		obj@fitness.cache <- duplicate(obj@fitness.cache)
		obj@chromosomes <- duplicate(obj@chromosomes)
		obj@chromosomes[["values"]] <- duplicate(obj@chromosomes[["values"]])
		obj
	}
)

setMethod("ga.print",
	signature = c("population"),
	definition = function(obj, ...){
		ga.print(chromosomes(obj), ...)
	}
)

#### Reproduction section

## mutation
alleles.fgen <- function(alleles.fn, ...){
	function(genes) alleles.fn(genes, ...)
}

alleles.binary <- function(genes){
	1 - genes	
}

alleles.boolean <- function(genes){
	!genes	
}

alleles.complement <- function(genes, alphabet){
	alleles <- vector(mode(genes),length(genes))
	for(i in 1:length(genes)){
		alleles[i] <- alphabet[(alphabet != genes[i])]
	}
	alleles	
}

alleles.spin <- function(genes){
	alleles.complement(genes, c(-1,1))
}

alleles.symbolic <- function(genes, alphabet){
	alleles <- vector(mode(genes),length(genes))
	for(i in 1:length(genes)){
		alleles[[i]] <- sample(alphabet[(alphabet != genes[[i]])],1)
	}
	alleles
}

# alleles.seq <- function(genes, ...){
#	alleles.symbolic(genes, seq(...))
# }
		
alleles.integer <- function(genes, values){
	alleles.symbolic(genes, values)
}

alleles.creep <- function(genes, max = Inf, min = -Inf){
	new.genes <- genes + sample(c(-1,1),length(genes), replace=TRUE)
	new.genes <- pmin(new.genes, max)
	new.genes <- pmax(new.genes, min)
	new.genes
}

alleles.uniform <- function(genes, delta=1){
	genes + runif(length(genes), -delta/2, delta/2)
}

alleles.gaussian <- function(genes, sd=1){
	genes + rnorm(count,0,sd)
}



chr.mutate <- function(chr, mutater, mutation.locations = NULL, mutations = NULL){
	"generic base function does nothing"
}

setGeneric("chr.mutate")

setMethod("chr.mutate",
	signature = c("chromosome", "environment", "ANY", "ANY"),
	definition = function(chr, repr.env, mutation.locations = NULL, mutations = NULL){
		chr.length <- length(chr)
		`%!=%` <- not.equal.op(repr.env)
		if(is.null(mutation.locations))
			mutation.locations <- (runif(chr.length) < prob.mutation(repr.env))
		genes <- chr[mutation.locations]
		if(length(genes) > 0){
			if(is.null(mutations))
				alleles <- new.alleles(repr.env)(genes)
			chr[mutation.locations] <- alleles
		} else {
			alleles = integer(0)
		}
		returnValues.mutation(sum(alleles %!=% genes), (1:chr.length)[mutation.locations], genes, alleles)
	}
)

setMethod("chr.mutate",
	signature = c("list", "environment", "ANY", "ANY"),
	definition = function(chr, repr.env, mutation.locations = NULL, mutations = NULL){
		n <- length(chr)
		returnValues <- vector("list", n)
		for(i in 1:n)
			returnValues[[i]] <- chr.mutate(chr[[i]], repr.env, mutation.locations[[i]], mutations[[i]])
		new("returnList", returnValues)
	}
)

mutate.fgen <- function(repr.env){
	function(chr){chr.mutate(chr, repr.env)}
}

setClass("returnValues.mutation",
	representation(mutation.count = "numeric", 
						 mutation.locations = "numeric", 
						 from.genes = "ANY", 
						 to.genes = "ANY"))

returnValues.mutation <- function(mutation.count, mutation.locations, from.genes, to.genes){
	new("returnValues.mutation", 
		mutation.count = mutation.count, 
		mutation.locations = mutation.locations, 
		from.genes = from.genes, 
		to.genes = to.genes)
}

mutation.count <- function(obj){obj@mutation.count} 
mutation.locations <- function(obj){obj@mutation.locations} 
from.genes <- function(obj){obj@from.genes} 
to.genes <- function(obj){obj@to.genes} 
						 
setClass("returnList",
	representation(returnList = "environment"))

setMethod("initialize", 
	signature = "returnList",
   definition = function(.Object, ...) {
   	returnList.env <- new.env()
   	returnList.env$values <- list()
   	.Object@returnList <- returnList.env
   	.Object
   }
)

setMethod("initialize", 
	signature = "returnList",
   definition = function(.Object, return.value, ...) {
   	returnList.env <- new.env()
   	if(is.list(return.value))
   		returnList.env$values <- return.value
   	else
   		returnList.env$values <- list(return.value)
   	.Object@returnList <- returnList.env
   	.Object
   }
)


returnList <- function(x){x@returnList[["values"]]}

setMethod("[", 
	signature = c("returnList"),
	definition = function(x,i,j,...,drop){
		returnList(x)[i]
	}
)

setMethod("[[", 
	signature = c("returnList"),
	definition = function(x,i,j,...,drop){
		returnList(x)[[i]]
	}
)

setMethod("ga.print",
	signature = c("returnValues.mutation"),
	definition = function(obj, max = 0, ...){
		cat("mcount =", mutation.count(obj), "\n")
		cat(addLeadingBlanks(max))
		cat("loc    =", mutation.locations(obj), "\n") 
		cat(addLeadingBlanks(max))
		cat("from   =", from.genes(obj), "\n")
		cat(addLeadingBlanks(max))
		cat("to     =", to.genes(obj), "\n")
	}
)

setMethod("ga.print",
	signature = c("returnList"),
	definition = function(obj, ...){
		ga.print(returnList(obj), ...)
	}
)			


## crossover
chr.xover <- function(chr1, chr2, swapMask.fn = xover.mask.2point, swapMask=NULL){
	chr1; chr2; swapMask.fn; swapMask
	"generic base function does nothing"
}

setGeneric("chr.xover")

setMethod("chr.xover",
	signature = c("chromosome", "chromosome", "function", "ANY"),
	definition = function(chr1, chr2, swapMask.fn = xover.mask.2point, swapMask=NULL){
		if(is.null(swapMask))
			swapMask <- swapMask.fn(length(chr1))
		swap.genes(chr1, chr2, swapMask)
		returnValues.xover(swapMask)
	}
)

setMethod("chr.xover",
	signature = c("list", "list", "function", "ANY"),
	definition = function(chr1, chr2, swapMask.fn, swapMask=NULL){
		n <- length(chr1)
		return.values <- vector("list", n)
		for(i in 1:n)
			return.values[[i]] <- chr.xover(chr1[[i]], chr2[[i]], swapMask.fn, swapMask[[i]])
		new("returnList", return.values)
	}
)

xover.fgen <- function(swapMask.fn){
	function(chr1, chr2){chr.xover(chr1, chr2, swapMask.fn)}
}

xover.swapMask.fgen <- function(create.swapMask, ...){
	function(chr.length){
		create.swapMask(chr.length, ...)
	}
}

xover.mask.kpoint <- function(chr.length, k, verbose=FALSE){
	xPts <- sort(sample(0:(chr.length-1), k, replace=TRUE))
	if(verbose)
		print(xPts)
	swapMask <- vector("logical", chr.length) # default value is FALSE
	from <- 1; swap <- FALSE
	for(to in xPts){
		if(from <= to)
			swapMask[from:to] <- swap
		swap <- !swap
		from <- to + 1
	}
	
	if(from <= chr.length){
		swapMask[from:chr.length] <- swap
	}
	
	swapMask
} 

xover.mask.1point <- function(chr.length, verbose=FALSE){
	xoverPoint <-sample(1:chr.length, 1)
	if(verbose)
		print(xoverPoint)
	swapMask <- vector("logical", chr.length) # default value is FALSE
	swapMask[xoverPoint:chr.length] <- TRUE
	swapMask
}

xover.mask.2point <- xover.swapMask.fgen(xover.mask.kpoint, 2)

xover.mask.uniform <- function(chr.length, alpha=0.5){
	runif(chr.length) < alpha
}

returnValues.xover <- function(swap.mask, xover.points = NULL){
	swap.mask; xover.points
	environment()	
}

setClass("returnValues.xover",
	representation(swap.mask = "logical"))

returnValues.xover <- function(swap.mask){
	new("returnValues.xover", swap.mask = swap.mask)
}

swap.mask <- function(rv.xover){rv.xover@swap.mask}

swap.genes <- function(chr1, chr2, mask){
	chr1; chr2; mask  
	"generic base function does nothing"
}

setGeneric("swap.genes")

setMethod("swap.genes",
	signature = c("chromosome", "chromosome", "logical"),
	definition = function(chr1, chr2, mask){
		temp <- duplicate(chr1)
		temp[mask] <- chr1[mask]
		chr1[mask] <- chr2[mask]
		chr2[mask] <- temp[mask]
	}
)

swapMask2xpts <- function(swap.mask){
	mask.length <- length(swap.mask)
	swap.mask.shift <- c(F, swap.mask)[1:mask.length]
	(1:mask.length)[(swap.mask != swap.mask.shift)] - 1
}

logic.print.vector <- function(logic.vector, false.char="F", true.char="T"){
	vector.length <- length(logic.vector)
	ifelse(logic.vector,rep(true.char, vector.length), rep(false.char, vector.length))
}

setMethod("ga.print",
	signature = c("returnValues.xover"),
	definition = function(obj, max = 0, print.xpts=FALSE, mask.sep=" ", ...){
		sm <- swap.mask(obj)
		cat("Xover Information\n")
		cat(addLeadingBlanks(max))
		cat("   swap mask = ") 
		cat(logic.print.vector(sm), "\n", sep = mask.sep)
		if(print.xpts)
			cat("   xover pts =", swapMask2xpts(sm), "\n")
	}
)


## Evaluation

evaluate <- function(obj, fitness.fn, decode.fn, ...){
	"generic base function does nothing"
}

setGeneric("evaluate")

setMethod("evaluate",
	signature = c("chromosome", "function"),
	definition = function(obj, fitness.fn, decode.fn = identity, ...) {
		obj@fitness$value <- fitness.fn(decode.fn(obj))
		obj@fitness$value
	}
)

setMethod("evaluate", 
	signature = c("list", "function"),
	definition = function(obj, fitness.fn, decode.fn = identity, ...) {
		n <- length(obj)
		fit1 <- evaluate(obj[[1]], fitness.fn, decode.fn, ...)
		
		if(is.multiobjective(obj[[1]]))
			fit.cache <- vector("list", n)
		else
			fit.cache <- vector("numeric", n)
		
		fit.cache[[1]] <- fit1
			
		for(i in 2:n){
			fit.cache[[i]] <- evaluate(obj[[i]], fitness.fn, decode.fn, ...)
		}
		
		fit.cache
	}
)

setMethod("evaluate", 
	signature = c("population", "function"),
	definition = function(obj, fitness.fn, decode.fn = identity, ...) {
		fit.vector <- evaluate(chromosomes(obj), fitness.fn, decode.fn, ...)
		fitness.cache(obj) <- fit.vector
	}
)

setMethod("evaluate", 
	signature = c("population", "environment"),
	definition = function(obj, fitness.fn, ...) {
		fitness.env <- fitness.fn
		add.population(fitness.env, obj)
		with(fitness.env, {
			fit.vector <- evaluate(chromosomes(pop), fitness.fn, decode.fn, ...)
			fitness.cache(obj) <- fit.vector
		})
	}
)



## Selection

select.population.fgen <- function(selection.fn, ...){
	function(selection.size, pop=NULL)
		selection.fn(selection.size, pop, ...)
}

# tournament selection

new.tournaments.locations <- function(selection.size, tourn.size, pop.size){
	n <- selection.size; k <- tourn.size; P <- pop.size 
	tourn.loc <- matrix(nrow = k, ncol = n)
	for(i in 1:k){
		tourn.loc[i,] <- sample(1:P, n, replace=TRUE)
	}
	tourn.loc
}

new.tournaments.fitness <- function(selection.size, tourn.size, pop = NULL, 
												    tourn.loc = NULL, rprob = runif){
	n <- selection.size; k <- tourn.size 
	tourn.fit <- matrix(nrow = k, ncol = n)
	if(is.null(pop))
		for(i in 1:k)
			tourn.fit[i,] <- rprob(n)
	else 
		for(i in 1:k)
			tourn.fit[i,] <- get.fitness(pop, tourn.loc[i,])
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

tournament.selection <- function(selection.size, pop = NULL, tourn.size = 2, prob.select.worse = 0, 
											 decreasing = FALSE, `%>%` = NULL,
											 tourn.fit = NULL, tourn.loc = NULL, 
											 select.worse = NULL, pop.size = NULL, verbose = FALSE){
	n <- selection.size; k <- tourn.size 
	P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, size.population(pop))
	if(is.null(`%>%`))
		if(decreasing) {`%>%` <- `>`} else {`%>%` <- `<`}
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

simple.tournament.selection <- function(selection.size, pop = NULL, tourn.size = 2, 
												decreasing = FALSE, `%>%` = NULL,
												tourn.fit = NULL, tourn.loc = NULL, 
												pop.size = NULL, verbose = FALSE){
	n <- selection.size; k <- tourn.size
	P <- ifelse(is.null(pop) || !is.null(pop.size), pop.size, size.population(pop)) 
	if(is.null(`%>%`))
		if(decreasing) {`%>%` <- `>`} else {`%>%` <- `<`}
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

#### Elitism

select.elite.population.fgen <- function(elite.fn, ...){
	function(pop=NULL)
		elite.fn(pop, ...)
}

#note: elite.selection only works with integer or floating point fitness ordered by <= or >=

elite.selection <- function(pop = NULL, elite.size = 1, decreasing = TRUE, 
										pop.fit = NULL, verbose = FALSE){
	P <- ifelse(is.null(pop.fit), size.population(pop), length(pop.fit))
	if(is.null(pop.fit))
		pop.fit <- get.fitness(pop)
	pop.loc <- order(pop.fit, decreasing = decreasing)
	if (verbose) print(pop.fit)
	pop.loc[1:elite.size]
}

# elite.selection(elite.size = 2, pop.fit = runif(10))

truncation.selection <- elite.selection

##### Genetic Algorithm

generational.ga <- function(GA.env){
	with(GA.env, {
		pop <- new.population(encoding.env)
		repr.results <- NULL
		for(gen in 0:max.gen){
			goal.reached <- evalutate(pop, fitness.env)
			report(gen, pop, repr.stats(repr.results), goal.reached)
			if (goal.reached)
				break
			repr.results <- next.generation(pop, reproduction.env)
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

next.generation <- function(popn, repr.env){
	add.population(repr.env, popn)
	with(repr.env, {
		P <- size(pop)
		if(is.null(select.elite))
			elite.loc = NULL
		else
			elite.loc <- select.elite(pop)
	
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

############
### Input GA parameters

# GA.parameters: fitness.fn, decode.fn, goal.fn, epsilon, max.gen
# GA.parameters: 

# chr.encode
# 		chr.encode.type: character = {"binary", "spin", "symbolic", "seq", "integer", "dist", "real"}
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

setClass("ga.controller",
	representation(control.env = "environment",
						 encoding.env = "environment",
						 fitness.env = "environment", 
						 selection.env = "environment" 
						 reproduction.env = "environment"))
)

new.GA.env <- function(control.env = new.env(), encoding.env = new.env(), fitness.env = new.env(),
   							   selection.env = new.env(), reproduction.env = new.env(), ...) {
   	control.env
		parent.env(fitness.env) <- encoding.env
		parent.env(selection.env) <- encoding.env
		parent.env(reproduction.env) <- selection.env
		
		environment()
   }
)

control.env <- function(GA){GA$control.env}
`control.env<-` <- function(GA, value){GA$control.env <- value}
encoding.env <- function(GA){GA$encoding.env}
`encoding.env<-` <- function(GA, value){GA$encoding.env <- value}
fitness.env <- function(GA){GA$fitness.env}
`fitness.env<-` <- function(GA, value){GA$fitness.env <- value}
selection.env <- function(GA){GA$selection.env}
`selection.env<-` <- function(GA, value){GA$selection.env <- value}
reproduction.env <- function(GA){GA$reproduction.env}
`reproduction.env<-` <- function(GA, value){GA$reproduction.env <- value}

add.population(env, popn){
	env$pop <- popn
}

setup.encoding.env <- function(GA, pop.size = 100, chr.length = 30, 
										 chr.encode.type = "binary", chr.encode.args = NULL){
	add.to.env(encoding.env(GA), environment())
	add.to.env(encoding.env(GA), chr.encode.args)
	setup.chromosome.encoding(encoding.env(GA))
}

chr.encode.args <- function(gene.alphabet = NULL, rdist = NULL, rdist.args = NULL, 
											 gene.mode = NULL, seq.args = NULL){
	gene.alphabet; rdist; rdist.args; gene.mode; seq.args
	as.list(environment())
}

setup.chromosome.encoding <- function(encoding.env, chr.encode.type){
	with(encoding.env, {
		if(chr.encode.type == "seq" || chr.encode.type == "integer"){
			gene.values <- do.call(seq, seq.args)
			gene.max <- max(gene.values)
			gene.min <- min(gene.values)
		}
			
		new.genes.fn <- switch(chr.encode.type,
				binary 	= new.genes.binary,
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
		
		rm("GA")
	})	
}



setup.fitness.env <- function(GA, fitness.fn = one.max.fn, fitness.args = NULL,
										decode.fn = identity, decode.args = NULL,
										goal.fn = reached.maximum, goal.args = NULL,
										epsilon = 0){
	add.to.env(fitness.env(GA), environment())
	add.to.env(fitness.env(GA), fitness.args)
	add.to.env(fitness.env(GA), decode.args)
	add.to.env(fitness.env(GA), goal.args)
	rm("GA", envir = fitness.env(GA))
}

setup.reproduction.env <- function(GA, prob.mutation = 2, mutation.type = "binary", mutation.args = NULL,
												 xover.prob = 0.8, xover.type = "uniform", xover.args = c(xover.alpha = 0.3)){
	add.to.env(reproduction.env(GA), environment())
	add.to.env(reproduction.env(GA), mutation.args)
	setup.mutation(reproduction.env(GA))

	add.to.env(reproduction.env(GA), xover.args)
	setup.xover(reproduction.env(GA))
}

new.mutation.args <- function(gene.delta = NULL, gene.sd = NULL){
	gene.delta; gene.sd
	as.list(environment())
}

setup.mutation <- function(reproduction.env){
	with(reproduction.env, {
		if(is.integer(prob.mutation)) 
			prob.mutation <- prob.mutation / chr.length

		alleles.fn <- switch(mutation.type, 
			binary 		= alleles.binary,
			boolean 		= alleles.boolean,
			complement 	= alleles.fgen(alleles.complement, gene.alphabet),
			spin 			= alleles.spin,
			symbolic 	= alleles.fgen(alleles.symbolic, gene.alphabet),
			integer 		= alleles.fgen(alleles.integer, gene.values),
			creap 		= alleles.fgen(alleles.creap, gene.max, gene.min),
			uniform 		= alleles.fgen(alleles.uniform, gene.delta),
			gaussian 	= alleles.fgen(alleles.gaussian, gene.sd),
			...			= simple.error(paste("mutation.type must be one of 'binary', 'boolean', 'complement', ", 
											          "'spin', 'symbolic', 'integer', 'creap', 'uniform' or 'gaussian'.",
											          "Instead it is '", mutation.type, "'", sep = "")))

		mutate <- mutate.fgen(environment())
	})
}

mutate.fn <- function(reproduction.env){reproduction.env$mutate}
prob.mutation <- function(rep.env){rep.env$prob.mutation}
new.alleles <- function(rep.env){rep.env$alleles.fn}
not.equal.op <- function(rep.env){rep.env$not.equal.op}

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
		xover.swapMask <- choose.xover.swapMask(enivronment())
		xover <- xover.fgen(xover.swapMask)
	})
}

xover.fn <- function(GA.env){GA.env$xover}


setup.selection.env <- function(selection.type = "simple.tournament", 
						selection.args = c(tourn.size = 2, prob.select.worse = 0, decreasing = TRUE, `%>%` = `>`),
						elitism = TRUE, elite.size = 2){
	if(is.integer(prob.mutation)) prob.mutation <- prob.mutation / chr.length
	selection.env <- environment()
	add.to.env(selection.env, selection.args)
#	setup.elitism(selection.env)
#	selection.controller <- create.selection.controller(selection.env, selection.type)
	selection.env
}

new.GA.env <- function(pop.size = 100,  
							 	 chr.length = 30, chr.encode.type = "binary", chr.encode.args = NULL,
								 prob.mutation = 2, mutation.type = "binary", mutation.args = NULL,
								 xover.prob = 0.8, xover.type = "uniform", xover.args = c(xover.alpha = 0.3),
								 selection.type = "simple.tournament", 
								 selection.args = c(tourn.size = 2, prob.select.worse = 0, decreasing = TRUE, `%>%` = `>`),
								 elitism = TRUE, elite.size = 2){
	if(is.integer(prob.mutation)) prob.mutation <- prob.mutation / chr.length
	GA.env <- environment()
	add.to.env(GA.env, chr.encode.args)
	add.to.env(GA.env, mutation.args)
	add.to.env(GA.env, xover.args)
	add.to.env(GA.env, selection.args)
#	setup.elitism(GA.env)
	setup.chromosome.encoding(GA.env, chr.encode.type)
	new.chromosome <- function(chr.length){new("chromosome", chr.length, new.genes.fn)}
	mutation.controller <- create.mutation.controller(GA.env, mutation.type)
	mutate <- mutate.fgen(mutation.controller)
	xover.swapMask <- choose.xover.swapMask(GA.env, xover.type)
	xover <- xover.fgen(xover.swapMask)
#	selection.controller <- create.selection.controller(GA.env, selection.type)
	GA.env
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



new.selection.args <- function(tourn.size = 2, prob.select.worse = 0, decreasing = TRUE, `%>%` = `>`){
	tourn.size; prob.select.worse; decreasing; `%>%`
	as.list(environment())
}

### need reworking 
setup.selection.controller <- function(GA, selection.type){
	with(GA.env, {
		select.fgen <- select.population.fgen
		select.chr <- switch(selection.type, 
			simple.tournament	= select.fgen(simple.tournament.selection, tourn.size, decreasing, `%>%`),
			tournament				= select.fgen(tournament.selection, tourn.size, prob.select.worse, 
												        decreasing, `%>%`),
			fps 						= fitnessProportional.selection,
			rank 						= rank.selection,  # not yet implemented ... just a stub
			...						= simple.error(paste("select.chr.type must be one of ", 
												               "'simple.tournament', 'tournament', 'fps' or 'rank'. ", 
											                  "Instead it is '", select.chr.type, "'", sep = "")))
	})
}

new.elite.args <- function(elite.size = 2, decreasing = TRUE){
	elite.size; decreasing	
	as.list(environment())
}

setup.elitism <- function(GA.env){
	with(GA.env, {
		select.fgen <- select.elite.population.fgen
		if(elitism)
			select.elite <- select.fgen(elite.selection, elite.size, decreasing)
		else
			select.elite <- NULL
	})
}