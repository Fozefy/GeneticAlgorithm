### Design of encoding 

# choose for new.chromosome creation function (as an argument to new.chromosome.fgen)
chr.length: type = integer
new.chromosome.binary:		type = function
new.chromosome.spin:		type = function
new.chromosome.symbolic:	type = function
	alphabet:	type = vector; e.g. = c('A','C','G','T')
	mode:			type = character;	    e.g. ="character"
new.chromosome.seq:			type = function 
	...:			type = seq(...) parameters; e.g. from = 10, to = 20, by = 2
new.chromosome.integer:		type = function 
	...:			type = seq(...) parameters; e.g. from = 10, to = 20, by = 2
new.chromosome.dist:		type = function 
	rdist: e.g. = rnorm 
	rdist.args: type = list (used as an arg list in a do.call); e.g. = list(sd = 5) 
	mode:			type = character;	e.g. = "numeric"
new.chromosome.real:		type = function 
	rdist: e.g. = rnorm 
	rdist.args: type = list (used as an arg list in a do.call); e.g. = list(sd = 5) 
	mode:			type = character;	e.g. = "numeric"

# design chromosome print
print.chromosome <- function(chr, gene.sep = " "){
print.chromosomes <- function(chr.vector, gene.sep = " ", line.numbers = TRUE){
print.population <- function(pop, gene.sep = " ", gene.brackets= "", line.numbers = TRUE){

#### Reproduction section

## choose mutation function for mutation.genes to be used in mutate.fgen to create mutate
mutation.genes.binary:		type = function
mutation.genes.boolean:		type = function
mutation.genes.complement:		type = function
	alphabet:	type = vector; e.g. = c('A','C','G','T')
mutation.genes.spin:		type = function
mutation.genes.symbolic:		type = function
	alphabet:	type = vector; e.g. = c('A','C','G','T')
mutation.genes.seq:		type = function
	...:			type = seq(...) parameters; e.g. from = 10, to = 20, by = 2
mutation.genes.integer:		type = function
	values:		type = integer vector; e.g. = 10:20
mutation.genes.creep:		type = function 
	max:			type = integer; e.g. = Inf 
	min:			type = integer; e.g. = -Inf
mutation.genes.uniform:		type = function 
	delta:		type = numeric; e.g. = 3.14159265
mutation.genes.gaussian:		type = function 
	sd:			type = numeric; e.g. = 5

returnValues.mutation <- function(mutation.count, mutation.locations, from.genes, to.genes){
print.returnValues.mutation <- function(returnValues.vector){

## choose crossover mask to be used in xover.fgen that creates cross
xover.mask.kpoint:		type = function 
	k: type = numeric; e.g. = 3; e.g. = 0.8; e.g. = 3.426
xover.mask.1point:		type = function
xover.mask.uniform:		type = function 
	alpha: type = double (between 0 and 1); e.g. = 0.5

# already calls xover.fgen 
onePt.xover:		type = function
twoPt.xover:		type = function

returnValues.xover <- function(swap.mask, xover.points = NULL){
print.returnValues.xover <- function(returnValues.vector, print.xpts=FALSE, mask.sep=" "){


## Choose selection fn (returns population locations) 
##      used in select.population.fgen that creates the select.chr function 

tournament.selection:				type = function
	tourn.size: 			type = integer; e.g. = 2
	prob.select.worse: 	type = double; e.g. = 0.3
	decreasing:				type = Boolean; e.g. = FALSE
	`%>%`:					type = function; e.g. `%my-predicate%`
simple.tournament.selection:	type = function
	tourn.size: 			type = integer; e.g. = 2
	decreasing:				type = Boolean; e.g. = FALSE
	`%>%`:					type = function; e.g. `%my-predicate%`
fitnessProportional.setup:		type = function
fitnessProportional.selection:	type = function
# not implemented yet   rank.selection <- function(n, pop = NULL, select=NULL){ 

#### Elitism

# if missing elite.size = 0

#note: elite.selection only works with integer or floating point fitness ordered by <= or >=

elite.selection:				type = function 
	elite.size: 	type = integer; e.g. = 3
	decreasing:		type = Boolean; e.g. = TRUE
# note: should be rewritten because eliminating members from a population 
#       is not the same as selecting members for the next population
truncation.selection:				type = function   
	elite.size: 	type = integer; e.g. = 3        # note: should change this so has a different name 
	decreasing:		type = Boolean; e.g. = TRUE

#### General parameters
max.gen

#### GA design choice
chr.length
pop.size
new.chromosome = function choice
mutate:	type = function choice
xover:	type = function choice
xover.prob
selection.setup:	type = function choice or NULL
selection: type = function choice
elite.size

## Fitness Parameters
fitness.fn
decode.fn
goal.fn
epsilon

##### Genetic Algorithm

# GA.parameters: fitness.fn, decode.fn, goal.fn, epsilon, max.gen, epsilon
# GA.parameters: select.elite, select.chr, mutate, xover, xover.prob
generational.GA <- function(GA.parameters){
	with(GA.parameters, {	 
		pop <- new.random.population()
		evalutate.population(pop, fitness.fn, decode.fn)
		report.stats(0, pop = pop)
		for(gen in 1:max.gen){
			if (goal.fn(pop, epsilon))
				break
			results <- next.generation(pop, select.elite, select.chr, mutate, xover, xover.prob)
			new.pop <- results[["pop"]]
			evalutate.population(new.pop, fitness.fn, decode.fn)
			report.stats(gen, results = results)
			pop <- new.pop
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

next.generation <- function(pop, select.elite, select.chr, mutate, cross, xover.prob){
	P <- size.population(pop)
	elite.loc <- select.elite(pop)
	
	elite.size <- length(elite.loc)
	xover.size <- xover.count(P, elite.count, xover.prob)
	mut.size <- mutate.only.count(P, xover.size, elite.size)
	
	p1.loc <- select.chr(xover.size, pop)
	p2.loc <- select.chr(xover.size, pop)
	rest.loc <- select.chr(mut.size, pop)
	
	elite <- duplicate.chromosomes(pop, elite.loc)
	p1 <- duplicate.chromosomes(pop, p1.loc)
	p2 <- duplicate.chromosomes(pop, p2.loc)
	rest <- duplicate.chromosomes(pop, rest.loc)
	
	m.results <- c(rest = mutate(rest), p1 = mutate(p1), p2 = mutate(p2))
	x.results <- cross(p1, p2)
	
	new.pop <- new.population(c(elite, p1, p2, rest))
	c(pop = new.pop, m.results, cross <- x.results)
}

### Fitness and Decode functions used in evaluate.chromosome and evaluate.population

straight.decode <- function(chr){
	genes.chromosome(chr)	
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

chromosome.length
chromosome.mode
population.size

select.elite, 
	elite.size
select.chr
	
mutate, 
	tournament
		tournament.size
		
xover, xover.prob

