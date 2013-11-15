#### Population Class

setClass("population", 
         representation(chromosomes = "environment",
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

new.population <- function(GA.env = NULL, chromosomes = NULL){
  new("population", GA.env = GA.env, chromosomes = chromosomes)  
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

#Set the GA's population
add.population <- function(reproduction.env, popn){
  reproduction.env$pop <- popn
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

#Returns a list of the population's fitnesses
getFitnesses <- function(pop){
  print("generic base function does nothing - getFitnesses")
}
setGeneric("getFitnesses")
setMethod("getFitnesses",
          signature = "population",
          definition = function(pop){
            fitnesses = NULL
            for(i in 1:size(pop))
            {
              fitnesses = c(fitnesses, pop@chromosomes$values[[i]]@fitness$value)
            }
            fitnesses
          }
)
