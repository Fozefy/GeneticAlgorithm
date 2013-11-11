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
  new("population", GA.env = GA.env)  
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

setMethod("ga.print",
          signature = c("population"),
          definition = function(obj, ...){
            ga.print(chromosomes(obj), ...)
          }
)