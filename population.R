#### Population Class

setClass("population", 
         representation(organisms = "environment",
                        pop.size = "numeric"))

setMethod("initialize", 
          signature = "population",
          definition = function(.Object, GA.env = NULL, organisms = NULL, ...) {
            chr.env <- new.env()
            if(is.null(organisms)){
              if(!is.null(GA.env))
                organisms <- create.random.organisms(GA.env)
              else
                simple.error("Either GA.env or chromosomes must be defined. Both are NULL.")
            }
            chr.env$values <- organisms
            .Object@organisms <- chr.env
            
            .Object@pop.size <- length(organisms)
            
            .Object
          }
)

create.random.organisms <- function(GA.env){
  P <- GA.env$pop.size
  organisms <- vector("list", P)
  for(i in 1:P){
    organisms[[i]] <- new.organism(GA.env)
  }
  organisms
}

new.population <- function(GA.env = NULL, organisms = NULL){
  new("population", GA.env = GA.env, organisms = organisms)  
}

organisms <- function(object){object$values}
setGeneric("organisms")

setMethod("organisms", 
          signature = c("population"),
          definition = function(object){
            organisms(object@organisms)
          }
)

setMethod("[", 
          signature = c("population"),
          definition = function(x,i,j,...,drop){
            organisms(x)[i]
          }
)

setMethod("[[", 
          signature = c("population"),
          definition = function(x,i,j,...,drop){
            organisms(x)[[i]]
          }
)

setMethod("[[<-", 
          signature = c("population", "ANY", "ANY"),
          definition = function(x, i, value){
            x@organisms[["values"]][[i]] <- value
            x
          }
)

setMethod("[<-", 
          signature = c("population", "ANY", "ANY"),
          definition = function(x, i, value){
            x@organisms[["values"]][i] <- value
            x
          }
)

#Set the GA's population
add.population <- function(reproduction.env, popn){
  reproduction.env$pop <- popn
}

size <- function(pop){
  length(organisms(pop))
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
              fitnesses = c(fitnesses, pop@organisms$values[[i]]@fitness$value)
            }
            fitnesses
          }
)