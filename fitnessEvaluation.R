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

setMethod("fitness", 
          signature = c("population"),
          definition = function(obj){
            fitness(organisms(obj))
          }
)

`fitness<-` <- function(chr, value){
  chr@fitness$value <- value
  chr@fitness
}

setGeneric("fitness<-")

new.fitness.fn <- function(fitness.fn, ...){
  function(genes) fitness.fn(genes, ...)
}

## Fitness Evaluation

evaluate <- function(pop, fitness.fn, ...){
  print("generic base function does nothing - evaluate")
}

setGeneric("evaluate")

setMethod("evaluate", 
          signature = c("population", "function"),
          definition = function(pop, fitness.fn, otherPops = NULL, externalConnectionsMatrix = NULL) {
            organisms = pop@organisms$values
            
            n <- length(organisms)
            
            fit1 <- organisms[[1]]@fitness$value <- fitness.fn(organisms[[1]], pop, otherPops, externalConnectionsMatrix)

            if(is.multiobjective(organisms[[1]]))
              fit.cache <- vector("list", n)
            else
              fit.cache <- vector("numeric", n)

            fit.cache[[1]] <- fit1
            
            for(i in 2:n)
            {
              
              fit.cache[[i]] <- organisms[[i]]@fitness$value <- fitness.fn(organisms[[i]], pop, otherPops, externalConnectionsMatrix)
            }
            
            fit.cache
          }
)


### Fitness Class (needed for multi-objective, stochastic and other complex fitnesses)
setClass("fitness", representation(value = "numeric"))
setMethod("initialize", 
          signature = "fitness",
          definition = function(.Object, chr = NULL, fitness.fn = function(chr) NULL, ...) {
            .Object@value <- fitness.fn(chr)
            .Object
          })
