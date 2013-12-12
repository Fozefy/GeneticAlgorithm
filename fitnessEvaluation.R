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

evaluate <- function(obj, fitness.fn, ...){
  print("generic base function does nothing - evaluate")
}

setGeneric("evaluate")

setMethod("evaluate", 
          signature = c("list", "function"),
          definition = function(obj, fitness.fn, otherPop = NULL) {
            n <- length(obj)
            
            #TODO - Use an id to help fitness.fn deal with other Pop, maybe adj matrix + id?
            
            if (is.null(otherPop))
            {
              fit1 <- obj[[1]]@fitness$value <- fitness.fn(obj[[1]])
            }
            else
            {
              #TODO - remove the 3rd arguement and store it in the object somehow (see todo above)
              fit1 <- obj[[1]]@fitness$value <- fitness.fn(obj[[1]], otherPop, 1)
            }
            
            if(is.multiobjective(obj[[1]]))
              fit.cache <- vector("list", n)
            else
              fit.cache <- vector("numeric", n)

            fit.cache[[1]] <- fit1
            
            for(i in 2:n)
            {
              if (is.null(otherPop))
              {
                fit.cache[[i]] <- obj[[i]]@fitness$value <- fitness.fn(obj[[i]])
              }
              else
              {
                fit.cache[[i]] <- obj[[i]]@fitness$value <- fitness.fn(obj[[i]], otherPop, i)
              }
            }
            
            fit.cache
          }
)

setMethod("evaluate", 
          signature = c("population", "function"),
          definition = function(obj, fitness.fn, ...) { 
            fit.vector <- evaluate(organisms(obj), fitness.fn, ...)
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
