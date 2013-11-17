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
            fitness(chromosomes(obj))
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

evaluate <- function(obj, fitness.fn, decode.fn, ...){
  print("generic base function does nothing - evaluate")
}

setGeneric("evaluate")

setMethod("evaluate",
          signature = c("chromosome", "function"),
          definition = function(obj, fitness.fn, decode.fn = identity, ...) {  
            obj@fitness$value <- fitness.fn(decode.fn(obj@chr.genes$genes))
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
            decode.fn
            fit.vector <- evaluate(chromosomes(obj), fitness.fn, decode.fn, ...)
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
