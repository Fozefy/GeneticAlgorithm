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
            fitness.fn$fitness.cache(obj) <- fit.vector
          }
)

setMethod("evaluate", 
          signature = c("population", "environment"),
          definition = function(obj, fitness.fn, ...) {
            add.population(fitness.fn, obj)
            fit.vector <- evaluate(chromosomes(fitness.fn$pop), fitness.fn, fitness.fn$decode.fn)
            fitness.fn$fitness.cache<- fit.vector
          }
)
