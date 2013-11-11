duplicate <- function(obj, ...){
  print("Generic base function; does nothing. -- duplicate")
  obj    
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
          signature = c("list"),
          definition = function(obj, ...){
            for(i in 1:length(obj)){
              obj[[i]] <- duplicate(obj[[i]])
            }
            obj
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



setMethod("duplicate",
          signature = c("population"),
          definition = function(obj, ...){
            obj@fitness.cache <- duplicate(obj@fitness.cache)
            obj@chromosomes <- duplicate(obj@chromosomes)
            obj@chromosomes[["values"]] <- duplicate(obj@chromosomes[["values"]])
            obj
          }
)