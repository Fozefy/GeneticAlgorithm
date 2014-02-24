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
            
            if (length(obj) > 0)
            {
              for(i in 1:length(obj)){              
                obj[[i]] <- duplicate(obj[[i]])
              }
              return(obj)
            }
            else
            {
              #Return an empty list
              list()
            }
          }
)

setMethod("duplicate",
          signature = c("organism"),
          definition = function(obj, ...){
            obj@chromosome <- duplicate(obj@chromosome)
            obj@fitness <- duplicate(obj@fitness)
            obj@index <- duplicate(obj@index)
            obj
          }
)

setMethod("duplicate",
          signature = c("population"),
          definition = function(obj, ...){
            obj@organisms <- duplicate(obj@organisms)
            obj@organisms[["values"]] <- duplicate(obj@organisms[["values"]])
            obj
          }
)
