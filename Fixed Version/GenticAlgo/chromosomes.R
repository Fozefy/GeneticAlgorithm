### population definition and creation section

### Fitness Class (needed for multi-objective, stochastic and other complex fitnesses)

setClass("fitness", representation(value = "numeric"))
setMethod("initialize", 
          signature = "fitness",
          definition = function(.Object, chr = NULL, fitness.fn = function(chr) NULL, ...) {
            .Object@value <- fitness.fn(chr)
            .Object
          })

### Chromosome Class

setClass("chromosome", representation(chr.genes = "environment", fitness = "environment"))
setMethod("initialize", 
          signature = "chromosome",
          definition = function(.Object, chr.length, 
                                rdist = function(n) sample(c(0,1), n, replace=TRUE),
                                fitness.fn = function(chr) {numeric(0)},
                                decode.fn = identity,
                                ...) {
            genes.env <- new.env()
            if (is.null(rdist))
            {
              rdist = function(n) sample(c(0,1), n, replace=TRUE)
            }
            chr.length
            genes.env$genes <- rdist(chr.length)
            .Object@chr.genes <- genes.env
            
            fitness.env <- new.env()
            fitness.env$value <- fitness.fn(decode.fn(.Object))
            .Object@fitness <- fitness.env
            
            .Object
          }
)

new.chromosome <- function(GA.env){
  new("chromosome", encoding.env(GA.env)$chr.length, rdist = encoding.env(GA.env)$new.genes.fn)
}

chromosome <- function(object){object@chr.genes[["genes"]]}
setGeneric("chromosome")
genes <- function(object){object@chr.genes[["genes"]]}
setGeneric("genes")

setMethod("[", 
          signature = c("chromosome"),
          definition = function(x,i,j,...,drop){
            chromosome(x)[i]
          }
)

setMethod("[[", 
          signature = c("chromosome"),
          definition = function(x,i,j,...,drop){
            chromosome(x)[[i]]
          }
)

setMethod("[[<-", 
          signature = c("chromosome", "ANY", "ANY"),
          definition = function(x, i, value){
            x@chr.genes[["genes"]][[i]] <- value
            x
          }
)

setMethod("[<-", 
          signature = c("chromosome", "ANY", "ANY"),
          definition = function(x, i, value){
            x@chr.genes[["genes"]][i] <- value
            x
          }
)


setMethod("length", 
          signature = c("chromosome"),
          definition = function(x){
            length(chromosome(x))
          }
)

setGeneric("mode")
setMethod("mode", 
          signature = c("chromosome"),
          definition = function(x){
            mode(chromosome(x))
          }
)

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

`fitness<-` <- function(chr, value){
  chr@fitness$value <- value
  chr@fitness
}

setGeneric("fitness<-")

new.genes.fgen <- function(new.chromosome.fn, ...){
  function(chr.length) new.chromosome.fn(chr.length, ...)
}

new.genes.symbolic <- function(chr.length = 1, alphabet=c('A','C','G','T')){
  sample(alphabet,chr.length, replace=TRUE)
}

new.genes.binary <- function(chr.length = 1){
  new.genes.symbolic(chr.length, c(0,1))
}

new.genes.spin <- function(chr.length = 1){
  new.genes.symbolic(chr.length, c(-1,1))
}

new.genes.seq <- function(chr.length = 1, seq.args){
  new.genes.symbolic(chr.length, alphabet = do.call(seq, seq.args))
}

new.genes.integer <- new.genes.seq

new.genes.dist <- function(chr.length = NULL, rdist=rnorm, rdist.args = list()){
  if(!is.null(chr.length))
    rdist.args <- c(chr.length, rdist.args)
  do.call(rdist, rdist.args)
}

new.genes.real <- new.genes.dist
