### Organism Class

setClass("organism", representation(chromosome = "environment", fitness = "environment"))
setMethod("initialize", 
          signature = "organism",
          definition = function(.Object, chr.length, 
                                rdist = function(n) sample(c(0,1), n, replace=TRUE),
                                fitness.fn = function(chr) {numeric(0)}, ...) {
            genes.env <- new.env()
            if (is.null(rdist))
            {
              rdist = function(n) sample(c(0,1), n, replace=TRUE)
            }
            chr.length
            genes.env$genes <- rdist(chr.length)
            .Object@chromosome <- genes.env
            
            fitness.env <- new.env()
            fitness.env$value <- fitness.fn(.Object)
            .Object@fitness <- fitness.env
            
            .Object
          }
)

new.organism <- function(GA.env){
  new("organism", encoding.env(GA.env)$chr.length, rdist = encoding.env(GA.env)$new.genes.fn)
}

organism <- function(object){object@chromosome[["genes"]]}
setGeneric("organism")
genes <- function(object){object@chromosome[["genes"]]}
setGeneric("genes")

setMethod("[", 
          signature = c("organism"),
          definition = function(x,i,j,...,drop){
            organism(x)[i]
          }
)

setMethod("[[", 
          signature = c("organism"),
          definition = function(x,i,j,...,drop){
            organism(x)[[i]]
          }
)

setMethod("[[<-", 
          signature = c("organism", "ANY", "ANY"),
          definition = function(x, i, value){
            x@chromosome[["genes"]][[i]] <- value
            x
          }
)

setMethod("[<-", 
          signature = c("organism", "ANY", "ANY"),
          definition = function(x, i, value){
            x@chromosome[["genes"]][i] <- value
            x
          }
)

setMethod("length", 
          signature = c("organism"),
          definition = function(x){
            length(organism(x))
          }
)

setGeneric("mode")
setMethod("mode", 
          signature = c("organism"),
          definition = function(x){
            mode(organism(x))
          }
)

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
