#### Reproduction section

## mutation
alleles.fgen <- function(alleles.fn, ...){
  function(genes) alleles.fn(genes, ...)
}

new.alleles <- function(rep.env){rep.env$alleles.fn}

alleles.binary <- function(genes){
  1 - genes  
}

alleles.boolean <- function(genes){
  !genes	
}

alleles.complement <- function(genes, alphabet){
  alleles <- vector(mode(genes),length(genes))
  for(i in 1:length(genes)){
    alleles[i] <- alphabet[(alphabet != genes[i])]
  }
  alleles	
}

alleles.spin <- function(genes){
  alleles.complement(genes, c(-1,1))
}

alleles.symbolic <- function(genes, alphabet){
  alleles <- vector(mode(genes),length(genes))
  for(i in 1:length(genes)){
    alleles[[i]] <- sample(alphabet[(alphabet != genes[[i]])],1)
  }
  alleles
}

# alleles.seq <- function(genes, ...){
#	alleles.symbolic(genes, seq(...))
# }

alleles.integer <- function(genes, values){
  alleles.symbolic(genes, values)
}

alleles.creep <- function(genes, max = Inf, min = -Inf){
  new.genes <- genes + sample(c(-1,1),length(genes), replace=TRUE)
  new.genes <- pmin(new.genes, max)
  new.genes <- pmax(new.genes, min)
  new.genes
}

alleles.uniform <- function(genes, delta=1){
  genes + runif(length(genes), -delta/2, delta/2)
}

alleles.gaussian <- function(genes, sd=1){
  genes + rnorm(count,0,sd)
}

#Determine the number of chromosomes to be determined from only mutation
mutate.only.count <- function(popSize, xover, elite){
  popSize - 2 * xover - elite
}

prob.mutation <- function(rep.env){rep.env$prob.mutation}

chr.mutate <- function(chr, repr.env, mutation.locations = NULL, mutations = NULL){
  print("generic base function does nothing -- chr.mutate")
}

setGeneric("chr.mutate")

setMethod("chr.mutate",
          signature = c("chromosome", "environment", "ANY", "ANY"),
          definition = function(chr, repr.env, mutation.locations = NULL, mutations = NULL){
            chr.length <- length(chr)
            `%!=%` <- repr.env$not.equal.op
            if(is.null(mutation.locations))
              mutation.locations <- (runif(chr.length) < prob.mutation(repr.env))
            genes <- chr[mutation.locations]
            if(length(genes) > 0){
              if(is.null(mutations))
                alleles <- new.alleles(repr.env)(genes)
              chr[mutation.locations] <- alleles
            } else {
              alleles = integer(0)
            }
            returnValues.mutation(sum(alleles %!=% genes), (1:chr.length)[mutation.locations], genes, alleles)
          }
)

setMethod("chr.mutate",
          signature = c("list", "environment", "ANY", "ANY"),
          definition = function(chr, repr.env, mutation.locations = NULL, mutations = NULL){
            n <- length(chr)
            if (n > 0)
            {
              returnValues <- vector("list", n)
              for(i in 1:n)
                returnValues[[i]] <- chr.mutate(chr[[i]], repr.env, mutation.locations[[i]], mutations[[i]])
              return(new("returnList", returnValues))
            }
            else
            {
              return(list())
            }
          }
)

mutate.fgen <- function(repr.env){
  function(chr){chr.mutate(chr, repr.env)}
}

setClass("returnValues.mutation",
         representation(mutation.count = "numeric", 
                        mutation.locations = "numeric", 
                        from.genes = "ANY", 
                        to.genes = "ANY"))

returnValues.mutation <- function(mutation.count, mutation.locations, from.genes, to.genes){
  new("returnValues.mutation", 
      mutation.count = mutation.count, 
      mutation.locations = mutation.locations, 
      from.genes = from.genes, 
      to.genes = to.genes)
}

mutation.count <- function(obj){obj@mutation.count} 
mutation.locations <- function(obj){obj@mutation.locations} 
from.genes <- function(obj){obj@from.genes} 
to.genes <- function(obj){obj@to.genes} 

setClass("returnList",
         representation(returnList = "environment"))

setMethod("initialize", 
          signature = "returnList",
          definition = function(.Object, ...) {
            returnList.env <- new.env()
            returnList.env$values <- list()
            .Object@returnList <- returnList.env
            .Object
          }
)

setMethod("initialize", 
          signature = "returnList",
          definition = function(.Object, return.value, ...) {
            returnList.env <- new.env()
            if(is.list(return.value))
              returnList.env$values <- return.value
            else
              returnList.env$values <- list(return.value)
            .Object@returnList <- returnList.env
            .Object
          }
)


returnList <- function(x){x@returnList[["values"]]}

setMethod("[", 
          signature = c("returnList"),
          definition = function(x,i,j,...,drop){
            returnList(x)[i]
          }
)

setMethod("[[", 
          signature = c("returnList"),
          definition = function(x,i,j,...,drop){
            returnList(x)[[i]]
          }
)

