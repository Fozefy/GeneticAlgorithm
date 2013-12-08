############################Printing Functions
addLeadingZeros <- function(value, max = 1){
  max.digits <- ceiling(log(max+1, 10))
  formatC(value,width = max.digits, flag="0")
}

addLeadingBlanks <- function(max = 0){
  if(max == 0)
    ""
  else{
    max.digits <- ceiling(log(max+1, 10)) + 3
    formatC(" ",width = max.digits, flag=" ")
  }
}

get.open.delimiter <- function(gene.brackets){
  if(is.null(gene.brackets))
    ""
  else if(length (gene.brackets) > 1)
    gene.brackets[[1]] 
  else if(nchar(gene.brackets) == 0)
    ""
  else if(nchar(gene.brackets) == 1)
    gene.brackets
  else
    substr(gene.brackets, 1, 1)
}

get.close.delimiter <- function(gene.brackets){
  if(is.null(gene.brackets))
    ""
  else if(length (gene.brackets) > 1)
    gene.brackets[[2]] 
  else if(nchar(gene.brackets) == 0)
    ""
  else if(nchar(gene.brackets) == 1)
    gene.brackets
  else
    substr(gene.brackets, 2, 2)
}

ga.print <- function(obj, ...) cat(obj, ...)
setGeneric("ga.print")

setMethod("ga.print",
          signature = c("organism"),
          definition = function(obj, max = 0, gene.sep = " ", genes.delimiters = NULL, 
                                fitness.sep = "", fitness.delimiters = NULL, ...){
            genes.delimiter.open <- get.open.delimiter(genes.delimiters)
            genes.delimiter.close <- get.close.delimiter(genes.delimiters)
            fitness.delimiter.open <- get.open.delimiter(fitness.delimiters)
            fitness.delimiter.close <- get.close.delimiter(fitness.delimiters)
            
            cat("genes =", genes.delimiter.open)
            cat(genes(obj), sep=gene.sep)
            cat(genes.delimiter.close)
            
            if(!unevaluated(obj)){
              cat("; fit =", fitness.delimiter.open)
              cat(fitness(obj), sep = fitness.sep)
              cat(fitness.delimiter.close)
            }
          }
)

setMethod("ga.print",
          signature = c("list"),
          definition = function(obj, line.numbers = TRUE, ...) {
            n <- length(obj)
            for(i in 1:n){
              if(line.numbers)
                cat("[", addLeadingZeros(i, n), "] ", sep = "") 
              ga.print(obj[[i]], max = n, ...)
              cat("\n")  
            }	
          }
)

setMethod("ga.print",
          signature = c("returnValues.mutation"),
          definition = function(obj, max = 0, ...){
            cat("mcount =", mutation.count(obj), "\n")
            cat(addLeadingBlanks(max))
            cat("loc    =", mutation.locations(obj), "\n") 
            cat(addLeadingBlanks(max))
            cat("from   =", from.genes(obj), "\n")
            cat(addLeadingBlanks(max))
            cat("to     =", to.genes(obj), "\n")
          }
)

setMethod("ga.print",
          signature = c("returnList"),
          definition = function(obj, ...){
            ga.print(returnList(obj), ...)
          }
)

setMethod("ga.print",
          signature = c("returnValues.xover"),
          definition = function(obj, max = 0, print.xpts=FALSE, mask.sep=" ", ...){
            sm <- swap.mask(obj)
            cat("Xover Information\n")
            cat(addLeadingBlanks(max))
            cat("   swap mask = ") 
            cat(logic.print.vector(sm), "\n", sep = mask.sep)
            if(print.xpts)
              cat("   xover pts =", swapMask2xpts(sm), "\n")
          }
)

setMethod("ga.print",
          signature = c("population"),
          definition = function(obj, ...){
            ga.print(chromosomes(obj), ...)
          }
)
