#Determine the number of chromosomes to be crossed
xover.count <- function(P, xover.prob, xover=NULL){
  xover.count <- P %/% 2
  if(is.null(xover))
    xover <- (runif(xover.count) < xover.prob)
  total = sum(xover)
  
  if (total < 1) return(1)
  
  return(total)
}

chr.xover <- function(chr1, chr2, swapMask.fn = xover.mask.2point, swapMask=NULL, ...){
  chr1; chr2; swapMask.fn; swapMask
  print("generic base function does nothing - chr.xover")
}

setGeneric("chr.xover")

setMethod("chr.xover",
          signature = c("organism", "organism", "function", "ANY"),
          definition = function(chr1, chr2, swapMask.fn = xover.mask.2point, swapMask=NULL){
            if(is.null(swapMask))
              swapMask <- swapMask.fn(length(chr1))
            swap.genes(chr1, chr2, swapMask)
            returnValues.xover(swapMask)
          }
)

setMethod("chr.xover",
          signature = c("list", "list", "function", "ANY"),
          definition = function(chr1, chr2, swapMask.fn, swapMask=NULL, xover.list = NULL){
            
            n <- length(chr1)

            if (!is.null(xover.list))
            {
              chr2[-xover.list] = 0              
            }

            return.values <- vector("list", n)
            for(i in 1:n)
            {
              if (class(chr2[[i]]) != "numeric")
              {
                  return.values[[i]] <- chr.xover(chr1[[i]], chr2[[i]], swapMask.fn, swapMask[[i]])
                  chr2[[i]]@index$value = i
              }

              chr1[[i]]@index$value = i              
            }
            
            new("returnList", return.values)
          }
)

xover.fgen <- function(xover.swapMask){
  function(chr1, chr2){chr.xover(chr1, chr2, xover.swapMask)}
}

xover.swapMask.fgen <- function(create.swapMask, ...){
  function(chr.length){
    create.swapMask(chr.length, ...)
  }
}

xover.mask.kpoint <- function(chr.length, k, verbose=FALSE){
  xPts <- sort(sample(0:(chr.length-1), k, replace=TRUE))
  if(verbose)
    print(xPts)
  swapMask <- vector("logical", chr.length) # default value is FALSE
  from <- 1; swap <- FALSE
  for(to in xPts){
    if(from <= to)
      swapMask[from:to] <- swap
    swap <- !swap
    from <- to + 1
  }
  
  if(from <= chr.length){
    swapMask[from:chr.length] <- swap
  }
  
  swapMask
} 

#Should these be in 'defaultGAProblems'?
xover.mask.1point <- function(chr.length, verbose=FALSE){
  xoverPoint <-sample(1:chr.length, 1)
  if(verbose)
    print(xoverPoint)
  swapMask <- vector("logical", chr.length) # default value is FALSE
  swapMask[xoverPoint:chr.length] <- TRUE
  swapMask
}

xover.mask.2point <- xover.swapMask.fgen(xover.mask.kpoint, 2)

xover.mask.uniform <- function(chr.length, alpha=0.5){
  runif(chr.length) < alpha
}

returnValues.xover <- function(swap.mask, xover.points = NULL){
  swap.mask; xover.points
  environment()  
}

setClass("returnValues.xover",
         representation(swap.mask = "logical"))

returnValues.xover <- function(swap.mask){
  new("returnValues.xover", swap.mask = swap.mask)
}

swap.mask <- function(rv.xover){rv.xover@swap.mask}

swap.genes <- function(chr1, chr2, mask){
  chr1; chr2; mask  
  print("generic base function does nothing - swap.genes")
}

setGeneric("swap.genes")

setMethod("swap.genes",
          signature = c("organism", "organism", "logical"),
          definition = function(chr1, chr2, mask){
            temp <- duplicate(chr1)
            temp[mask] <- chr1[mask]
            chr1[mask] <- chr2[mask]
            chr2[mask] <- temp[mask]
          }
)

swapMask2xpts <- function(swap.mask){
  mask.length <- length(swap.mask)
  swap.mask.shift <- c(F, swap.mask)[1:mask.length]
  (1:mask.length)[(swap.mask != swap.mask.shift)] - 1
}

logic.print.vector <- function(logic.vector, false.char="F", true.char="T"){
  vector.length <- length(logic.vector)
  ifelse(logic.vector,rep(true.char, vector.length), rep(false.char, vector.length))
}
