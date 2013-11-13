#Decode Functions
straight.decode <- function(chr){
  genes(chr)  
}

one.max.decode <- straight.decode

#Default fitness function
one.max.fn <- function(genes){
  sum(genes)  
}

finite.min.fn <- function(genes, gene.max){
  length(genes) * gene.max - sum(genes)
}

#Goal functions
#TODO - Make a better default function, this one doesn't really work well with current setup (chr.length hardcoded in setup.R)
simpleGoal<- function(goal, epsilon)
{
  goalFunction <- function(popFit)
  {
    if (max(popFit) >= goal + epsilon) return(TRUE) else return(FALSE)    
  }
}