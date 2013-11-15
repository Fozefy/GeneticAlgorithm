#Decode Functions
straight.decode <- function(chr){
  chr
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
simpleGoal<- function(goal, epsilon)
{
  goalFunction <- function(popFit)
  {
    if (max(popFit) >= goal + epsilon) return(TRUE) else return(FALSE)    
  }
}

#Elitism
truncation.selection <- elite.selection