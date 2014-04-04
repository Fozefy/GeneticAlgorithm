onePop.one.max.withMultiSubPatterns <- function(geneLength,submatch=c(0.35, 0.25), probOne = c(0.4,0.6)){
  #geneLength should be a GA parameter that one can find from the GA setup parameters
  # number of locals equals number of (distinct) patterns,
  #    i.e may equal patternCount, but may be smaller if some patterns (by random chance) match each other
  
  patternCount <- length(probOne)
  pattern <- vector(mode = "list", length = patternCount)
  for(i in 1:patternCount)
    pattern[[i]] <- runif(geneLength) < probOne[[i]]
  
  onePop.one.max.subPatterns <- function(organism,...){
    geneLength = length(organism@chromosome$genes)
    genes = organism@chromosome$genes[1:(geneLength/2)]
    otherGenes = organism@chromosome$genes[(geneLength/2 + 1):geneLength]
    onematch <- 1 - sum(submatch)
    fitness <- sum(genes)*onematch
    for(i in 1:patternCount)
      fitness = fitness + sum(genes==pattern[[i]])*submatch[[i]]
    return(fitness)
  }
}

onePop.one.max.withSubPattern <- function(submatch=0.4, geneLength, probOne = 0.5){
  # geneLength should be a GA parameter that one can find from the GA setup parameters
  # one local
  pattern <- runif(geneLength) < probOne
  
  onePop.one.max.subPattern <- function(organism,...){
    geneLength = length(organism@chromosome$genes)
    genes = organism@chromosome$genes[1:(geneLength/2)]
    otherGenes = organism@chromosome$genes[(geneLength/2 + 1):geneLength]
    onematch <- 1 - submatch
    fitness = sum(genes)*onematch + sum(genes == pattern)*submatch
    return(fitness)
  }
  
  return(c(onePop.one.max.subPattern,sum(pattern)*submatch + geneLength*(1 - submatch)))
}