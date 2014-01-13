coordinationProblem <- function(organism, popNum, otherPops, externalConnectionsMatrix){
  otherNodes = externalConnectionsMatrix[organism@index, popNum]  
  otherGenes = otherPops[[1]]@organisms$values[[otherNodes]]@chromosome$genes
  
  coordinated = sum(otherGenes == organism@chromosome$genes)

  coordinated/otherNodes #Return a normalized coordination value
}

coordinationGoalFunction <- function(popFit)
{
  return(median(popFit[[1]]) > .9)
}

#TODO - Doesn't make sense to use this as it requires a single bit which makes crossover pointless
#Also, need to pass a differnet external connections matrix to connect with surrounding nodes
n=20

graph = gridConstructor(100) #4 connections
results = vector("list",n)
generations = c(1)
maxFit = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=coordinationProblem, goal.fn=coordinationGoalFunction, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results[[i]]=ga$reported.data
  generations[i] = ga$gen
  maxFit[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

graph = gridConstructor.withDiag(100) #9 connections

results2 = vector("list",n)
generations2 = c(1)
maxFit2 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=coordinationProblem, goal.fn=coordinationGoalFunction, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results2[[i]]=ga$reported.data
  generations2[i] = ga$gen
  maxFit2[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

graph = complete.graph(100) #complete connections
results3 = vector("list",n)
generations3 = c(1)
maxFit3 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=coordinationProblem, goal.fn=coordinationGoalFunction, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results3[[i]]=ga$reported.data
  generations3[i] = ga$gen
  maxFit3[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}
