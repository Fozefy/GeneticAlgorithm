#Testing Spatial Effects
n=50

one.max.WithLinkage.twoPop <- function(organism, popNum, otherPops, externalConnectionsMatrix)
{
  otherGenes = otherPops[[1]]@organisms$values[[externalConnectionsMatrix[organism@index$value, popNum]]]@chromosome$genes
  sum(organism@chromosome$genes) + sum(otherGenes) + sum(organism@chromosome$genes == otherGenes)
}


graph = gridConstructor(100) #4 connections
generations.4graph = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=one.max.WithLinkage.twoPop, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.4graph[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = gridConstructor.withDiag(100) #8 connections
generations.8graph = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=one.max.WithLinkage.twoPop, goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.8graph[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = complete.graph(100) #complete connections
generations.complete = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(one.max.WithLinkage.twoPop, goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.complete[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}

graph = complete.graph(100) #complete connections
generations.complete = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(one.max.WithLinkage.twoPop, goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.complete[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}

graph = gridConstructor(100)
stdSpatial.elite.two = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn = reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.two,file="stdSpatial.elite.two")
