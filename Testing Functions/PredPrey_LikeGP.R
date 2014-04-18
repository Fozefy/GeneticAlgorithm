graph = gridConstructor(100)
graph4.GP.predprey = c(1)
coevoGrid=gridConstructor.withDiag(100)
coevoGrid <- cbind(coevoGrid,1:100)
coevoGrid = split(coevoGrid,row(coevoGrid))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.AvgGrid, goal=100, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=16), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph4.GP.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(graph4.GP.predprey, file="graph4.GP.predprey")

graph = complete.graph(100)
complete.GP.predprey = c(1)
coevoGrid=gridConstructor.withDiag(100)
coevoGrid <- cbind(coevoGrid,1:100)
coevoGrid = split(coevoGrid,row(coevoGrid))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.AvgGrid, goal=100, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=16),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  complete.GP.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(complete.GP.predprey, file="complete.GP.predprey")

graph = gridConstructor.withDiag(100) #8 connections
graph8.GP.predprey = c(1)
coevoGrid=gridConstructor.withDiag(100)
coevoGrid <- cbind(coevoGrid,1:100)
coevoGrid = split(coevoGrid,row(coevoGrid))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.AvgGrid, goal=100, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=16), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  graph8.GP.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(graph8.GP.predprey, file="graph8.GP.predprey")