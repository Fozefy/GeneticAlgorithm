graph4.GridFitness.coevo = c(1)
graph = gridConstructor(100)
coevoGrid=gridConstructor(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.onGrid, goal=60, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph4.GridFitness.coevo[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(graph4.GridFitness.coevo, file="graph4.GridFitness.coevo")

graph4.GridFitness.best.coevo = c(1)
graph = gridConstructor(100)
coevoGrid=gridConstructor(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.bestOnGrid, goal=60, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph4.GridFitness.best.coevo[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(graph4.GridFitness.best.coevo, file="graph4.GridFitness.best.coevo")