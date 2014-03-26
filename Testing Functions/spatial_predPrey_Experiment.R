n=30
graph = gridConstructor(100)
generations.4graph.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  generations.4graph.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.4graph.predprey, file="graph4.predprey")

graph = complete.graph(100)
generations.complete.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  generations.complete.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.complete.predprey, file="complete.predprey")

graph = gridConstructor.withDiag(100) #8 connections
generations.8graph.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  generations.8graph.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.8graph.predprey, file="graph8.predprey")

graph = ring.graph(100) #ring graph
generations.ring4.predprey = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  generations.ring4.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.ring4.predprey, file="ring4.predprey")

graph = ring.graph.extra(100) #ring graph more connection
generations.ring8.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  generations.ring8.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.ring8.predprey, file="ring8.predprey")

generations.random4.predprey = c(1)
for (i in 1:n)
{
  graph =randomConstructor.NoDuplicate(4,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  generations.random4.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.random4.predprey, file="rand4.predprey")

generations.random8.predprey = c(1)
for (i in 1:n)
{
  graph =randomConstructor.NoDuplicate(8,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  generations.random8.predprey[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.random8.predprey, file="rand8.predprey")

graph = gridConstructor(100)
graph4.predprey.NoElite = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph4.predprey.NoElite[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(graph4.predprey.NoElite, file="graph4.predprey.NoElite")

graph = gridConstructor(100)
graph8.predprey.NoElite = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph8.predprey.NoElite[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(graph8.predprey.NoElite, file="graph8.predprey.NoElite")

graph = complete.graph(100)
complete.predprey.NoElite = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  complete.predprey.NoElite[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(complete.predprey.NoElite, file="complete.predprey.NoElite")

graph = gridConstructor(100)
graph4.predprey.InnerMatch = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.InnerMatch, goal=46, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph4.predprey.InnerMatch[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(graph4.predprey.InnerMatch, file="graph4.predprey.InnerMatch")

graph = gridConstructor(100)
graph8.predprey.InnerMatch = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.InnerMatch, goal=46, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph8.predprey.InnerMatch[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(graph8.predprey.InnerMatch, file="graph8.predprey.InnerMatch")

graph = complete.graph(100)
complete.predprey.InnerMatch = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.InnerMatch, goal=46, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  complete.predprey.InnerMatch[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(complete.predprey.InnerMatch, file="complete.predprey.InnerMatch")

#GridFitness
graph4.GridFitness.predprey = c(1)
graph = gridConstructor(100)
coevoGrid=gridConstructor(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.withGrid, goal=31, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph4.GridFitness.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(graph4.GridFitness.predprey, file="graph4.GridFitness.predprey")

graph8.GridFitness.predprey = c(1)
graph = gridConstructor.withDiag(100)
coevoGrid=gridConstructor.withDiag(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.withGrid, goal=31, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph8.GridFitness.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(graph8.GridFitness.predprey, file="graph8.GridFitness.predprey")

graph8.GridFitness4.predprey = c(1)
graph = gridConstructor.withDiag(100)
coevoGrid=gridConstructor(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.withGrid, goal=31, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph8.GridFitness4.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(graph8.GridFitness4.predprey, file="graph8.GridFitness4.predprey")

graph4.GridFitness8.predprey = c(1)
graph = gridConstructor(100)
coevoGrid=gridConstructor.withDiag(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.withGrid, goal=31, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  graph4.GridFitness8.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(graph4.GridFitness8.predprey, file="graph4.GridFitness8.predprey")

complete.GridFitness4.predprey = c(1)
graph = complete.graph(100)
coevoGrid=gridConstructor(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.withGrid, goal=31, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  complete.GridFitness4.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(complete.GridFitness4.predprey, file="complete.GridFitness4.predprey")

complete.GridFitness8.predprey = c(1)
graph = complete.graph(100)
coevoGrid=gridConstructor.withDiag(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.withGrid, goal=31, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  complete.GridFitness8.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(complete.GridFitness8.predprey, file="complete.GridFitness8.predprey")

complete.GridFitness.predprey = c(1)
graph = complete.graph(100)
coevoGrid=complete.graph(100)
coevoGrid=split(coevoGrid, rep(1:nrow(coevoGrid), each = ncol(coevoGrid)))
coevoGrid = matrix(c(coevoGrid, coevoGrid), nrow=100, ncol=2)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey.withGrid, goal=31, externalConnectionsMatrix=coevoGrid), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  complete.GridFitness.predprey[i] = ga$gen
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(complete.GridFitness.predprey, file="complete.GridFitness.predprey")