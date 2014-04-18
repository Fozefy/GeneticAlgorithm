graph = gridConstructor(100) #4 connections
coevoSpt.rosenbrock = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=Rosenbrock.twoPop.fitness.fn, goal=0, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph,maximizing = FALSE),encoding.args=new.encoding.args(chr.length=16), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.rosenbrock[i] = ga$gen
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(coevoSpt.rosenbrock,file="coevoSpt.rosenbrock")

graph = complete.graph(100) #complete connections
coevo.rosenbrock = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=Rosenbrock.twoPop.fitness.fn, goal=0, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph,maximizing = FALSE), encoding.args=new.encoding.args(chr.length=16),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.rosenbrock[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(coevo.rosenbrock,file="coevo.rosenbrock")

graph = complete.graph(100)
std.rosenbrock = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=Rosenbrock.fitness.fn,goal=0), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph,maximizing = FALSE), encoding.args=new.encoding.args(chr.length=32),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.rosenbrock[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(std.rosenbrock,file="std.rosenbrock")

graph = gridConstructor(100)
stdSpatial.rosenbrock = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=Rosenbrock.fitness.fn,goal=0), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph,maximizing = FALSE), encoding.args=new.encoding.args(chr.length=32),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.rosenbrock[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(stdSpatial.rosenbrock,file="stdSpatial.rosenbrock")