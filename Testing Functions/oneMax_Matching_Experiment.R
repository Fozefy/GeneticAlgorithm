graph = gridConstructor(100) #4 connections
coevoSpt.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(), goal=45, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.matching[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevoSpt.matching,file="coevoSpt.matching")

graph = complete.graph(100) #complete connections
coevo.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(), goal=45, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.matching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.matching,file="coevo.matching")

graph = complete.graph(100)
std.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(),goal=45), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.matching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.matching,file="std.matching")

graph = gridConstructor(100)
stdSpatial.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(),goal=45), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.matching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.matching,file="stdSpatial.matching")

#More matching

graph = gridConstructor(100) #4 connections
coevoSpt.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(matching=4), goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.extraMatching[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevoSpt.extraMatching,file="coevoSpt.extraMatching")

graph = complete.graph(100) #complete connections
coevo.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(matching=4), goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.extraMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.extraMatching,file="coevo.extraMatching")

graph = complete.graph(100)
std.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(matching=4),goal=90), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.extraMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.extraMatching,file="std.extraMatching")

graph = gridConstructor(100)
stdSpatial.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(matching=4),goal=90), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.extraMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.extraMatching,file="stdSpatial.extraMatching")