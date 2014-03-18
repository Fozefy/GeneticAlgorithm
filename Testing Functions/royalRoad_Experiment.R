graph = gridConstructor(100) #4 connections
coevoSpt.RoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.PureRoyalRoad, goal=32, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=16), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.RoyalRoad[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevoSpt.RoyalRoad,file="coevoSpt.RoyalRoad")

graph = complete.graph(100) #complete connections
coevo.RoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.PureRoyalRoad, goal=32, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=16),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.RoyalRoad[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.RoyalRoad,file="coevo.RoyalRoad")

graph = complete.graph(100)
std.RoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.PureRoyalRoad,goal=32), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=32),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.RoyalRoad[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.RoyalRoad,file="std.RoyalRoad")

graph = gridConstructor(100)
stdSpatial.RoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.PureRoyalRoad,goal=32), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=32),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.RoyalRoad[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.RoyalRoad,file="stdSpatial.RoyalRoad")

#Bonus Royal Road
graph = gridConstructor(100) #4 connections
coevoSpt.BonusRoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.BonusRoyalRoad, goal=64, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=16), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.BonusRoyalRoad[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevoSpt.BonusRoyalRoad,file="coevoSpt.BonusRoyalRoad")

graph = complete.graph(100) #complete connections
coevo.BonusRoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.BonusRoyalRoad, goal=64, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=16),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.BonusRoyalRoad[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.BonusRoyalRoad,file="coevo.BonusRoyalRoad")

graph = complete.graph(100)
std.BonusRoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.BonusRoyalRoad,goal=64), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=32),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.BonusRoyalRoad[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.BonusRoyalRoad,file="std.BonusRoyalRoad")

graph = gridConstructor(100)
stdSpatial.BonusRoyalRoad = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.BonusRoyalRoad,goal=64), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=32),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.BonusRoyalRoad[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.BonusRoyalRoad,file="stdSpatial.BonusRoyalRoad")

