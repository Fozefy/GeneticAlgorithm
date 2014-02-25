n=50
graph = complete.graph(100)
elite.one.CoevoComp = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.one.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.one.CoevoComp,file="elite.one.CoevoComp")

graph = complete.graph(100)
elite.two.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.two.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.two.CoevoComp,file="elite.two.CoevoComp")

graph = complete.graph(100)
elite.three.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.three.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.three.CoevoComp,file="elite.three.CoevoComp")

graph = complete.graph(100)
elite.five.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.five.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.five.CoevoComp,file="elite.five.CoevoComp")

graph = complete.graph(100)
elite.ten.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.ten.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.ten.CoevoComp,file="elite.ten.CoevoComp")

graph = complete.graph(100)
elite.full.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=100,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.full.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.full.CoevoComp,file="elite.full.CoevoComp")

graph = complete.graph(100)
elite.none.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.none.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.none.CoevoComp,file="elite.none.CoevoComp")