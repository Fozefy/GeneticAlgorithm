n=50
graph = complete.graph(100)
hard.elite.one.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.one.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.one.CoevoComp,file="hard.elite.one.CoevoComp")

graph = complete.graph(100)
hard.elite.two.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn = reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.two.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.two.CoevoComp,file="hard.elite.two.CoevoComp")

graph = complete.graph(100)
hard.elite.three.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.three.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.three.CoevoComp,file="hard.elite.three.CoevoComp")

graph = complete.graph(100)
hard.elite.five.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.five.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.five.CoevoComp,file="hard.elite.five.CoevoComp")

graph = complete.graph(100)
hard.elite.ten.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.ten.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.ten.CoevoComp,file="hard.elite.ten.CoevoComp")

graph = complete.graph(100)
hard.elite.fifty.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=50,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.fifty.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete" -),ga$gen)
  rm(ga)
}
save(hard.elite.fifty.CoevoComp,file="hard.elite.fifty.CoevoComp")

graph = complete.graph(100)
hard.elite.full.CoevoComp = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=100,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.full.CoevoComp[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.full.CoevoComp,file="hard.elite.full.CoevoComp")