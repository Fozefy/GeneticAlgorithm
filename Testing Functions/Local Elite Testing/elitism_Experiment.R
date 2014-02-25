n=50
graph = gridConstructor(100)
elite.one = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.one[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.one,file="elite.one")

graph = gridConstructor(100)
elite.two = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.two,file="elite.two")

graph = gridConstructor(100)
elite.three = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.three[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.three,file="elite.three")

graph = gridConstructor(100)
elite.five = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.five[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.five,file="elite.five")

graph = gridConstructor(100)
elite.ten = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.ten[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.ten,file="elite.ten")

graph = gridConstructor(100)
elite.full = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=100,elitism=TRUE,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  elite.full[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.full,file="elite.full")

graph = gridConstructor(100)
elite.none = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.none[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.none,file="elite.none")

#boxplot(elite.one,elite.two,elite.three,elite.five,elite.ten,names=c("One","Two","Three","Five","Ten"), main="Elites on Coevo - 4 Grid")


#Large Grid
graph = gridConstructor.withDiag(100)
elite.one.BigGrid = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.one.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.one.BigGrid,file="elite.one.BigGrid")

graph = gridConstructor.withDiag(100)
elite.two.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.two.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.two.BigGrid,file="elite.two.BigGrid")

graph = gridConstructor.withDiag(100)
elite.three.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.three.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.three.BigGrid,file="elite.three.BigGrid")

graph = gridConstructor.withDiag(100)
elite.five.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.five.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.five.BigGrid,file="elite.five.BigGrid")

graph = gridConstructor.withDiag(100)
elite.ten.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.ten.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.ten.BigGrid,file="elite.ten.BigGrid")

graph = gridConstructor.withDiag(100)
elite.full.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=100,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.full.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.full.BigGrid,file="elite.full.BigGrid")

graph = gridConstructor.withDiag(100)
elite.none.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.none.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.none.BigGrid,file="elite.none.BigGrid")

#boxplot(elite.one.BigGrid,elite.two.BigGrid,elite.three.BigGrid,elite.five.BigGrid,elite.ten.BigGrid,elite.full.BigGrid,names=c("One","Two","Three","Five","Ten","100"), main="Elites on Coevo - 8 Grid")

#Big Pop
graph = gridConstructor(1024)
elite.one.BigPop = c(1)

for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.one.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.one.BigPop,file="elite.one.BigPop")

graph = gridConstructor(1024)
elite.two.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.two.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.two.BigPop,file="elite.two.BigPop")

graph = gridConstructor(1024)
elite.three.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.three.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.three.BigPop,file="elite.three.BigPop")

graph = gridConstructor(1024)
elite.five.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.five.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.five.BigPop,file="elite.five.BigPop")

graph = gridConstructor(1024)
elite.ten.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  elite.ten.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.ten.BigPop,file="elite.ten.BigPop")