n=50
graph = gridConstructor(100)
hard.elite.one = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.one[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.one,file="hard.elite.one")

graph = gridConstructor(100)
hard.elite.two = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn = reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.two,file="hard.elite.two")

graph = gridConstructor(100)
hard.elite.three = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.three[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.three,file="hard.elite.three")

graph = gridConstructor(100)
hard.elite.five = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.five[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.five,file="hard.elite.five")

graph = gridConstructor(100)
hard.elite.ten = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.ten[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.ten,file="hard.elite.ten")

graph = gridConstructor(100)
hard.elite.fifty = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=50,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.fifty[i] = ga$gen
  
  print(paste(i,"Complete -",ga$gen))
  rm(ga)
}
save(hard.elite.fifty,file="hard.elite.fifty")

graph = gridConstructor(100)
hard.elite.full = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=100,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.full[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.full,file="hard.elite.full")

#boxplot(hard.elite.one,hard.elite.two,hard.elite.three,hard.elite.five,hard.elite.ten,names=c("One","Two","Three","Five","Ten"), main="Elites on Coevo - 4 Grid")


#Large Grid
graph = gridConstructor.withDiag(100)
hard.elite.one.BigGrid = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.one.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.one.BigGrid,file="hard.elite.one.BigGrid")

graph = gridConstructor.withDiag(100)
hard.elite.two.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.elite.two.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.two.BigGrid,file="hard.elite.two.BigGrid")

graph = gridConstructor.withDiag(100)
hard.elite.three.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.three.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.three.BigGrid,file="hard.elite.three.BigGrid")

graph = gridConstructor.withDiag(100)
hard.elite.five.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.five.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.five.BigGrid,file="hard.elite.five.BigGrid")

graph = gridConstructor.withDiag(100)
hard.elite.ten.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.ten.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.ten.BigGrid,file="hard.elite.ten.BigGrid")

graph = gridConstructor.withDiag(100)
hard.elite.full.BigGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=100,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.full.BigGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.full.BigGrid,file="hard.elite.full.BigGrid")

#boxplot(hard.elite.one.BigGrid,hard.elite.two.BigGrid,hard.elite.three.BigGrid,hard.elite.five.BigGrid,hard.elite.ten.BigGrid,names=c("One","Two","Three","Five","Ten"), main="Elites on Coevo - 8 Grid")

#Big Pop
graph = gridConstructor(1024)
hard.elite.one.BigPop = c(1)

for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.one.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.one.BigPop,file="hard.elite.one.BigPop")

graph = gridConstructor(1024)
hard.elite.two.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.two.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.two.BigPop,file="hard.elite.two.BigPop")

graph = gridConstructor(1024)
hard.elite.three.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=3,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.three.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.three.BigPop,file="hard.elite.three.BigPop")

graph = gridConstructor(1024)
hard.elite.five.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.five.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.five.BigPop,file="hard.elite.five.BigPop")

graph = gridConstructor(1024)
hard.elite.ten.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:1024, 1:1024), nrow=1024, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  hard.elite.ten.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.elite.ten.BigPop,file="hard.elite.ten.BigPop")