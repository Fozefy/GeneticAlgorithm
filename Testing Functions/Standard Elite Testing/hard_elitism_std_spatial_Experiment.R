n=50

graph = gridConstructor(100)
hard.stdSpatial.elite.one = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.stdSpatial.elite.one[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(hard.stdSpatial.elite.one,file="hard.stdSpatial.elite.one")

graph = gridConstructor(100)
hard.stdSpatial.elite.two = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn = reportNone.report.fn)
  generational.ga(ga)
  
  hard.stdSpatial.elite.two[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(hard.stdSpatial.elite.two,file="hard.stdSpatial.elite.two")

graph = gridConstructor(100)
hard.stdSpatial.elite.three = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=3,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  hard.stdSpatial.elite.three[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(hard.stdSpatial.elite.three,file="hard.stdSpatial.elite.three")

graph = gridConstructor(100)
hard.stdSpatial.elite.five = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=5,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  hard.stdSpatial.elite.five[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(hard.stdSpatial.elite.five,file="hard.stdSpatial.elite.five")

graph = gridConstructor(100)
hard.stdSpatial.elite.ten = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=10,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.stdSpatial.elite.ten[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(hard.stdSpatial.elite.ten,file="hard.stdSpatial.elite.ten")

graph = gridConstructor(100)
hard.stdSpatial.elite.fifty = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=50,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  hard.stdSpatial.elite.fifty[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(hard.stdSpatial.elite.fifty,file="hard.stdSpatial.elite.fifty")

graph = gridConstructor(100)
stdSpatial.elite.full = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=100,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn = reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.elite.full[i] = ga$gen
  
  print(paste(i,"Complete -", ga$gen))
  rm(ga)
}
save(stdSpatial.elite.full,file="hard.stdSpatial.elite.full")

#Larger Grid
graph = gridConstructor.withDiag(100)
stdSpatial.elite.one.LargeGrid = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.one.LargeGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.one.LargeGrid,file="stdSpatial.elite.one.LargeGrid")

graph = gridConstructor.withDiag(100)
stdSpatial.elite.two.LargeGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.elite.two.LargeGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.two.LargeGrid,file="stdSpatial.elite.two.LargeGrid")

graph = gridConstructor.withDiag(100)
stdSpatial.elite.three.LargeGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=3,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.three.LargeGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.three.LargeGrid,file="stdSpatial.elite.three.LargeGrid")

graph = gridConstructor.withDiag(100)
stdSpatial.elite.five.LargeGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=5,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.five.LargeGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.five.LargeGrid,file="stdSpatial.elite.five.LargeGrid")

graph = gridConstructor.withDiag(100)
stdSpatial.elite.ten.LargeGrid = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=10,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.ten.LargeGrid[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.ten.LargeGrid,file="stdSpatial.elite.ten.LargeGrid")

#Big Pop
graph = gridConstructor(1024)
stdSpatial.elite.one.BigPop = c(1)

for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.one.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.one.BigPop,file="stdSpatial.elite.one.BigPop")

graph = gridConstructor(1024)
stdSpatial.elite.two.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.two.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.two.BigPop,file="stdSpatial.elite.two.BigPop")

graph = gridConstructor(1024)
stdSpatial.elite.three.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=3,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.three.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.three.BigPop,file="stdSpatial.elite.three.BigPop")

graph = gridConstructor(1024)
stdSpatial.elite.five.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=5,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.five.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.five.BigPop,file="stdSpatial.elite.five.BigPop")

graph = gridConstructor(1024)
stdSpatial.elite.ten.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=10,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  stdSpatial.elite.ten.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.elite.ten.BigPop,file="stdSpatial.elite.ten.BigPop")
