n=50

graph = complete.graph(100)
std.elite.one = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.one[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.one,file="std.elite.one")

graph = complete.graph(100)
std.elite.two = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.two,file="std.elite.two")

graph = complete.graph(100)
std.elite.three = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=3,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.three[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.three,file="std.elite.three")

graph = complete.graph(100)
std.elite.five = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=5,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.five[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.five,file="std.elite.five")

graph = complete.graph(100)
std.elite.ten = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=10,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.ten[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.ten,file="std.elite.ten")

graph = complete.graph(100)
std.elite.full = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=100,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.full[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.full,file="std.elite.full")

#Big Pop
std.elite.one.BigPop = c(1)

for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.one.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.one.BigPop,file="std.elite.one.BigPop")

std.elite.two.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.two.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.two.BigPop,file="std.elite.two.BigPop")

std.elite.three.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=3), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.three.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.three.BigPop,file="std.elite.three.BigPop")

std.elite.five.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=5), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.five.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.five.BigPop,file="std.elite.five.BigPop")

std.elite.ten.BigPop = c(1)
for (i in 1:n)
{
  ga = new.GA.env(pop.size=1024,GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=10), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  std.elite.ten.BigPop[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.elite.ten.BigPop,file="std.elite.ten.BigPop")