n=50
graph = complete.graph(100)
hard.std.elite.one = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  hard.std.elite.one[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.std.elite.one,file="hard.std.elite.one")

graph = complete.graph(100)
hard.std.elite.two = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE,reporting.fn = reportNone.report.fn)
  generational.ga(ga)
  
  hard.std.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.std.elite.two,file="hard.std.elite.two")

graph = complete.graph(100)
hard.std.elite.three = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=3,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  hard.std.elite.three[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.std.elite.three,file="hard.std.elite.three")

graph = complete.graph(100)
hard.std.elite.five = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=5,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  hard.std.elite.five[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.std.elite.five,file="hard.std.elite.five")

graph = complete.graph(100)
hard.std.elite.ten = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=10,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  hard.std.elite.ten[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.std.elite.ten,file="hard.std.elite.ten")

graph = complete.graph(100)
hard.std.elite.full = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=100,spatial.selection.fn=spatial.child.selection.random.hardElite,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  
  hard.std.elite.full[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(hard.std.elite.full,file="hard.std.elite.full")
