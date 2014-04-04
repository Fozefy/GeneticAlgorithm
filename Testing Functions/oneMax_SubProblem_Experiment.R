source('Fitness Functions - Extra/onemaxWithSubProblems.R')

graph = gridConstructor(100)
coevo.4Graph.subPattern.elite.two = c(1)
chrLength = 30
for (i in 1:n)
{
  problem = twoPop.one.max.withSubPattern(geneLength=chrLength)
  
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(problem[[1]],goal=problem[[2]], externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=chrLength), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.4Graph.subPattern.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.4Graph.subPattern.elite.two,file="coevo.4Graph.subPattern.elite.two")

graph = complete.graph(100)
coevo.comp.subPattern.elite.two = c(1)
chrLength = 30
for (i in 1:n)
{
  problem = twoPop.one.max.withSubPattern(geneLength=chrLength)
  
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=problem[[1]],goal=problem[[2]], externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=chrLength), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.comp.subPattern.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.comp.subPattern.elite.two,file="coevo.comp.subPattern.elite.two")

graph = complete.graph(100)
std.comp.subPattern.elite.two = c(1)
chrLength = 60
for (i in 1:n)
{
  problem = onePop.one.max.withSubPattern(geneLength=chrLength)
  
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=problem[[1]],goal=problem[[2]]), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=chrLength),verbose=FALSE)
  generational.ga(ga)
  
  std.comp.subPattern.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.comp.subPattern.elite.two,file="std.comp.subPattern.elite.two")

graph = gridConstructor(100)
std.4Graph.subPattern.elite.two = c(1)
chrLength = 60
for (i in 1:n)
{
  problem = onePop.one.max.withSubPattern(geneLength=chrLength)
  
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=problem[[1]],goal=problem[[2]]), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=chrLength),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.4Graph.subPattern.elite.two[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.4Graph.subPattern.elite.two,file="std.4Graph.subPattern.elite.two")