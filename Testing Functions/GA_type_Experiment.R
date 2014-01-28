ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=TRUE)


n=20

#Coevo Spatial
graph = ring.graph(100)
results = vector("list",n)
generations = c(1)
maxFit = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results[[i]]=ga$reported.data
  generations[i] = ga$gen
  maxFit[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#Standard
results2 = vector("list",n)
generations2 = c(1)
maxFit2 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), fitness.args=new.fitness.args(goal=60), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  results2[[i]]=ga$reported.data
  generations2[i] = ga$gen
  maxFit2[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#Standard Spatial
graph = gridConstructor.withDiag(100)
results3 = vector("list",n)
generations3 = c(1)
maxFit3 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), fitness.args=new.fitness.args(goal=60), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  results3[[i]]=ga$reported.data
  generations3[i] = ga$gen
  maxFit3[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}
graph = ring.graph.extra(100)
generations3.1 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), fitness.args=new.fitness.args(goal=60), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)
  generations3.1[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}


#Coevo
graph = complete.graph(100)
results4 = vector("list",n)
generations4 = c(1)
maxFit4 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results4[[i]]=ga$reported.data
  generations4[i] = ga$gen
  maxFit4[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}


#Compare vs Ashlock
n=40
graph = gridConstructor(100)
generationsSpatial.ashlock = c(1)
maxFitSpatial.ashlock = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), fitness.args=new.fitness.args(goal=20), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type="fps",elitism=FALSE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=20),verbose=FALSE)
  generational.ga(ga)

  generationsSpatial.ashlock[i] = ga$gen
  maxFitSpatial.ashlock[i] = ga$currentGen.results@maxFit@fitness$value

  print(paste(i,"Complete"))
  rm(ga)
}

graph = complete.graph(100)
generationsComplete.ashlock = c(1)
maxFitComplete.ashlock = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), fitness.args=new.fitness.args(goal=20), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type="fps",elitism=FALSE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=20),verbose=FALSE)
  generational.ga(ga)
  
  generationsComplete.ashlock[i] = ga$gen
  maxFitComplete.ashlock[i] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generationsComplete.ashlock
[1]  82 162 341 500 448 500 296 500 500 500
> generationsSpatial.ashlock
[1] 500 500 500 500 500 500 500 500 500 500
#generations3 = c(47,44,46,142,55,47,104,42,82,38,95,69,52,39,73,44,67,30,47,81,65,27,34,36,72,38,57,39,55,309)
#generations2 = c(127,225,291,145,84,151,65,129,70,64,94,136,176,165,172,248,199,217,150,238,207,110,85,223,83,194,78,118,189,63)
#generations = c(118,72,81,23,19,64,39,43,36,27,29,31,51,57,81,145,24,34,24,24,36,66,43,33,38,32,32,64,28,59)
#generations4 = c( 61,173,52,76,102,67,485,59,81,42,140,63,149,189,34,49,57,85,136,66,105,56,500,77,76,69,77,59,79,55)
#generations3.1 = c(55,69,58,82,80,57,103,60,94,66,103,70,65,91,60,50,35,44,52,72,92,65,69,56,110,79,47,66,66,53)