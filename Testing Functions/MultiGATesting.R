source("Testing Functions/spatialConstructors.R")

#Test Many
n=40
graph = gridConstructor(100)

#With Elite
results = vector("list",n)
generations = c(1)
maxFit = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results[[i]]=ga$reported.data
  generations[i] = ga$gen
  maxFit[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#No ELite
results2 = vector("list",n)
generations2 = c(1)
maxFit2 = c(1)
for (i in 1:20)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=TRUE)
  generational.ga(ga)
  results2[[i]] = ga$reported.data
  generations2[i] = ga$gen
  maxFit2[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
}

results3 = vector("list",n)
generations3 = c(1)
maxFit3 = c(1)
for (i in 1:20)
{
  ga = ga = new.GA.env(selection.args=new.selection.args(maximizing = TRUE), fitness.args=new.fitness.args(goal=30), verbose=FALSE)
  generational.ga(ga)
  results3[[i]] = ga$reported.data
  generations3[i] = ga$gen
  maxFit3[i] = ga$currentGen.results@elite[[1]]$maxFit@fitness$value
  print(paste(i,"Complete"))
}

results4 = vector("list",n)
generations4 = c(1)
maxFit4 = c(1)
for (i in 1:20)
{
  ga = ga = new.GA.env(selection.args=new.selection.args(maximizing = TRUE), fitness.args=new.fitness.args(goal=30), verbose=FALSE)
  generational.ga(ga)
  results4[[i]] = ga$reported.data
  generations4[i] = ga$gen
  maxFit4[i] = ga$currentGen.results@elite[[1]]$maxFit@fitness$value
  print(paste(i,"Complete"))
}

#Testing Spatial Effects
n=20

graph = gridConstructor(100) #4 connections
results = vector("list",n)
generations = c(1)
maxFit = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results[[i]]=ga$reported.data
  generations[i] = ga$gen
  maxFit[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

graph = gridConstructor.withDiag(100) #9 connections

results2 = vector("list",n)
generations2 = c(1)
maxFit2 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results2[[i]]=ga$reported.data
  generations2[i] = ga$gen
  maxFit2[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

graph = complete.graph(100) #complete connections
results3 = vector("list",n)
generations3 = c(1)
maxFit3 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results3[[i]]=ga$reported.data
  generations3[i] = ga$gen
  maxFit3[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

graph = ring.graph(100) #ring graph
results4 = vector("list",n)
generations4 = c(1)
maxFit4 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results4[[i]]=ga$reported.data
  generations4[i] = ga$gen
  maxFit4[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

graph = ring.graph.extra(100) #ring graph more connection
results5 = vector("list",n)
generations5 = c(1)
maxFit5 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results5[[i]]=ga$reported.data
  generations5[i] = ga$gen
  maxFit5[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#ring graph w/ small world
results6 = vector("list",n)
generations6 = c(1)
maxFit6 = c(1)
for (i in 1:n)
{
  graph = ring.graph.smallWorld(100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results6[[i]]=ga$reported.data
  generations6[i] = ga$gen
  maxFit6[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#random graph - 4 connections
results7 = vector("list",n)
generations7 = c(1)
maxFit7 = c(1)
for (i in 1:n)
{
  graph =randomConstructor(4,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results7[[i]]=ga$reported.data
  generations7[i] = ga$gen
  maxFit7[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#random graph - 8 connections
results8 = vector("list",n)
generations8 = c(1)
maxFit8 = c(1)
for (i in 1:n)
{
  graph =randomConstructor(8,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results8[[i]]=ga$reported.data
  generations8[i] = ga$gen
  maxFit8[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#Print fitness result graphs to files
for (a in 1:n)
{
  fitNum = a
  fitness = NULL
  for (i in 2:length(results2[[fitNum]]))
  {
    fitness[[i-1]] = results2[[fitNum]][[i]]@currentGen.results@maxFit@fitness$value
  }
  jpeg(paste("plot",a,"_NoElite.jpg"))
  plot(fitness)
  dev.off()
}
