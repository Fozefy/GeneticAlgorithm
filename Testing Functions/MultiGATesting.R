source("spatialConstructors.R")

#Test Many
graph = gridConstructor(100)
n=20
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
}

results2 = vector("list",n)
generations2 = c(1)
maxFit2 = c(1)
for (i in 1:20)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results2[[i]] = ga$reported.data
  generations2[i] = ga$gen
  maxFit2[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
}

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