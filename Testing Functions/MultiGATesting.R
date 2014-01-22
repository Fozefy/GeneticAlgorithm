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
n=50

graph = gridConstructor(100) #4 connections
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
#generations = c(27,129,20,30,25,52,26,73,73,25,39,63,21,88,53,65,28,21,73,39,22,98,25,57,20,42,124,42,111,38,74,19,43,21,19,64,23,124,26,24,36,27,87,26,27,34,64,28,23,51)
generations = c(generations,30,23,18,24,20,61,21,130,18,32,38,33,105,44,33,131,37,30,20,123,45,32,44,29,31,51,47,60,34,88,60,31,53,22,64,30,45,107,100,22,23,31,30,29,24,18,42,23,52,43)

graph = gridConstructor.withDiag(100) #9 connections

results2 = vector("list",n)
generations2 = c(1)
maxFit2 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results2[[i]]=ga$reported.data
  generations2[i] = ga$gen
  maxFit2[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}
#generations2=c(131,95,33,93,28,41,134,31,27,101,32,37,46,42,82,37,83,47,66,22,41,44,142,60,27,41,90,36,34,46,79,45,29,62,34,33,104,37,26,19,28,82,36,35,77,31,48,27,35,50)
generations2 =  c(generations2,33,57,24,110,53,69,25,109,42,33,48,112,157,250,43,125,100,76,23,45,22,37,109,167,148,30,166,21,55,60,36,21,22,44,93,24,135,74,30,40,43,40,163,47,39,250,33,24,25,87)

graph = complete.graph(100) #complete connections
results3 = vector("list",n)
generations3 = c(1)
maxFit3 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
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
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results4[[i]]=ga$reported.data
  generations4[i] = ga$gen
  maxFit4[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}
#generations4=c(45,23,41,48,81,48,20,49,40,31,21,48,63,85,61,109,23,174,74,19,30,21,56,33,62,18,37,99,22,36,65,36,29,20,138,42,21,80,30,25,21,43,18,40,55,27,82,42,56,22)
generations4 = c(generations4,20,24,27,50,21,39,99,45,44,18,33,35,41,26,20,22,20,53,74,38,71,22,32,87,50,259,21,20,60,17,65,26,19,56,21,23,29,23,20,20,23,32,68,20,45,106,46,32,29,110)

graph = ring.graph.extra(100) #ring graph more connection
results5 = vector("list",n)
generations5 = c(1)
maxFit5 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results5[[i]]=ga$reported.data
  generations5[i] = ga$gen
  maxFit5[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#generations5 = c(80,28,88,37,44,53,34,34,26,44,73,82,46,33,66,48,70,46,22,21,29,54,29,93,62,32,31,81,30,71,93,61,41,46,29,30,63,55,36,26,48,25,47,75,33,127,54,96,109,56)
generations5 = c(generations5,42,33,44,32,234,194,34,44,36,38,35,45,49,159,40,43,34,51,51,165,35,23,41,28,32,36,26,112,310,54,24,56,189,136,66,28,36,145,70,139,96,30,110,30,45,58,63,40,25,34)
#ring graph w/ small world
results6 = vector("list",n)
generations6 = c(1)
maxFit6 = c(1)
for (i in 1:n)
{
  graph = ring.graph.smallWorld(100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
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
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
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
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  results8[[i]]=ga$reported.data
  generations8[i] = ga$gen
  maxFit8[i] = ga$currentGen.results@maxFit@fitness$value
  print(paste(i,"Complete"))
  rm(ga)
}

#random graph with line - 4 connections
generations9 = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withLine(4,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations9[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations9 = c(34  31  28 101  61  35  50  76 320  19 500  85  97  30  17  34  19  23  52 160  54  33  96  34  32  26  28  25  88  24  22  79  33  62  91  29  24  19  28  23 103  33  26 43  27  63  64  44  35  19)
#random graph with line - 8 connections
generations10 = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withLine(8,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations10[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = singleRing(100)
generations11 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations11[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

generations11 = c(24  34  38  63  44  54  91  16  25 104  18  42  73  28  75  43  83  26 113  44  32  20  53  27  36  75  36  31  27  47  23  14  46  22  24  30  24  32  30  73  18  73  13 52  26  51  81  18  23  15)

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
