#Testing Spatial Effects
n=20

graph = gridConstructor(100) #4 connections
generations.4graph = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.4graph[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = gridConstructor.withDiag(100) #8 connections
generations.8graph = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.8graph[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = complete.graph(100) #complete connections
generations.complete = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)

  generations.complete[i] = ga$gen

  print(paste(i,"Complete"))
  rm(ga)
}

graph = ring.graph(100) #ring graph
generations.ring4 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.ring4[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.ring4,file="coevo.ring4.2elite")

graph = ring.graph.extra(100) #ring graph more connection
generations.ring8 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withMatching(), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)

  generations.ring8[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.ring8,file="coevo.ring8.2elite")

#random graph - 4 connections
generations.random4 = c(1)
for (i in 1:n)
{
  graph =randomConstructor.NoDuplicate(4,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.random4[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.random4,file="coevo.rand4.2elite")

#random graph - 8 connections
generations.random8 = c(1)
for (i in 1:n)
{
  graph =randomConstructor.NoDuplicate(8,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)

  generations.random8[i] = ga$gen

  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.random8,file="coevo.rand8.2elite")

#random graph with line - 4 connections
generations.randomWithLine4 = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withLine(4,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.randomWithLine4[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

#random graph with line - 8 connections
generations.randomWithLine8 = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withLine(8,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.randomWithLine8[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

#random graph 2 seperate pop - 4 connections
generations.rand.2pop.4conn = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withSeperatePop.noDuplicate(4,100,2)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.rand.2pop.4conn[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.rand.2pop.4conn,file="rand.2pop.4conn")

#random graph 2 seperate pop - 8 connections
generations.rand.2pop.8conn = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withSeperatePop.noDuplicate(8,100,2)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.rand.2pop.8conn[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.rand.2pop.8conn,file="rand.2pop.8conn")

#random graph 4 seperate pop - 4 connections
generations.rand.4pop.4conn = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withSeperatePop.noDuplicate(4,100,4)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.rand.4pop.4conn[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.rand.4pop.4conn,file="rand.4pop.4conn")

#random graph 4 seperate pop - 8 connections
generations.rand.4pop.8conn = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withSeperatePop.noDuplicate(8,100,4)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.rand.4pop.8conn[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.rand.4pop.8conn,file="rand.4pop.8conn")

#random graph 10 seperate pop - 4 connections
generations.rand.10pop.4conn = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withSeperatePop.noDuplicate(4,100,10)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.rand.10pop.4conn[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.rand.10pop.4conn,file="rand.10pop.4conn")

#random graph 10 seperate pop - 8 connections
generations.rand.10pop.8conn = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withSeperatePop.noDuplicate(8,100,10)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.rand.10pop.8conn[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generations.rand.10pop.8conn,file="rand.10pop.8conn")

#Experiment Data
generations.4graph = c(27,129,20,30,25,52,26,73,73,25,39,63,21,88,53,65,28,21,73,39,22,98,25,57,20,42,124,42,111,38,74,19,43,21,19,64,23,124,26,24,36,27,87,26,27,34,64,28,23,51,30,23,18,24,20,61,21,130,18,32,38,33,105,44,33,131,37,30,20,123,45,32,44,29,31,51,47,60,34,88,60,31,53,22,64,30,45,107,100,22,23,31,30,29,24,18,42,23,52,43)

generations.8graph=c(131,95,33,93,28,41,134,31,27,101,32,37,46,42,82,37,83,47,66,22,41,44,142,60,27,41,90,36,34,46,79,45,29,62,34,33,104,37,26,19,28,82,36,35,77,31,48,27,35,50,33,57,24,110,53,69,25,109,42,33,48,112,157,250,43,125,100,76,23,45,22,37,109,167,148,30,166,21,55,60,36,21,22,44,93,24,135,74,30,40,43,40,163,47,39,250,33,24,25,87)

generations.complete = c(61,173,52,76,102,67,485,59,81,42,140,63,149,189,34,49,57,85,136,66,105,56,500,77,76,69,77,59,79,55,68,64,73,91,110,75,93,315,107,69,107,80,174,47,86,175,71,106,34,82,75,75,58,75,326,40,78,66,292,40,123,73,91,282,62,79,33,82,312,61,63,83,87,108,91,95,67,42,236,83,136,168,77,147,52,66,432,114,74,57,138,79,66,44,85,79,102,500,75,52)

generations.ring4=c(45,23,41,48,81,48,20,49,40,31,21,48,63,85,61,109,23,174,74,19,30,21,56,33,62,18,37,99,22,36,65,36,29,20,138,42,21,80,30,25,21,43,18,40,55,27,82,42,56,22,20,24,27,50,21,39,99,45,44,18,33,35,41,26,20,22,20,53,74,38,71,22,32,87,50,259,21,20,60,17,65,26,19,56,21,23,29,23,20,20,23,32,68,20,45,106,46,32,29,110)

generations.ring8 = c(80,28,88,37,44,53,34,34,26,44,73,82,46,33,66,48,70,46,22,21,29,54,29,93,62,32,31,81,30,71,93,61,41,46,29,30,63,55,36,26,48,25,47,75,33,127,54,96,109,56,42,33,44,32,234,194,34,44,36,38,35,45,49,159,40,43,34,51,51,165,35,23,41,28,32,36,26,112,310,54,24,56,189,136,66,28,36,145,70,139,96,30,110,30,45,58,63,40,25,34)

generations.random4 = c(56,43,33,27,23,34,66,30,102,53,33,62,26,33,35,35,68,27,97,32,100,31,22,20,21,54,48,19,33,31,84,149,20,22,43,173,41,79,100,88,18,46,38,96,17,42,42,33,208,500,19,32,51,68,19,20,76,19,18,169,119,49,15,33,34,59,57,72,200,24,12,61,39,22,62,38,57,25,42,30,66,40,31,25,23,22,23,46,31,34,28,24,33,22,71,30,132,40,38,56)

generations.random8 = c(95,42,37,33,45,44,51,29,43,24,402,137,103,54,31,42,500,164,63,95,38,72,44,44,37,226,49,214,114,38,123,49,144,33,38,38,42,27,23,31,99,42,114,28,148,34,151,89,29,51,29,42,48,29,30,108,31,79,64,34,28,26,29,500,39,43,106,31,51,45,35,41,104,82,107,52,249,26,32,102,115,76,20,55,163,500,44,39,53,68,53,29,28,32,33,30,59,39,50,84)

generations.randomWithLine4 = c(34,31,28,101,61,35,50,76,320,19,500,85,97,30,17,34,19,23,52,160,54,33,96,34,32,26,28,25,88,24,22,79,33,62,91,29,24,19,28,23,103,33,26,43,27,63,64,44,35,19,45  26 500 245  37  58  31 251  41  90  23  25  49  75  37  68  29  30 114  72 117  48  57  45  23  56  67  20  17  40,18  55  28  19  25  40  23  65  25  70  20 151  29  39  19  28  27  77  51  21)

generations.randomWithLine8 = c(36,37,78,279,500,23,500,22,26,150,30,26,31,50,44,149,112,40,180,133,159,26,105,103,60,77,46,31,500,46,30,50,33,76,115,111,187,25,73,54,68,98,99,41,378,28,33,142,51,22,37,126,87,68,52,44,37,24,23,116,21,29,29,23,95,500,110,43,36,29,28,24,21,48,42,43,134,41,38,37,53,39,155,36,35,75,30,27,79,34,36 205,100,98,32,500,37,35,53,500)

median(generations.4graph);median(generations.8graph);median(generations.complete);median(generations.ring4);median(generations.ring8);median(generations.random4);median(generations.random8);median(generations.randomWithLine4);median(generations.randomWithLine8)

var(generations.4graph);var(generations.random4);var(generations.randomWithLine4);var(generations.ring4)
var(generations.8graph);var(generations.random8);var(generations.randomWithLine8);var(generations.ring8)
var(generations.complete)

boxplot(generations.4graph,generations.8graph,generations.complete,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"), main="Spatial Effects on Spatial One-Max")

boxplot(generations.4graph,generations.ring4,generations.8graph,generations.ring8,generations.complete,ylab="Generations", names=c("4 Grid","4 Ring","8 Ring", "8 Grid", "Complete"), )