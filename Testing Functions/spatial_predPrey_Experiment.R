n=30
graph = gridConstructor(100)
generations.4graph.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.4graph.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = complete.graph(100)
generations.complete.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.complete.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = gridConstructor.withDiag(100) #8 connections
generations.8graph.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.8graph.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = ring.graph(100) #ring graph
generations.ring4.predprey = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.ring4.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

graph = ring.graph.extra(100) #ring graph more connection
generations.ring8.predprey = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.ring8.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

generations.random4.predprey = c(1)
for (i in 1:n)
{
  graph =randomConstructor(4,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.random4.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

generations.random8.predprey = c(1)
for (i in 1:n)
{
  graph =randomConstructor(8,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  
  generations.random8.predprey[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.randomWithLine4.predprey = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withLine(4,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.randomWithLine4.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

generations.randomWithLine8.predprey = c(1)
for (i in 1:n)
{
  graph =randomConstructor.withLine(8,100)
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations.randomWithLine8.predprey[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

#Experimental Data
generations.4graph.predprey = c(36,31,20,29,32,34,28,36,23,48,20,29,47,21,26,29,21,23,29,17,27,40,25,31,24,23,17,23,33,29,34,32,48,26,28,28,26,23,55,17,34,34,30,23,37,38,42,36,44,23,53,23,30,15,19,38,29,36,15,34,41,38,37,71,31,20,31,62,25,24,36,21,68,43,30,37,29,37,35,39,16,28,55,22,29,27,47,24,27,28,49,52,26,30,32,29,67,66,22,34,23,45,35,27,33,17,18,41,14,30,38,34,27,58,13,24,35,91,33,30,37,30,27,47,20,34,32,26,19,17)

generations.8graph.predprey = c(85,27,29,68,73,23,51,34,28,53,39,56,64,27,47,32,27,35,94,102,52,103,35,74,36,63,58,39,45,36,32,61,56,38,45,74,41,43,30,35,45,52,46,26,24,36,37,62,54,47,50,40,32,79,28,136,52,39,30,74,18,47,58,59,36,82,35,91,37,29,67,63,47,53,25,29,97,56,55,24,29,73,45,80,52,41,98,69,72,39,45,43,65,72,43,46,50,34,54,93)

generations.complete.predprey = c(184,147,445,50,144,352,168,500,500,152,91,115,500,299,118,500,209,179,445,500,433,500,67,500,500,438,37,500,500,67,372,500,195,35,500,249,181,341,37,28,168,263,138,104,74,89,500,236,500,500,277,286,358,164,500,334,500,71,201,500,183,352,43,95,140,500,148,205,499,108,494,72,271,254,500,109,500,202,35,484,500,376,41,73,182,205,143,45,110,491,72,31,500,500,255,225,259,500,500,500,46,128,58,383,472,105,242,500,208,74,108,500,500,305,103,500,76,115,348,53,500,253,500,71,184)

generations.ring4.predprey = c(30,51,30,25,18,30,19,29,29,53,53,25,24,40,18,40,61,26,24,46,35,48,58,70,23,61,23,28,35,33,19,32,26,41,14,45,22,43,38,22,29,27,40,62,30,26,23,26,22,56,14,29,40,32,19,33,26,39,29,37,35,65,26,23,39,55,15,52,31,31,25,56,38,40,17,18,36,33,12,28,70,32,42,27,46,23,27,21,26,13,41,40,17,19,29,28,62,34,33,40)

generations.ring8.predprey = c(69,67,32,38,34,27,36,42,87,41,76,59,74,13,78,83,64,29,66,51,46,38,165,38,70,43,44,62,42,75,94,111,37,92,29,28,34,61,26,25,39,65,41,17,66,122,44,96,39,46,30,91,107,45,47,22,29,87,39,61,27,134,36,41,83,19,57,23,45,59,73,35,99,33,70,56,51,32,20,84,32,51,62,24,40,52,69,34,45,25,35,30,24,28,70,31,28,68,42,34)

generations.random4.predprey = c(56,19,90,29,50,20,25,16,30,17,47,37,24,30,59,23,26,16,19,56,32,59,31,29,57,44,17,17,32,49,64,118,26,28,26,26,61,37,31,53,55,81,26,73,82,35,110,25,32,18,32,43,83,29,27,88,17,45,21,17,27,25,25,57,19,31,25,42,92,191,15,36,29,42,60,30,37,47,38,19,32,27,18,20,51,30,43,29,39,24,27,51,77,31,24,30,52,48,42,56)

generations.random8.predprey =c(60,46,224,354,37,38,39,68,57,76,48,24,80,76,62,40,66,122,29,56,57,24,53,36,59,66,52,46,47,43,23,107,148,25,25,36,29,70,52,49,26,38,44,36,75,31,68,28,63,89,137,21,27,41,34,37,43,68,27,181,33,64,16,17,75,54,38,32,37,440,39,43,38,30,60,32,102,55,44,89,52,95,89,104,43,55,31,21,77,26,80,61,38,70,28,46,34,37,182,53)

generations.randomWithLine4.predprey= c(49,49,45,85,37,21,20,153,30,28,36,31,24,31,60,34,33,65,31,52,117,43,54,28,26,30,21,33,53,24,48,32,69,42,30,38,26,93,25,32,24,24,22,124,26,74,56,25,36,43,24,25,38,43,25,22,99,31,53,43,46,40,32,33,26,23,41,49,24,13,26,30,17,17,19,39,91,28,25,27,23,22,27,27,49,31,16,17,30,33,18,31,25,47,88,32,33,25,30,20)

generations.randomWithLine8.predprey = c(35,68,63,64,43,40,59,69,60,49,45,63,82,26,76,35,42,41,33,45,38,38,35,30,44,100,29,36,57,88,26,37,32,35,52,24,99,39,23,70,20,51,37,62,39,110,32,66,36,114,42,48,32,29,76,27,65,47,53,35,36,41,41,113,26,69,44,27,60,31,27,34,58,39,44,96,28,94,34,27,84,67,52,21,65,44,41,56,66,73,30,54,51,33,154,155,40,34,72,40)

median(generations.4graph.predprey);median(generations.8graph.predprey);median(generations.complete.predprey);median(generations.ring4.predprey);median(generations.ring8.predprey);median(generations.random4.predprey);median(generations.random8.predprey);median(generations.randomWithLine4.predprey);median(generations.randomWithLine8.predprey)

var(generations.4graph.predprey);var(generations.random4.predprey);var(generations.randomWithLine4.predprey);var(generations.ring4.predprey)
var(generations.8graph.predprey);var(generations.random8.predprey);var(generations.randomWithLine8.predprey);var(generations.ring8.predprey)
var(generations.complete.predprey)

boxplot(generations.4graph.predprey,generations.8graph.predprey,generations.complete.predprey,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"))