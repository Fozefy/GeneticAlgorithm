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

generations.8graph.predprey = c(85,27,29,68,73,23,51,34,28,53,39,56,64,27,47,32,27,35,94,102,52,103,35,74,36,63,58,39,45,36)

generations.complete.predprey = c(184,147,445,50,144,352,168,500,500,152,91,115,500,299,118,500,209,179,445,500,433,500,67,500,500,438,37,500,500,67,372,500,195,35,500,249,181,341,37,28,168,263,138,104,74,89,500,236,500,500,277,286,358,164,500,334,500,71,201,500,183,352,43,95,140,500,148,205,499,108,494,72,271,254,500,109,500,202,35,484,500,376,41,73,182,205,143,45,110,491,72,31,500,500,255,225,259,500,500,500,46,128,58,383,472,105,242,500,208,74,108,500,500,305,103,500,76,115,348,53,500,253,500,71,184)

generations.ring4.predprey = c(30,51,30,25,18,30,19,29,29,53,53,25,24,40,18,40,61,26,24,46,35,48,58,70,23,61,23,28,35,33,19,32,26,41,14,45,22,43,38,22,29,27,40,62,30,26,23,26,22,56)

generations.ring8.predprey = c(69,67,32,38,34,27,36,42,87,41,76,59,74,13,78,83,64,29,66,51,46,38,165,38,70,43,44,62,42,75,94,111,37,92,29,28,34,61,26,25,39,65,41,17,66,122,44,96,39,46)

generations.random4.predprey = c(56,19,90,29,50,20,25,16,30,17,47,37,24,30,59,23,26,16,19,56,32,59,31,29,57,44,17,17,32,49,64,118,26,28,26,26,61,37,31,53,55,81,26,73,82,35,110,25,32,18)

generations.random8.predprey =c(60,46,224,354,37,38,39,68,57,76,48,24,80,76,62,40,66,122,29,56,57,24,53,36,59,66,52,46,47,43,23,107,148,25,25,36,29,70,52,49,26,38,44,36,75,31,68,28,63,89)

generations.randomWithLine4.predprey= c(49,49,45,85,37,21,20,153,30,28,36,31,24,31,60,34,33,65,31,52,117,43,54,28,26,30,21,33,53,24,48,32,69,42,30,38,26,93,25,32,24,24,22,124,26,74,56,25,36,43)

generations.randomWithLine8.predprey = c(35,68,63,64,43,40,59,69,60,49,45,63,82,26,76,35,42,41,33,45,38,38,35,30,44,100,29,36,57,88,26,37,32,35,52,24,99,39,23,70,20,51,37,62,39,110,32,66,36,114)

median(generations.4graph.predprey);median(generations.8graph.predprey);median(generations.complete.predprey);median(generations.ring4.predprey);median(generations.ring8.predprey);median(generations.random4.predprey);median(generations.random8.predprey);median(generations.randomWithLine4.predprey);median(generations.randomWithLine8.predprey)