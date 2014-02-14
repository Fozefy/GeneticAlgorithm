ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=TRUE)


n=30

#Coevo Spatial
graph = ring.graph(100)
generations = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)

  generations[i] = ga$gen

  print(paste(i,"Complete"))
  rm(ga)
}

#Standard
generations2 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), fitness.args=new.fitness.args(goal=60), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)

  generations2[i] = ga$gen

  print(paste(i,"Complete"))
  rm(ga)
}

#Standard Spatial
graph = gridConstructor.withDiag(100)

generations3 = c(1)

for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), fitness.args=new.fitness.args(goal=60), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
  generational.ga(ga)

  generations3[i] = ga$gen

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
n=20
graph = complete.graph(100)
results4 = vector("list",n)
generations4 = c(1)
maxFit4 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)

  generations4[i] = ga$gen

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
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=20), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type="fps",elitism=FALSE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=20),verbose=FALSE)
  generational.ga(ga)

  generationsSpatial.ashlock[i] = ga$gen
  #maxFitSpatial.ashlock[i] = ga$currentGen.results@maxFit@fitness$value

  print(paste(i,"Complete"))
  rm(ga)
}
save(generationsSpatial.ashlock, "stdSpatial_NoElite")

graph = complete.graph(100)
generationsComplete.ashlock = c(1)
maxFitComplete.ashlock = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=20), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type="fps",elitism=FALSE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=20),verbose=FALSE)
  generational.ga(ga)
  
  generationsComplete.ashlock[i] = ga$gen
  #maxFitComplete.ashlock[i] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(generationsComplete.ashlock, "Standard_NoElite")

generationsComplete.ashlock=c(82,162,341,500,448,500,296,500,500,500,409,500,371,500,500,230,437,500,500,234,28,404,113,500,500,17,340,500,500,500,365,500,265,500,59,500,249,500,184,344,500,500,439,500,201,500,218,500,500,148)
generationsSpatial.ashlock= c(500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,450,500,500,196,500)
#generations3.1 = c(55,69,58,82,80,57,103,60,94,66,103,70,65,91,60,50,35,44,52,72,92,65,69,56,110,79,47,66,66,53)


#Data to use in paper
generationsStandard = c(127,225,291,145,84,151,65,129,70,64,94,136,176,165,172,248,199,217,150,238,207,110,85,223,83,194,78,118,189,63,83,127,96,151,154,99,86,90,67,98,197,198,144,232,62,203,135,92,194,107,139,252,95,119,180,180,133,101,175,126,125,151,150,106,112,88,233,104,201,130,88,122,147,100,144,140,127,144,141,103,193,171,156,65,73,141,65,146,126,120,126,198,131,142,88,117,129,113,197,84)
generationsStandardSpatial = c(47,44,46,142,55,47,104,42,82,38,95,69,52,39,73,44,67,30,47,81,65,27,34,36,72,38,57,39,55,309,55,44,45,78,64,87,69,64,76,58,58,106,75,63,138,121,53,53,70,79,62,76,57,96,46,110,72,61,64,55,75,111,44,64,143,108,74,61,50,55,87,51,55,52,69,50,82,72,57,55,65,55,52,68,82,91,40,60,62,52,72,55,45,83,47,80,124,61,92,82)
generationsCoevoSpatial = c(118,72,81,23,19,64,39,43,36,27,29,31,51,57,81,145,24,34,24,24,36,66,43,33,38,32,32,64,28,59,75,28,19,23,53,50,26,33,53,70,167,19,60,66,39,35,35,74,95,25,76,33,30,22,19,54,65,28,91,93,89,101,106,41,51,39,27,24,78,24,80,28,25,17,63,49,36,42,24,28,19,26,28,48,30,24,47,24,65,22,23,37,43,57,50,21,57,28,76,40)
generationsCoevo=c(61,173,52,76,102,67,485,59,81,42,140,63,149,189,34,49,57,85,136,66,105,56,500,77,76,69,77,59,79,55,400,79,112,212,77,32,48,73,72,68,48,90,93,51,132,99,35,86,96,56,210,99,125,90,53,102,81,324,36,163,63,114,183,146,60,70,65,91,125,61,74,96,77,70,54,54,109,500,71,85,94,111,105,310,46,105,119,70,141,61,96,86,86,61,57,120,130,104,62,133)

boxplot(generationsStandard,generationsStandardSpatial,generationsCoevo,generationsCoevoSpatial, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types")