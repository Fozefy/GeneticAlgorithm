n=30
graph = gridConstructor(100)
generationsSpatialPred = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generationsSpatialPred[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

#generations = c(36,31,20,29,32,34,28,36,23,48,20,29,47,21,26,29,21,23,29,17,27,40,25,31,24,23,17,23,33,29,34,32,48,26,28,28,26,23,55,17,34,34,30,23,37,38,42,36,44,23)
generationsSpatialPred = c(53,23,30,15,19,38,29,36,15,34,41,38,37,71,31,20,31,62,25,24,36,21,68,43,30,37,29,37,35,39)


graph = complete.graph(100)
generations2 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.predPrey, goal=31, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), verbose=FALSE)
  generational.ga(ga)
  generations2[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
#generations2 = c(184,147,445,50,144,352,168,500,500,152,91,115,500,299,118,500,209,179,445,500,433,500,67,500,500,438,37,500,500,67,372,500,195,35,500,249,181,341,37,28,168,263,138,104,74)
generations2 = c(89,500,236,500,500,277,286,358,164,500,334,500,71,201,500,183,352,43,95,140,500,148,205,499,108,494,72,271,254,500)