track.elite <- function(pop, mutation, cross, elite,...)
{
  elites = NULL
  for (i in 1:length(elite[[1]]))
  {
    elites[i] = elite[[1]][[i]]@index$value
  }
  elites
}

elite.locations.one = NULL
graph = gridConstructor(100)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE, reporting.fn=track.elite)
  generational.ga(ga)
  
  elite.locations = NULL
  for(j in 2:length(ga$reported.data))
  {
    elite.locations[[j-1]] = ga$reported.data[[j]]@currentGen.results
  }
  
  elite.locations.one[[i]] = elite.locations
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.locations.one,file="elite.locations.one")

elite.locations.two = NULL
graph = gridConstructor(100)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=2,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE, reporting.fn=track.elite)
  generational.ga(ga)
  
  elite.locations = NULL
  for(j in 2:length(ga$reported.data))
  {
    elite.locations[[j-1]] = ga$reported.data[[j]]@currentGen.results
  }
  
  elite.locations.two[[i]] = elite.locations
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.locations.two,file="elite.locations.two")


for(i in 1:n)
{
  finalElite = elite.locations.one[[i]][[length(elite.locations.one[[i]])]]
  print(length(elite.locations.one[[i]])-19-sum(elite.locations.one[[i]][(20):length(elite.locations.one[[i]])] == finalElite))
  #print(50-sum(elite.locations.one[[i]][(length(elite.locations.one[[i]]) - 49):length(elite.locations.one[[i]])] == finalElite))
}