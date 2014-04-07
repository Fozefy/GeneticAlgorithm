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

elite.locations.five = NULL
graph = gridConstructor(100)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=5,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE, reporting.fn=track.elite)
  generational.ga(ga)
  
  elite.locations = NULL
  for(j in 2:length(ga$reported.data))
  {
    elite.locations[[j-1]] = ga$reported.data[[j]]@currentGen.results
  }
  
  elite.locations.five[[i]] = elite.locations
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.locations.five,file="elite.locations.five")

elite.locations.ten = NULL
graph = gridConstructor(100)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=10,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE, reporting.fn=track.elite)
  generational.ga(ga)
  
  elite.locations = NULL
  for(j in 2:length(ga$reported.data))
  {
    elite.locations[[j-1]] = ga$reported.data[[j]]@currentGen.results
  }
  
  elite.locations.ten[[i]] = elite.locations
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(elite.locations.ten,file="elite.locations.ten")
