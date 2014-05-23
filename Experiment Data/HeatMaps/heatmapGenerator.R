heatmap(matrix(coevo.1.elite.ga$reported.data[[1255]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)

heatmap(matrix(tabulate(elite.locations.one[[5]], nbins=100),nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)

testList = NULL
for (i in 1:length(elite.locations))
{
  for(j in 1:length(elite.locations[[i]]))
  {
    testList[[i*5+j]] = elite.locations[[i]][[j]]
  }
}

#Std Spatial
graph = gridConstructor(100)
ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=100,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
generational.ga(ga)

graph = gridConstructor(100)
std.1.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,elite.size=1,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
generational.ga(std.1.elite.ga)

graph = gridConstructor(100)
std.none.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=60),verbose=FALSE)
generational.ga(std.none.elite.ga)

#Coevo Elites
graph = gridConstructor(100)
coevo.1.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=1,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
generational.ga(coevo.1.elite.ga)

graph = gridConstructor(100)
coevo.2.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=2,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
generational.ga(coevo.2.elite.ga)

graph = gridConstructor(100)
coevo.5.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=5,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
generational.ga(coevo.5.elite.ga)

graph = gridConstructor(100)
coevo.10.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=10,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
generational.ga(coevo.10.elite.ga)

graph = gridConstructor(100)
coevo.full.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elite.size=100,elitism=TRUE,adjMatrix=graph), verbose=FALSE)
generational.ga(coevo.full.elite.ga)

graph = gridConstructor(100)
coevo.none.elite.ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withCoupling(.5), goal=30, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=FALSE,adjMatrix=graph), verbose=FALSE)
generational.ga(coevo.none.elite.ga)

#Heatmap set
heatmap(matrix(std.none.elite.ga$reported.data[[10]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)
heatmap(matrix(std.none.elite.ga$reported.data[[1000]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)
heatmap(matrix(std.none.elite.ga$reported.data[[2000]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)
heatmap(matrix(std.none.elite.ga$reported.data[[3000]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)
heatmap(matrix(std.none.elite.ga$reported.data[[4000]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)
heatmap(matrix(std.none.elite.ga$reported.data[[5000]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)