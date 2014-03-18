heatmap(matrix(ga$reported.data[[166]]@currentGen.results@fitness[[1]]$test,nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)

heatmap(matrix(tabulate(elite.locations.one[[5]], nbins=100),nrow=10,byrow=TRUE),Rowv=NA,Colv=NA,symm=TRUE)

testList = NULL
for (i in 1:length(elite.locations))
{
  for(j in 1:length(elite.locations[[i]]))
  {
    testList[[i*5+j]] = elite.locations[[i]][[j]]
  }
}