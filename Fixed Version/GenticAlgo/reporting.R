#Reporting
report <-function(gen, repr.results, goal.reached){
  c(gen, repr.results, goal.reached)
}

base.reporting.fn <- function(pop, mutation, cross)
{
  #TODO Create averages for muta/cross/etc
  results = c(pop, mutation, cross)
  
  results
}

reportAll.reporting.fn <- function(pop, mutation, cross)
{
  results = c(pop, mutation, cross)
  
  results
}

print.report <- function(ga.env)
{
  print(ga.env$reported.data)
}