n=50
generations = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(99), elitism=FALSE), verbose=TRUE)
  generational.ga(ga)

  generations[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(60), elitism=TRUE), encoding.args=new.encoding.args(chr.length=60))
generational.ga(ga)

avgRankTester <- function(pop,selections)
{
  fit=NULL
  for (i in 1:length(pop))
  {
    fit[[i]] = pop[[i]]@fitness$value
  }
  sortOrganisms = data.frame(1:length(pop),fit,0)
  sortOrganisms = sortOrganisms[order(sortOrganisms[2], decreasing = FALSE),]

  sortOrganisms[3] = 1:length(pop)
  sortOrganisms = sortOrganisms[order(sortOrganisms[1], decreasing = FALSE),]
  totalRank = 0
  for (i in 1:length(selections))
  {
    totalRank = totalRank + sortOrganisms[selections[i],3]
  }
  totalRank/length(selections)
}