n=20
generations55 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(55), elitism=FALSE), verbose=FALSE)
  generational.ga(ga)

  generations55[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations60 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(60), elitism=FALSE), verbose=FALSE)
  generational.ga(ga)
  
  generations60[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations65 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(65), elitism=FALSE), verbose=FALSE)
  generational.ga(ga)
  
  generations65[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations70 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(70), elitism=FALSE), verbose=FALSE)
  generational.ga(ga)
  
  generations70[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations75 = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(75), elitism=FALSE), verbose=FALSE)
  generational.ga(ga)
  
  generations75[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations55=c(500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500)
generations60=c(500,500,500,500,500,500,500,500,500,500,500,500,500,500,369,500,500,500,500,500)
generations65=c(500,500,500,500,500,500,500,500,500,500,500,313,26,500,500,500,72,500,500,500)
generations70=c(500,107,436,500,116,500,500,500,496,161,500,500,500,150,500,126,233,500,500,500)
generations75=c(66,58,385,26,280,53,214,254,179,67,129,147,177,28,44,245,115,98,113,35)

generationsEXP = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/30), elitism=FALSE), verbose=FALSE)
  generational.ga(ga)
  
  generationsEXP[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}

ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(60), elitism=TRUE), encoding.args=new.encoding.args(chr.length=60))
generational.ga(ga)


generations65withElite.FPS = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(65), elitism=TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations65withElite.FPS[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations65withElite.linearRank = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=500), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.linear(65,100), elitism=TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations65withElite.linearRank[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
generations65withElite.FPS=c(16,36,38,34,31,47,67,72,39,37,56,23,30,45,59,44,37,29,64,48,57,44,53,24,41,71,45,31,76,32,86,48,53,20,23,36,41,72,38,45,29,36,41,41,37,37,70,20,66,27)
generations65withElite.linearRank=c(51,41,70,14,55,37,30,51,35,51,58,67,31,32,31,50,29,52,66,59,59,50,38,48,48,21,30,36,131,39,68,54,23,46,57,86,32,63,37,29,134,29,23,31,40,62,47,41,28,35)


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