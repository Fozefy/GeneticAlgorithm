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

tgeom.mean <- function(m=128, p=0.5){
  1/p - m * (1-p)^m / (1 - (1 - p)^m)
}

n=100
generations.linear55 = c(1)
fitData.linear55 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.linear(55,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear55[i] = ga$gen
  fitData.linear55[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear55[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear55[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD

  print(paste(i,"Complete"))
  rm(ga)
}
generations.linear60 = c(1)
fitData.linear60 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.linear(60,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear60[i] = ga$gen
  fitData.linear60[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear60[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear60[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.linear65 = c(1)
fitData.linear65 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.linear(65,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear65[i] = ga$gen
  fitData.linear65[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear65[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear65[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.exp55 = c(1)
fitData.exp55 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/151+.00000457), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp55[i] = ga$gen
  fitData.exp55[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp55[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp55[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp55[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp60 = c(1)
fitData.exp60 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/78+0.00004534), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp60[i] = ga$gen
  fitData.exp60[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp60[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp60[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp60[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp65 = c(1)
fitData.exp65 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/51-.00001942), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp65[i] = ga$gen
  fitData.exp65[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp65[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp65[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp65[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp75 = c(1)
fitData.exp75 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/28+.0005995), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp75[i] = ga$gen
  fitData.exp75[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp75[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp75[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp75[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp85 = c(1)
fitData.exp85 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/16+0.0036982), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp85[i] = ga$gen
  fitData.exp85[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp85[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp85[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp85[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp95 = c(1)
fitData.exp95 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/11+0.0090643), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp95[i] = ga$gen
  fitData.exp95[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp95[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp95[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp95[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS55 = c(1)
fitData.FPS55 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(55), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS55[i] = ga$gen
  fitData.FPS55[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS55[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS55[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS55[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS60 = c(1)
fitData.FPS60 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(60), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS60[i] = ga$gen
  fitData.FPS60[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS60[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS60[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS60[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS65 = c(1)
fitData.FPS65 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(65), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS65[i] = ga$gen
  fitData.FPS65[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS65[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS65[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS65[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.FPS75 = c(1)
fitData.FPS75 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(75), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS75[i] = ga$gen
  fitData.FPS75[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS75[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS75[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS75[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.FPS85 = c(1)
fitData.FPS85 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(85), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS85[i] = ga$gen
  fitData.FPS85[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS85[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS85[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS85[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.FPS95 = c(1)
fitData.FPS95 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(95), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS95[i] = ga$gen
  fitData.FPS95[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS95[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS95[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS95[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.FPS.standard = c(1)
fitData.FPS.standard = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type="fps", elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS.standard[i] = ga$gen
  fitData.FPS.standard[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS.standard[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS.standard[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS.standard[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
