rastriginGA=new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(55,100), elitism=TRUE, elite.size=1), verbose=TRUE)

n=100
generations.linear51.rast = c(1)
fitData.linear51.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(51,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear51.rast[i] = ga$gen
  fitData.linear51.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear51.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear51.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear51.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.linear51.rast,file="fitData.linear51.rast")

generations.linear53.rast = c(1)
fitData.linear53.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(53,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear53.rast[i] = ga$gen
  fitData.linear53.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear53.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear53.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear53.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.linear53.rast,file="fitData.linear53.rast")

generations.linear55.rast = c(1)
fitData.linear55.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(55,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear55.rast[i] = ga$gen
  fitData.linear55.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear55.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear55.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear55.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.linear60.rast = c(1)
fitData.linear60.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(60,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear60.rast[i] = ga$gen
  fitData.linear60.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear60.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear60.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear60.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.linear65.rast = c(1)
fitData.linear65.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(65,100), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear65.rast[i] = ga$gen
  fitData.linear65.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear65.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear65.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear65.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.exp50.rast = c(1)
fitData.exp50.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/1666.9), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp50.rast[i] = ga$gen
  fitData.exp50.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp50.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp50.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp50.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp50.rast,file="fitData.exp50.rast")

generations.exp51.rast = c(1)
fitData.exp51.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/555.7), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp51.rast[i] = ga$gen
  fitData.exp51.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp51.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp51.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp51.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp51.rast,file="fitData.exp51.rast")

generations.exp53.rast = c(1)
fitData.exp53.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/237.9 + .00000052), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp53.rast[i] = ga$gen
  fitData.exp53.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp53.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp53.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp53.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp53.rast,file="fitData.exp53.rast.extra")

generations.exp55.rast = c(1)
fitData.exp55.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/151+.00000457), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp55.rast[i] = ga$gen
  fitData.exp55.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp55.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp55.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp55.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp60.rast = c(1)
fitData.exp60.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/78+0.00004534), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp60.rast[i] = ga$gen
  fitData.exp60.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp60.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp60.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp60.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp65.rast = c(1)
fitData.exp65.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/51-.00001942), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp65.rast[i] = ga$gen
  fitData.exp65.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp65.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp65.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp65.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp75.rast = c(1)
fitData.exp75.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/28+.0005995), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp75.rast[i] = ga$gen
  fitData.exp75.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp75.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp75.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp75.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp85.rast = c(1)
fitData.exp85.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/16+0.0036982), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp85.rast[i] = ga$gen
  fitData.exp85.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp85.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp85.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp85.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.exp95.rast = c(1)
fitData.exp95.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/11+0.0090643), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp95.rast[i] = ga$gen
  fitData.exp95.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp95.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp95.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp95.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.FPS51.rast = c(1)
fitData.FPS51.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(51), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS51.rast[i] = ga$gen
  fitData.FPS51.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS51.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS51.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS51.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS51.rast,file="fitData.FPS51.rast")

generations.FPS53.rast = c(1)
fitData.FPS53.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(53), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS53.rast[i] = ga$gen
  fitData.FPS53.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS53.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS53.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS53.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
fitData.FPS53.rast=fitData.FPS55.rast
save(fitData.FPS53.rast,file="fitData.FPS53.rast")

generations.FPS55.rast = c(1)
fitData.FPS55.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(55), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS55.rast[i] = ga$gen
  fitData.FPS55.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS55.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS55.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS55.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS60.rast = c(1)
fitData.FPS60.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(60), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS60.rast[i] = ga$gen
  fitData.FPS60.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS60.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS60.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS60.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS65.rast = c(1)
fitData.FPS65.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(65), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS65.rast[i] = ga$gen
  fitData.FPS65.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS65.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS65.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS65.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS75.rast = c(1)
fitData.FPS75.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(75), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS75.rast[i] = ga$gen
  fitData.FPS75.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS75.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS75.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS75.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS85.rast = c(1)
fitData.FPS85.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(85), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS85.rast[i] = ga$gen
  fitData.FPS85.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS85.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS85.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS85.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS95.rast = c(1)
fitData.FPS95.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(95), elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS95.rast[i] = ga$gen
  fitData.FPS95.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS95.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS95.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS95.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.FPS.standard.rast = c(1)
fitData.FPS.standard.rast = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type="fps", elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS.standard.rast[i] = ga$gen
  fitData.FPS.standard.rast[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS.standard.rast[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS.standard.rast[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS.standard.rast[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS.standard.rast,file="fitData.FPS.standard.rast")
#Test data for selection types
linear 55-60-65 - all 200s

