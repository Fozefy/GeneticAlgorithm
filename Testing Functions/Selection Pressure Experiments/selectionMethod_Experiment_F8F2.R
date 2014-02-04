F8F2GA=new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.linear(55,100), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)

n=50
#linear 51
generations.linear51.F8F2 = c(1)
fitData.linear51.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.linear(51,100), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear51.F8F2[i] = ga$gen
  fitData.linear51.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear51.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear51.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear51.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.linear51.F8F2,file="fitData.linear51.F8F2")

generations.linear53.F8F2 = c(1)
fitData.linear53.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.linear(53,100), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear53.F8F2[i] = ga$gen
  fitData.linear53.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear53.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear53.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear53.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.linear53.F8F2,file="fitData.linear53.F8F2")

generations.linear55.F8F2 = c(1)
fitData.linear55.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.linear(55,100), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear55.F8F2[i] = ga$gen
  fitData.linear55.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear55.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear55.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear55.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.linear60.F8F2 = c(1)
fitData.linear60.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.linear(60,100), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear60.F8F2[i] = ga$gen
  fitData.linear60.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear60.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear60.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear60.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
generations.linear65.F8F2 = c(1)
fitData.linear65.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.linear(65,100), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.linear65.F8F2[i] = ga$gen
  fitData.linear65.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.linear65.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.linear65.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.linear65.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}

generations.exp50.F8F2 = c(1)
fitData.exp50.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/1666.9), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp50.F8F2[i] = ga$gen
  fitData.exp50.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp50.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp50.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp50.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp50.F8F2,file="fitData.exp53.F8F2")

generations.exp51.F8F2 = c(1)
fitData.exp51.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/555.7), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp51.F8F2[i] = ga$gen
  fitData.exp51.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp51.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp51.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp51.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp51.F8F2,file="fitData.exp51.F8F2")


generations.exp53.F8F2 = c(1)
fitData.exp53.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/237.9 + .00000052), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp53.F8F2[i] = ga$gen
  fitData.exp53.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp53.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp53.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp53.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp53.F8F2,file="fitData.exp53.F8F2")

generations.exp55.F8F2 = c(1)
fitData.exp55.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/151+.00000457), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp55.F8F2[i] = ga$gen
  fitData.exp55.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp55.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp55.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp55.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp55.F8F2,file="fitData.exp55.F8F2")

generations.exp60.F8F2 = c(1)
fitData.exp60.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/78+0.00004534), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp60.F8F2[i] = ga$gen
  fitData.exp60.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp60.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp60.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp60.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp60.F8F2,file="fitData.exp60.F8F2")

generations.exp65.F8F2 = c(1)
fitData.exp65.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/51-.00001942), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp65.F8F2[i] = ga$gen
  fitData.exp65.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp65.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp65.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp65.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp65.F8F2,file="fitData.exp65.F8F2")

generations.exp75.F8F2 = c(1)
fitData.exp75.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/28+.0005995), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp75.F8F2[i] = ga$gen
  fitData.exp75.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp75.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp75.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp75.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp75.F8F2,file="fitData.exp75.F8F2")

generations.exp85.F8F2 = c(1)
fitData.exp85.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/16+0.0036982), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp85.F8F2[i] = ga$gen
  fitData.exp85.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp85.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp85.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp85.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp85.F8F2,file="fitData.exp85.F8F2")

generations.exp95.F8F2 = c(1)
fitData.exp95.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=rank.selection.withExp(1/11+0.0090643), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.exp95.F8F2[i] = ga$gen
  fitData.exp95.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.exp95.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.exp95.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.exp95.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.exp95.F8F2,file="fitData.exp95.F8F2")

generations.FPS51.F8F2 = c(1)
fitData.FPS51.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(51), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS51.F8F2[i] = ga$gen
  fitData.FPS51.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS51.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS51.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS51.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS55.F8F2,file="fitData.FPS51.F8F2")

generations.FPS53F8F2 = c(1)
fitData.FPS53.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(53), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS53F8F2[i] = ga$gen
  fitData.FPS53.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS53.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS53.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS53.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS53.F8F2,file="fitData.FPS53.F8F2")

generations.FPS55.F8F2 = c(1)
fitData.FPS55.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(55), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS55.F8F2[i] = ga$gen
  fitData.FPS55.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS55.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS55.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS55.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS55.F8F2,file="fitData.FPS55.F8F2")

generations.FPS60.F8F2 = c(1)
fitData.FPS60.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(60), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS60.F8F2[i] = ga$gen
  fitData.FPS60.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS60.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS60.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS60.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS60.F8F2,file="fitData.FPS60.F8F2")

generations.FPS65.F8F2 = c(1)
fitData.FPS65.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(65), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS65.F8F2[i] = ga$gen
  fitData.FPS65.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS65.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS65.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS65.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS65.F8F2,file="fitData.FPS65.F8F2")

generations.FPS75.F8F2 = c(1)
fitData.FPS75.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(75), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS75.F8F2[i] = ga$gen
  fitData.FPS75.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS75.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS75.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS75.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS75.F8F2,file="fitData.FPS75.F8F2")

generations.FPS85.F8F2 = c(1)
fitData.FPS85.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(85), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS85.F8F2[i] = ga$gen
  fitData.FPS85.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS85.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS85.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS85.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS85.F8F2,file="fitData.FPS85.F8F2")

generations.FPS95.F8F2 = c(1)
fitData.FPS95.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type=setup.fitnessProportional.withFitScaling(95), elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS95.F8F2[i] = ga$gen
  fitData.FPS95.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS95.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS95.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS95.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS95.F8F2,file="fitData.FPS95.F8F2")

generations.FPS.standard.F8F2 = c(1)
fitData.FPS.standard.F8F2 = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 783.8844), selection.args=new.selection.args(selection.type="fps", elitism=TRUE, elite.size=1,maximizing = TRUE), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS.standard.F8F2[i] = ga$gen
  fitData.FPS.standard.F8F2[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS.standard.F8F2[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS.standard.F8F2[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS.standard.F8F2[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(fitData.FPS.standard.F8F2,file="fitData.FPS.standard.F8F2")

#Test data for selection types
