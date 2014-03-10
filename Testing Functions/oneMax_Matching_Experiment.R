graph = gridConstructor(100) #4 connections
coevoSpt.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(), goal=45, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.matching[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevoSpt.matching,file="coevoSpt.matching")

graph = complete.graph(100) #complete connections
coevo.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(), goal=45, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.matching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.matching,file="coevo.matching")

graph = complete.graph(100)
std.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(),goal=45), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.matching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.matching,file="std.matching")

graph = gridConstructor(100)
stdSpatial.matching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(),goal=45), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.matching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.matching,file="stdSpatial.matching")

#More matching

graph = gridConstructor(100) #4 connections
coevoSpt.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(matching=4), goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.extraMatching[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevoSpt.extraMatching,file="coevoSpt.extraMatching")

graph = complete.graph(100) #complete connections
coevo.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(matching=4), goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.extraMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.extraMatching,file="coevo.extraMatching")

graph = complete.graph(100)
std.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(matching=4),goal=90), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.extraMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.extraMatching,file="std.extraMatching")

graph = gridConstructor(100)
stdSpatial.extraMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(matching=4),goal=90), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.extraMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.extraMatching,file="stdSpatial.extraMatching")


#Pure Matching
graph = gridConstructor(100) #4 connections
coevoSpt.pureMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(primary=0,secondary=0,matching=1)), goal=15, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  coevoSpt.pureMatching[i] = ga$gen
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevoSpt.pureMatching,file="coevoSpt.pureMatching")

graph = complete.graph(100) #complete connections
coevo.pureMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withMatching(primary=0,secondary=0,matching=1), goal=15, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.pureMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.pureMatching,file="coevo.pureMatching")

graph = complete.graph(100)
std.pureMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(primary=0,secondary=0,matching=1), goal=15), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.pureMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.pureMatching,file="std.pureMatching")

graph = gridConstructor(100)
stdSpatial.pureMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withMatching(primary=0,secondary=0,matching=1), goal=15), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.pureMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.pureMatching,file="stdSpatial.pureMatching")

#Inner Matching
graph = gridConstructor(100) #4 connections
coevoSpt.InnerMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withInnerMatching(primary=0,secondary=0,matching=1)), goal=15, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
generational.ga(ga)
coevoSpt.InnerMatching[i] = ga$gen
print(paste(i,"Complete"))
rm(ga)
}
save(coevoSpt.InnerMatching,file="coevoSpt.InnerMatching")

graph = complete.graph(100) #complete connections
coevo.InnerMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withInnerMatching(primary=0,secondary=0,matching=1), goal=15, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.InnerMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.InnerMatching,file="coevo.InnerMatching")

graph = complete.graph(100)
std.InnerMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withInnerMatching(primary=0,secondary=0,matching=1), goal=15), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.InnerMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.InnerMatching,file="std.InnerMatching")

graph = gridConstructor(100)
stdSpatial.InnerMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withInnerMatching(primary=0,secondary=0,matching=1), goal=15), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.InnerMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.InnerMatching,file="stdSpatial.InnerMatching")

#Inner Matching Mix
graph = gridConstructor(100) #4 connections
coevoSpt.InnerMatchingMix = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withInnerMatching(primary=1,secondary=1,matching=1)), goal=45, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
generational.ga(ga)
coevoSpt.InnerMatchingMix[i] = ga$gen
print(paste(i,"Complete"))
rm(ga)
}
save(coevoSpt.InnerMatchingMix,file="coevoSpt.InnerMatchingMix")

graph = complete.graph(100) #complete connections
coevo.InnerMatchingMix = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withInnerMatching(primary=1,secondary=1,matching=1), goal=45, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.InnerMatchingMix[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.InnerMatchingMix,file="coevo.InnerMatchingMix")

graph = complete.graph(100)
std.InnerMatchingMix = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withInnerMatching(primary=1,secondary=1,matching=1), goal=45), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.InnerMatchingMix[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.InnerMatchingMix,file="std.InnerMatchingMix")

graph = gridConstructor(100)
stdSpatial.InnerMatchingMix = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withInnerMatching(primary=1,secondary=1,matching=1), goal=45), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.InnerMatchingMix[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.InnerMatchingMix,file="stdSpatial.InnerMatchingMix")

#Anti Matching Mix
graph = gridConstructor(100) #4 connections
coevoSpt.AntiMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withAntiMatching(primary=2,secondary=2,matching=1)), goal=60, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph),encoding.args=new.encoding.args(chr.length=15), verbose=FALSE,reporting.fn=reportNone.report.fn)
generational.ga(ga)
coevoSpt.AntiMatching[i] = ga$gen
print(paste(i,"Complete"))
rm(ga)
}
save(coevoSpt.AntiMatching,file="coevoSpt.AntiMatching")

graph = complete.graph(100) #complete connections
coevo.AntiMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000,numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max.withAntiMatching(primary=2,secondary=2,matching=1), goal=60, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=15),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  coevo.AntiMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(coevo.AntiMatching,file="coevo.AntiMatching")

graph = complete.graph(100)
std.AntiMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withAntiMatching(primary=2,secondary=2,matching=1), goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  std.AntiMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(std.AntiMatching,file="std.AntiMatching")

graph = gridConstructor(100)
stdSpatial.AntiMatching = c(1)
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=5000), fitness.args=new.fitness.args(fitness.fn=onePop.one.max.withAntiMatching(primary=2,secondary=2,matching=1), goal=60), mutation.args = new.mutation.args(prob.mutation=4),xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph), encoding.args=new.encoding.args(chr.length=30),verbose=FALSE,reporting.fn=reportNone.report.fn)
  generational.ga(ga)
  
  stdSpatial.AntiMatching[i] = ga$gen
  
  print(paste(i,"Complete"))
  rm(ga)
}
save(stdSpatial.AntiMatching,file="stdSpatial.AntiMatching")