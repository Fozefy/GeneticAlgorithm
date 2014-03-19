elites.p1 = c(1)
for (i in 2: length(ga$reported.data))
{
  elites.p1[[i-1]] = ga$reported.data[[i]]@currentGen.results@elite[[1]]$maxFit@index$value
}

elites.p2 = c(1)
for (i in 2: length(ga$reported.data))
{
  elites.p2[[i-1]] = ga$reported.data[[i]]@currentGen.results@elite[[2]]$maxFit@index$value
}

eliteFits.p1 = c(1)
for (i in 2: length(ga$reported.data))
{
  elitesFits.p1[[i-1]] = ga$reported.data[[i]]@currentGen.results@elite[[1]]$maxFit@fitness$value
}
elitesFits.p2 = c(1)
for (i in 2: length(ga$reported.data))
{
  elitesFits.p2[[i-1]] = ga$reported.data[[i]]@currentGen.results@elite[[2]]$maxFit@fitness$value
}