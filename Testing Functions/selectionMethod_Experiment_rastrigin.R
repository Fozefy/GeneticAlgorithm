rastriginGA=new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(55,100), elitism=TRUE, elite.size=1), verbose=TRUE)

n=100
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

#Test data for selection types
linear 55-60-65 - all 200s

fitData.linear55.rast
V1       V2       V3
1  282.3836 281.4157 35.94404
2  281.0066 283.2804 36.91670
3  272.9536 273.4518 37.60105
4  268.2065 277.2199 49.81387
5  276.1789 279.0693 48.96030
6  276.1504 280.4915 44.55691
7  280.4418 279.6460 39.54148
8  281.5036 280.5813 35.08312
9  274.2577 268.8305 37.94730
10 275.9237 279.4464 46.54202
11 279.3605 276.8825 38.63818
12 285.7170 285.5010 33.98712
13 299.7650 298.5844 46.43035
14 275.5194 277.7853 38.93341
15 279.8152 280.9571 35.99209
16 289.5932 290.0291 33.39020
17 262.2100 260.7750 36.19720
18 282.2766 277.6064 33.35738
19 290.8522 290.3467 38.97525
20 298.2648 288.9445 39.44110
21 270.4633 276.9362 44.38931
22 286.6129 289.2574 42.68791
23 280.9077 286.4969 36.29483
24 294.4951 295.8086 32.09794
25 290.8721 295.7657 39.29698
26 282.9653 279.5521 40.22005
27 275.7502 281.6683 38.02016
28 275.0779 276.7922 32.59949
29 269.8901 268.8629 31.56401
30 272.9882 274.8212 36.95062
31 286.7195 280.5614 35.14745
32 271.1394 274.6804 41.96605
33 263.1910 267.3306 36.53050
34 266.2587 265.7378 36.26531
35 267.1161 271.0791 41.07952
36 280.9648 276.9075 41.29785
37 295.0736 294.8419 39.89513
38 292.8950 290.4737 40.83269
39 275.1235 276.0353 28.04725
40 289.6029 292.6519 40.71352
41 272.5306 276.2573 37.94259
42 271.9893 268.8273 36.55704
43 272.0958 277.8720 34.56702
44 262.5339 272.5068 40.79799
45 283.8395 283.0073 35.85333
46 276.9472 288.8487 41.07886
47 280.6929 280.4988 30.65111
48 269.7870 272.5809 38.55040
49 281.7552 281.4921 33.27965
50 282.6889 278.7196 37.43575

fitData.linear60.rast
V1       V2       V3
1  317.0452 315.8820 30.58094
2  313.0174 312.7295 29.11399
3  312.7770 312.2886 36.46328
4  309.3360 306.8355 28.10728
5  313.2577 311.6212 32.87428
6  305.4894 300.9706 31.79622
7  317.4513 314.1755 32.06929
8  314.4464 313.5367 29.33910
9  312.8351 310.7099 31.01658
10 312.9805 314.3773 29.67223
11 300.9352 302.0125 33.14090
12 309.6850 310.5379 32.92022
13 321.0809 320.4810 32.29984
14 311.2635 315.9216 36.24282
15 316.4335 312.3142 32.16041
16 315.4880 313.8730 29.47977
17 310.4234 312.5726 32.21934
18 303.7109 307.3092 37.98772
19 319.5083 317.4938 28.67608
20 317.7246 317.7596 38.70195
21 301.0920 300.6646 33.45850
22 303.8520 305.8079 32.70936
23 307.4466 309.4540 25.95513
24 303.7598 301.6740 31.98384
25 299.3022 295.6704 32.17958
26 315.7671 317.6297 29.76402
27 310.8652 312.7423 30.72272
28 298.1143 305.9683 34.37170
29 300.6655 301.7125 29.58516
30 315.2154 312.4337 29.84100
31 311.2236 312.1275 31.09951
32 317.9084 316.1398 33.64519
33 308.1554 308.1133 34.94476
34 328.0023 325.2895 32.19360
35 332.7892 329.3170 25.79905
36 299.1726 300.7287 28.01124
37 307.5066 311.2143 31.21526
38 307.7346 309.3054 34.58212
39 329.3856 327.3662 32.25045
40 320.2358 323.0759 28.44348
41 287.3325 289.3940 22.64050
42 325.6120 321.6581 29.01057
43 305.3720 307.1761 34.34742
44 312.3770 316.2003 34.76615
45 315.7797 312.9110 35.78296
46 312.3815 313.4189 30.58972
47 301.4328 308.2581 33.38822
48 313.9251 312.8128 37.38421
49 322.7301 321.7233 30.66201
50 312.9251 309.8232 32.02027

fitData.linear65.rast
V1       V2       V3
1  330.3145 327.3802 25.94302
2  332.8925 331.4977 23.54545
3  338.6600 339.6135 26.45440
4  334.6538 335.2486 29.14420
5  342.8421 344.0986 22.81300
6  340.1477 340.1579 26.27198
7  330.0646 328.1448 28.65202
8  331.0771 326.2625 27.55154
9  322.1674 325.8628 26.16156
10 338.2448 336.7664 27.06309
11 330.9488 328.2948 25.95075
12 325.2526 322.2717 23.20081
13 329.2201 330.9915 25.47961
14 330.5296 327.7981 28.48508
15 340.7565 337.5329 31.20014
16 332.4794 331.9257 28.27617
17 307.7094 311.3947 23.68785
18 324.6123 324.6748 24.49057
19 324.2351 323.6510 25.03917
20 348.2047 343.5859 29.28422
21 319.8628 323.7137 24.75356
22 326.6894 327.4498 27.78776
23 340.1797 337.8232 25.64841
24 330.4913 328.7215 29.05578
25 331.9343 330.2062 33.62743
26 332.7771 334.8740 28.51895
27 338.5153 334.8857 29.19577
28 322.4446 322.9200 29.01937
29 322.4024 319.6301 26.24480
30 335.9051 334.3524 25.50594
31 322.8082 325.6988 29.76021
32 336.6560 335.4202 23.42178
33 327.1841 327.1181 28.70304
34 332.2789 332.2878 26.54722
35 331.0662 332.2270 28.62986
36 330.6419 328.6315 27.53932
37 340.5533 336.4362 24.24270
38 328.9345 324.4332 25.16916
39 322.4254 324.8493 25.59194
40 330.0959 328.4981 27.73285
41 329.8289 328.1651 26.48057
42 330.2853 328.6275 25.15371
43 330.7950 326.6746 26.15222
44 334.0677 329.1575 27.46330
45 309.1785 309.4520 27.66685
46 337.7555 335.8558 24.51987
47 338.7353 338.2109 24.52534
48 334.4275 333.8251 27.15921
49 329.0788 330.0471 19.85050
50 330.1205 327.7343 24.38319

fitData.exp55.rast
V1       V2       V3       V4
1  296.1616 294.8606 42.14627 390.6478
2  294.0465 292.6713 41.88469 390.4884
3  280.9305 281.6561 42.33479 391.9346
4  283.0287 283.1044 31.37708 391.2559
5  285.8523 286.3518 34.80131 389.0024
6  283.3176 280.2202 44.69830 384.7473
7  265.8633 264.9885 39.10113 383.9066
8  285.9045 286.2386 44.67941 390.1491
9  288.5207 289.8660 36.45310 385.6457
10 284.9127 285.3457 40.89758 383.3995
11 272.2997 277.1291 31.55386 377.0756
12 293.2601 295.7381 42.70815 396.2785
13 281.0326 285.4132 43.47032 374.3455
14 280.7197 284.5752 38.04361 387.5227
15 271.8051 271.3555 31.46676 387.1667
16 315.6942 308.8099 42.51074 391.0872
17 284.4394 281.8903 37.89703 375.6254
18 281.2979 283.9378 31.36390 381.4487
19 303.3851 302.0075 40.91871 386.7885
20 296.9875 295.4786 37.35581 389.0752
21 283.9581 280.6150 37.13746 363.4535
22 277.4356 277.7237 31.01782 384.3429
23 270.9376 270.2733 37.67367 389.1916
24 282.8028 285.6758 34.05170 391.8323
25 283.6536 282.9916 35.03102 373.5718
26 281.3032 283.3746 42.06800 389.7924
27 286.8220 286.5116 37.41432 389.2704
28 284.1131 290.5109 42.82347 395.3611
29 297.6287 293.4979 42.86523 396.8248
30 280.9001 280.9164 34.88995 383.1411
31 280.4911 282.5354 40.76164 381.6646
32 281.6869 289.6158 38.78914 392.5032
33 285.7459 288.3061 36.56107 376.7835
34 282.2831 283.1603 39.37436 379.1822
35 296.9872 299.8095 38.14904 388.8228
36 271.3054 268.2672 38.16133 375.0712
37 273.2124 272.9314 37.18807 385.5216
38 289.5561 288.8870 30.77879 389.1232
39 295.6624 295.1185 38.51272 392.0931
40 279.5821 276.7173 29.25975 380.0390
41 292.5898 292.4881 37.82830 383.0318
42 307.1865 300.0638 33.77079 389.2799
43 283.8267 281.1778 37.37358 390.3379
44 279.6785 283.9441 38.19720 393.9834
45 293.6815 292.7519 41.22708 387.0138
46 295.8465 298.3123 34.61364 386.0920
47 279.8142 277.5028 34.08322 381.0455
48 294.2620 291.1083 42.04428 380.2125
49 307.7285 301.3707 40.40951 383.6308
50 287.6143 286.5970 31.81391 383.9037

fitData.exp60.rast
V1       V2       V3       V4
1  309.7885 312.4305 36.30753 384.2178
2  327.9176 325.8458 31.19252 387.0828
3  324.6590 322.1187 29.78598 392.1981
4  329.8994 327.4350 28.33533 396.5717
5  321.2959 320.0802 34.20284 399.2751
6  334.2791 331.0288 27.61140 384.6940
7  302.8211 305.3813 23.18088 380.5816
8  333.7265 326.1645 35.00302 388.9230
9  321.6281 318.6511 36.09390 389.6046
10 328.7100 326.3304 32.38281 392.0323
11 306.1265 304.8336 35.98484 387.7750
12 313.0816 309.4666 34.37972 386.3073
13 329.6915 327.9343 32.30333 398.4345
14 319.6763 318.7975 34.82746 392.3860
15 310.6604 312.8559 28.71639 388.9932
16 325.7162 325.5225 31.52377 391.8899
17 322.5632 319.8632 33.56909 385.1271
18 316.4130 313.6138 33.84758 385.4236
19 334.5489 331.8681 30.82144 392.4376
20 323.4446 324.5611 28.69864 387.0601
21 319.9602 314.3420 32.17062 393.4052
22 317.5118 318.5002 31.37774 384.7955
23 325.5356 319.2395 33.13543 387.3455
24 314.4437 310.6889 35.95986 387.5826
25 321.4408 318.8263 36.73014 390.6649
26 327.9715 321.5275 36.12359 382.6822
27 307.5051 302.3344 36.65020 382.5739
28 323.1056 318.9653 33.98459 390.3737
29 315.8745 314.0923 35.31843 383.6761
30 321.4547 318.3024 32.56217 397.1897
31 321.6415 320.8326 31.73463 383.6862
32 317.6514 317.8240 37.62503 388.6862
33 327.6563 325.8108 35.45704 389.6816
34 313.0721 316.6431 34.18083 395.9039
35 335.9261 329.7961 32.40549 394.0591
36 325.6458 323.2269 30.20765 394.4850
37 308.4073 306.1800 28.77899 376.5123
38 314.6123 316.5177 29.57574 390.4558
39 315.3904 315.8759 31.45786 385.4508
40 327.6637 325.7274 34.57806 391.5472
41 324.1057 320.7798 28.36308 395.4869
42 330.9979 328.2458 34.35719 393.4392
43 318.5305 316.8492 31.02719 388.8515
44 325.2176 321.1497 30.82509 394.6125
45 316.6452 316.9887 32.95333 391.9245
46 319.8659 320.8212 29.06797 387.7831
47 313.5052 313.9419 30.87979 390.0818
48 316.1499 311.1953 31.95258 381.2952
49 328.3782 324.2635 27.34646 396.7535
50 332.4539 326.6289 33.57044 391.4016

fitData.exp65.rast
V1       V2       V3       V4
1  331.5976 332.0859 25.95843 386.1191
2  347.0740 340.6269 31.72457 393.5886
3  348.1308 343.6333 24.92007 393.1571
4  343.2093 341.2307 29.86588 394.1769
5  340.4203 340.7194 27.69825 390.2817
6  344.8218 340.8716 28.61099 392.2036
7  335.4070 334.5468 26.46342 387.8317
8  349.6835 347.9465 25.56848 398.2354
9  339.3357 335.8741 30.97264 394.1418
10 337.2768 334.6608 30.16905 391.5176
11 341.8907 339.4073 30.65919 395.2139
12 340.0368 338.3830 26.62573 395.7635
13 353.2107 350.0753 25.92618 396.5102
14 336.3510 336.3662 32.18137 398.4508
15 341.3484 342.1347 30.07403 391.8570
16 342.3593 340.1231 29.41694 388.3208
17 352.0337 347.7988 23.90172 391.9556
18 347.5475 346.7980 32.47927 398.1739
19 334.1155 330.7228 28.62274 383.6139
20 351.5985 348.8993 25.43696 397.3363
21 342.2557 339.2262 25.27504 390.9380
22 331.7024 333.3536 27.50843 392.7427
23 342.7848 342.2460 25.40655 392.2920
24 340.5190 335.8360 31.58884 399.2485
25 343.7549 341.0758 27.41700 395.9015
26 334.6739 332.9879 29.33457 388.8186
27 332.0734 331.1934 26.69812 388.9110
28 345.7574 338.7346 29.98599 391.3373
29 339.3951 335.4968 27.38200 385.6412
30 333.4067 335.0015 28.87311 391.1183
31 340.2700 339.7789 27.54085 398.1986
32 339.4279 333.8250 29.56164 388.9758
33 347.1876 343.5718 27.04432 397.9428
34 329.5444 329.8675 25.80068 390.4730
35 342.6697 340.5138 28.36833 388.9864
36 334.9215 334.5040 28.55001 396.6897
37 350.0309 347.2859 25.31086 398.5419
38 329.0917 326.7084 27.77158 384.4053
39 345.0058 345.3754 26.06492 395.6805
40 345.7524 345.8162 27.90419 398.2558
41 354.5167 349.8345 27.49764 399.7239
42 347.8460 341.3738 31.12990 398.7464
43 342.3806 337.6673 27.06237 394.1203
44 341.8800 338.3557 25.59119 387.6246
45 350.7673 344.8054 24.26693 391.2332
46 353.6524 348.2361 25.17420 389.1843
47 325.8316 326.4195 30.58367 387.0578
48 342.6199 339.0909 29.71094 393.8408
49 343.0931 343.7664 23.83640 393.7020
50 327.9475 328.0678 29.96090 387.3664

fitData.FPS55.rast
V1       V2       V3       V4
1  293.7859 293.1440 39.74047 384.4763
2  277.4217 278.2582 43.24796 389.4193
3  278.8451 278.4622 45.39565 381.8170
4  280.0210 283.5367 33.43890 382.7099
5  304.8911 298.8702 37.38041 393.2401
6  275.6810 275.3075 42.09509 373.8232
7  272.4940 273.4646 38.45624 387.4371
8  272.5158 281.5191 40.65274 383.3125
9  283.2007 279.8876 33.82184 382.9886
10 290.7747 292.8143 35.54449 382.7404
11 274.8998 276.4374 35.37071 384.6514
12 281.3983 283.0151 38.02729 388.8584
13 281.7811 281.0452 38.55565 387.7027
14 279.5117 285.3554 36.93624 379.0134
15 265.1738 266.4317 40.66572 373.3293
16 270.9830 279.5383 45.47327 386.5098
17 265.7093 272.4917 35.86824 389.8725
18 291.5243 290.2089 43.31985 392.9631
19 275.0292 274.0954 43.10280 384.8208
20 282.2195 284.9471 36.28307 382.1246
21 289.8506 291.1709 36.39268 388.5212
22 265.8712 266.3955 39.30205 376.6136
23 274.9800 277.1682 44.27883 384.9040
24 280.2808 280.5293 38.99717 390.0779
25 279.8239 285.7449 39.72870 387.2222
26 285.0124 278.6910 39.10389 384.1355
27 283.7011 282.8639 39.45953 382.5541
28 284.6494 286.5208 38.61669 383.9285
29 314.4700 310.0410 41.91680 387.5430
30 285.6472 291.1173 37.84084 377.5133
31 283.9206 284.9271 38.12994 382.2465
32 286.1232 282.5155 35.20201 368.8261
33 291.0330 290.5556 43.90764 388.7114
34 279.6881 279.5123 34.14795 378.8290
35 290.3356 291.8702 37.84757 386.9430
36 282.6254 291.9567 40.07046 385.9699
37 297.5844 295.7553 37.60812 383.5215
38 278.7519 281.8533 42.51198 378.5097
39 272.5180 280.8160 40.20218 380.7147
40 263.2126 267.2089 37.36753 379.6192
41 284.1528 284.4844 36.20208 392.0617
42 280.4789 284.5873 38.01385 385.5143
43 288.5259 277.9725 46.47269 387.7601
44 269.3957 269.4178 39.15038 377.0332
45 276.6519 274.4223 31.13476 382.2762
46 293.2845 294.0186 39.34770 382.2221
47 278.3119 279.8767 42.76260 375.6245
48 291.8703 292.2734 33.57910 383.4059
49 289.6209 296.0820 37.59224 385.1905
50 274.0457 274.7810 39.47396 383.2066

fitData.FPS60.rast
V1       V2       V3       V4
1  312.1566 312.9595 26.84969 392.7655
2  300.4269 302.6117 30.72114 390.1133
3  314.8556 314.7652 31.41140 390.8432
4  320.2095 319.6949 31.12816 390.4799
5  322.7120 322.8033 39.38765 398.6089
6  321.8853 319.1564 28.11468 385.0701
7  332.7453 333.6949 30.93385 398.5418
8  307.9741 310.9993 35.57514 384.3023
9  315.6361 318.5496 31.40688 390.8298
10 329.4395 329.2274 33.94034 395.5917
11 298.9754 302.8280 30.58063 382.1583
12 315.8549 310.1263 28.22555 380.1430
13 315.0343 312.2645 27.55251 378.3724
14 310.8934 312.1833 30.43896 385.1866
15 314.8061 313.7086 31.14062 380.9200
16 327.5693 321.2403 34.35296 382.8484
17 318.3798 317.9736 27.78941 381.4334
18 314.8505 312.1103 35.21087 386.9667
19 325.5683 324.3020 29.17828 391.9004
20 317.9041 321.1114 38.23775 395.5249
21 325.6258 321.9697 32.08184 390.0861
22 306.4659 309.4281 30.74214 386.5536
23 321.7707 319.8001 35.31755 391.3743
24 290.4679 293.1124 31.89715 386.9463
25 316.1779 312.9212 34.19069 389.6388
26 310.4064 311.0213 30.47547 389.1456
27 311.0999 311.7112 33.07148 392.3840
28 314.4096 312.7403 33.02972 385.8798
29 318.3601 316.5328 31.24511 382.7829
30 308.5241 309.7937 32.35806 384.4300
31 293.9512 293.5803 27.59823 376.8438
32 303.4378 302.6078 30.73498 379.4766
33 329.2762 325.9950 33.78261 393.5633
34 304.6768 302.3012 28.90126 387.2768
35 317.2328 316.1900 33.58632 393.0940
36 299.3853 300.7293 28.67305 372.8328
37 304.7971 304.5495 25.21937 377.8487
38 309.3222 307.9986 28.17867 385.1695
39 328.6207 330.3754 26.16917 394.0442
40 316.5199 316.7035 28.57846 384.0436
41 302.3667 303.6462 32.09227 389.1053
42 305.2340 308.0194 32.41141 394.4842
43 313.5808 310.9712 27.79906 383.9134
44 311.5723 311.1996 32.68419 387.2938
45 311.0241 314.9496 34.96813 389.5119
46 325.3247 323.0274 33.56070 390.7764
47 319.8980 314.0224 32.69013 380.4017
48 314.8596 315.3872 38.72030 394.4159
49 325.0577 323.0466 25.83850 386.8509
50 309.1162 306.9541 32.68646 386.9528

fitData.FPS65.rast
V1       V2       V3       V4
1  344.2066 343.3951 27.99276 398.8869
2  332.4647 334.5881 29.71256 394.5493
3  334.1276 332.3581 24.31568 395.2238
4  336.6381 338.0357 25.32872 389.6648
5  331.1415 331.4945 28.01790 395.9589
6  331.3970 330.8488 27.49618 392.4204
7  321.1207 322.3853 29.25685 391.8073
8  339.1541 338.8357 24.82576 393.8248
9  329.3189 332.0198 27.17861 386.9746
10 328.5581 329.3771 28.33079 386.6340
11 333.6729 334.8694 28.68040 392.7319
12 339.0316 339.3035 24.75115 395.2178
13 319.3277 321.0113 23.56768 380.8368
14 331.7681 328.3071 28.93665 391.4510
15 340.6061 340.3249 26.74976 397.8980
16 323.4298 322.9738 25.99700 385.7837
17 335.4388 333.8184 25.11340 390.8629
18 336.3190 332.3795 29.79510 387.5840
19 327.3522 325.5449 27.57065 383.5386
20 339.6213 340.1450 23.08537 397.9436
21 343.8577 341.4961 25.67705 391.1362
22 339.4301 341.0639 29.05360 397.3528
23 345.1051 345.5460 27.68231 395.6921
24 342.1841 339.3475 28.14238 392.3712
25 338.3031 336.2987 25.13982 391.8683
26 340.4217 338.5065 26.55017 396.7016
27 330.8550 330.9431 24.79487 384.8472
28 330.2258 326.9031 22.78286 387.6899
29 335.4807 330.5701 27.21048 388.9223
30 331.7288 330.6767 22.44591 387.9636
31 344.9458 343.4233 25.31062 393.4138
32 332.9147 329.8006 29.63583 394.0621
33 325.1131 322.5299 23.30924 387.5672
34 339.0753 338.8407 27.43801 398.9798
35 322.9287 322.7939 30.57270 384.4716
36 334.8226 332.6069 28.19368 391.3271
37 333.2200 330.6808 27.13753 386.4492
38 329.8396 329.7356 26.16056 389.5073
39 343.6947 341.9107 24.17117 389.6324
40 332.9343 333.4641 25.77806 387.6345
41 321.5798 321.5858 28.54148 381.0785
42 332.4097 329.3002 29.37833 390.4919
43 327.1723 330.0089 27.69410 397.3710
44 331.9247 333.4786 25.12384 389.4712
45 330.1325 328.5122 26.92992 388.8956
46 336.7910 335.9713 24.52574 384.4625
47 337.3857 333.7645 27.73684 387.9262
48 320.9228 324.1178 25.48935 387.7265
49 341.0624 338.7451 26.49140 392.2691
50 329.7436 329.4269 25.92407 389.4190

fitData.FPS.standard.rast
V1       V2       V3       V4
1  277.2335 283.3254 44.92877 391.2755
2  277.3875 274.1804 44.05406 379.9435
3  269.8725 272.3474 32.86449 367.3287
4  266.8547 269.2000 39.29569 371.0814
5  275.0793 281.1089 39.85775 382.8322
6  261.8122 265.2267 33.61675 378.6939
7  275.8757 274.5185 30.13864 377.6797
8  274.6694 270.9909 38.37462 390.2136
9  294.6996 290.7121 43.09163 385.2615
10 279.6452 286.3633 50.61025 388.8443
11 290.2284 292.6841 34.49320 377.4743
12 296.6816 300.4514 41.79571 387.7451
13 291.2084 287.5574 40.77131 388.0801
14 276.2940 277.7264 38.42169 386.1682
15 274.5923 275.6286 46.48135 378.7454
16 286.5394 289.6606 43.30183 388.2493
17 287.3602 291.4743 35.39638 374.1075
18 284.4681 281.6838 33.84771 389.1638
19 273.0386 275.3252 41.21840 386.6667
20 266.7032 269.6625 43.59804 374.8212
21 275.1640 279.4285 36.40943 380.6099
22 277.3226 277.3846 32.16827 388.9140
23 288.7638 287.7243 40.53136 377.4440
24 264.6679 268.7819 42.13488 385.7090
25 283.8349 282.7684 36.69038 383.2410
26 279.6757 278.5435 40.60882 390.8382
27 274.0165 278.8772 38.43272 389.9277
28 290.2458 288.7569 37.27062 383.7382
29 304.2891 291.7078 44.02482 383.1346
30 271.1359 276.7150 40.30812 386.3110
31 286.1312 283.0965 39.42986 374.0480
32 274.7453 272.9901 43.28622 378.1159
33 265.1484 268.9414 39.53654 392.1051
34 277.6840 271.8069 41.13019 380.1425
35 273.4051 271.7071 39.65585 386.9151
36 279.4287 280.6626 40.41872 388.1445
37 285.0718 283.6586 38.56213 378.0626
38 265.5330 269.9161 40.71409 382.1195
39 275.7979 276.0785 35.17444 371.4473
40 271.5410 274.6962 35.80794 388.6692
41 271.9968 271.1686 30.55405 393.4615
42 277.9101 282.3643 39.74789 376.9778
43 273.9048 275.3985 38.13691 375.7339
44 274.1856 277.2420 37.91388 384.2524
45 278.5482 279.4010 34.15382 383.5163
46 266.3339 264.8839 32.03747 381.4222
47 297.8281 294.1017 40.95155 390.3831
48 285.2408 280.1407 40.60813 372.1774
49 259.9649 261.3733 35.51954 384.1610
50 262.9296 265.5275 35.64630 381.8452