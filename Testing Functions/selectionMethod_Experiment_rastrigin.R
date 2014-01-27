rastriginGA=new.GA.env(GA.base.args=new.GA.base.args(max.gen=200),xover.args = new.xover.args(keepSecondaryParent=FALSE),encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 403.5329), selection.args=new.selection.args(selection.type=rank.selection.linear(55,100), elitism=TRUE, elite.size=1), verbose=TRUE)

n=50
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