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

generations.FPS.standard = c(1)
fitData.FPS.standard = data.frame()
for (i in 1:n)
{
  ga = new.GA.env(GA.base.args=new.GA.base.args(max.gen=200), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(selection.type="FPS", elitism=TRUE, elite.size=1), verbose=FALSE)
  generational.ga(ga)
  
  generations.FPS.standard[i] = ga$gen
  fitData.FPS.standard[i,1] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$median
  fitData.FPS.standard[i,2] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$mean
  fitData.FPS.standard[i,3] = ga$reported.data[[length(ga$reported.data)]]@currentGen.results@fitness[[1]]$SD
  fitData.FPS.standard[i,4] = ga$currentGen.results@maxFit@fitness$value
  
  print(paste(i,"Complete"))
  rm(ga)
}

#Test data for selection types
generations.linear55
[1] 200 124 200 157 180  96  75 200  45  99 140 117 200 158 200 103 200 155 125 148 118 200 123 200 118 184  85 200 200 168 200  95  32 170  52  51 131 200 200
[40]  74 200 200  55  81 160  95 200  56  94  44 156 117 162 156  51 190 155 167 200 149  46 200  73 176 125 200 134 200  85 200 118 129  89  55 108 200  46  61
[79] 182 116 200  89 200  77 130  96  91 200 151 200 200 123  57 200 200  32 200  57 200 200

generations.linear60
[1]  67 200  71  36  86 100  55 144  29 117  48 140  83 147  67  27  66 174 200 142 200  50  82
[24]  91  93  80 106 152 104  26 169 113  91  74  75 128  90  61  97  81  58 116 181 134  94  79
[47]  69  82 174  51 170 116 102  63  94  40  83  79  62  49 200  82  93  63  94 111  54 113  82
[70] 195 189 152  30  79  46  52 138 129 200 200 173 200  46  57  70  73  87 118  95  84 129 168
[93]  37  50  60  37  65  62  92  73

generations.linear65
[1] 199  58 125  81  50 200 114  72  83 156  40  48  89 122 100  71  22  53  46  20  32  90  86
[24]  65  42 169  65  25  62  54 142  30  63  47  56  33  19  50  36 136  94  69  61 109  67  30
[47]  59  65  39  41  22 127 102  70  87  41  53  22  71 140 146  66 107  62  33  67  45  66  20
[70]  66  53 119  81  92  25  53  95  94  43 190  41  72  41  52  27  50  52  70  75  88 111  56
[93]  55  99 200  70  47  23 101  85

generations.FPS55
[1]  79  37  68 200 200 200 124  94 164  64  32 174  99  79 200 151 122 162  48
[20] 107 181  32 200 146  53  84 112 200 100  87 109  95  95  81  64 129  91  30
[39] 180 200 168 115 126  39 200  92  52 127  82  79  63 156 121 200  59 200  71
[58] 102  43  99 200 200 127  67 152 153 192 200 200 129  81  49 122 156 134 200
[77]  89 198 122 200  66 101 105 195 192  80  65 118  71 200  74  91  23 101  71
[96] 200 179 122  58 104

fitData.linear55
V1    V2       V3
1   18.0 18.24 2.796535
2   18.0 18.91 4.476865
3   19.0 19.05 3.373620
4   18.0 18.59 3.423316
5   19.0 18.98 3.443571
6   20.0 20.19 3.580799
7   19.0 19.19 3.805087
8   19.0 19.61 3.575718
9   19.0 19.09 3.990127
10  18.0 18.20 3.107502
11  18.0 18.23 3.484033
12  18.0 18.52 4.051387
13  18.0 17.89 2.884634
14  20.0 19.89 3.507265
15  19.0 19.13 3.454334
16  19.0 19.00 3.396374
17  19.0 18.76 3.071932
18  19.5 19.66 3.272845
19  19.0 19.13 3.448481
20  19.0 19.11 3.997209
21  19.0 19.43 3.604865
22  20.5 20.65 4.108073
23  19.0 19.01 3.511870
24  19.0 19.50 4.159691
25  20.0 20.35 3.043041
26  19.5 19.71 3.322087
27  20.0 19.89 2.820900
28  17.0 17.71 3.534234
29  19.0 18.84 3.024212
30  19.0 18.67 3.800731
31  18.0 18.88 3.325081
32  17.0 17.41 3.837863
33  19.0 19.73 3.486931
34  20.0 19.82 3.764212
35  18.0 18.49 3.233333
36  19.5 19.40 3.402317
37  20.0 20.22 3.566341
38  20.0 20.09 2.920132
39  19.0 19.03 3.465428
40  17.0 18.38 4.315300
41  18.0 17.64 3.023544
42  19.0 18.43 3.198027
43  19.0 19.23 3.308559
44  20.0 19.87 3.844595
45  19.0 19.47 3.468342
46  18.0 18.86 3.434878
47  18.0 18.76 3.348813
48  19.0 19.23 3.305505
49  19.0 19.81 3.834176
50  19.0 19.20 3.692745
51  19.0 19.62 3.831765
52  18.0 18.55 4.063573
53  19.0 19.56 3.358451
54  19.0 19.17 3.499076
55  17.0 17.74 3.350320
56  18.0 19.01 3.663897
57  18.0 17.86 3.423094
58  18.0 17.97 3.276654
59  20.0 20.11 3.381157
60  17.0 17.44 3.423980
61  19.0 19.38 3.831765
62  19.0 18.74 3.614281
63  18.0 18.35 3.313197
64  17.0 18.38 3.912632
65  19.0 19.32 3.446210
66  18.0 18.44 3.638515
67  19.0 19.51 3.568932
68  18.5 18.61 3.051395
69  19.0 19.12 3.638403
70  19.0 18.87 3.157979
71  18.0 18.08 3.664407
72  18.0 18.52 3.433289
73  20.0 19.73 3.416700
74  19.0 19.05 3.633111
75  19.0 19.25 3.511525
76  19.0 19.61 3.956570
77  19.0 19.35 4.117896
78  19.0 19.70 3.491693
79  18.0 18.48 3.785886
80  19.0 19.51 3.289085
81  18.0 18.04 3.203281
82  18.0 18.84 4.174768
83  20.0 20.28 3.393636
84  19.0 18.82 3.476850
85  18.0 18.72 3.601571
86  19.0 19.65 3.476835
87  19.0 18.81 3.791791
88  19.0 19.30 2.942204
89  18.0 17.98 3.360435
90  19.0 19.16 3.129269
91  19.0 19.39 3.871405
92  18.0 18.51 4.186305
93  19.0 19.69 3.896891
94  18.0 18.19 3.020787
95  19.0 19.26 3.489146
96  19.0 19.03 4.409482
97  19.0 19.34 2.864693
98  19.0 18.74 3.483351
99  19.0 19.72 3.643397
100 19.0 19.58 3.025748

fitData.linear60
V1    V2       V3
1   22.0 21.90 3.176619
2   22.0 22.16 2.758714
3   21.0 21.18 2.483806
4   20.0 20.33 2.895678
5   22.0 21.75 2.868762
6   22.0 22.37 2.725321
7   21.0 20.86 2.863987
8   22.0 22.09 2.781958
9   21.0 22.05 2.705681
10  21.0 20.82 2.728007
11  21.0 20.96 3.278088
12  21.0 21.50 2.536601
13  22.0 22.21 2.843724
14  22.0 21.67 2.916532
15  21.0 21.04 3.363260
16  22.0 21.63 2.866226
17  21.0 20.90 2.790677
18  21.0 21.77 3.126475
19  21.0 20.94 2.505832
20  21.0 21.17 2.864111
21  21.0 20.85 2.524126
22  20.0 20.22 3.003634
23  22.0 21.94 2.654594
24  21.0 21.29 3.242069
25  21.0 21.27 2.751877
26  22.0 21.48 3.115552
27  22.0 21.76 3.015515
28  22.0 22.32 2.369333
29  20.0 20.79 3.056026
30  21.0 20.82 2.606829
31  21.0 20.93 3.220060
32  20.0 20.53 3.619406
33  22.0 21.91 3.315391
34  22.0 21.88 2.989139
35  21.0 20.91 2.923589
36  21.0 21.29 3.346323
37  22.0 22.27 3.180925
38  21.0 21.43 2.471065
39  22.0 21.83 2.726803
40  22.0 22.17 2.940676
41  21.0 20.95 2.750115
42  22.5 22.53 2.914454
43  20.5 20.71 2.889532
44  20.0 20.47 3.076319
45  20.0 20.81 3.100000
46  21.0 21.33 2.817890
47  21.0 21.02 3.120412
48  21.0 20.63 3.301836
49  21.0 20.96 2.974369
50  21.0 21.59 3.018780
51  22.0 22.12 2.789881
52  22.0 22.10 2.833779
53  20.0 20.66 2.850554
54  21.0 21.06 2.696125
55  22.0 21.79 2.475312
56  21.0 21.68 2.788432
57  20.0 20.37 2.798466
58  21.0 21.16 2.692001
59  21.0 21.08 2.812957
60  21.0 21.08 2.751235
61  20.5 21.02 2.726525
62  22.0 22.19 2.680626
63  21.0 21.27 3.136153
64  22.0 21.31 3.555320
65  21.0 21.23 3.184099
66  21.0 21.51 2.649547
67  21.0 21.01 3.154266
68  20.0 20.83 2.832192
69  22.0 21.79 3.260709
70  21.0 21.44 3.163300
71  21.0 21.00 3.305948
72  22.0 21.45 2.928008
73  22.0 21.86 3.041863
74  22.0 21.48 2.890178
75  21.0 21.19 3.060650
76  23.0 22.28 3.088198
77  21.0 20.68 2.933196
78  21.0 21.35 2.528125
79  20.0 20.35 2.753785
80  21.0 20.95 2.645274
81  22.0 22.46 2.753950
82  20.0 20.66 2.944675
83  21.0 21.36 2.618967
84  21.0 20.53 2.904038
85  21.0 21.17 3.149651
86  22.0 21.50 3.160680
87  21.0 21.39 3.161942
88  21.0 20.83 2.846423
89  21.0 21.12 2.944881
90  20.0 20.28 2.749031
91  21.0 20.65 3.062893
92  20.5 20.84 3.070617
93  22.0 21.59 2.781958
94  22.0 21.63 2.904733
95  21.0 21.38 2.740567
96  22.0 21.64 2.938838
97  21.0 21.31 2.904872
98  21.0 21.08 3.060600
99  22.0 21.76 2.958023
100 22.0 21.67 2.593913

fitData.linear65
V1    V2       V3
1   23.0 22.72 2.678308
2   22.5 22.68 2.538114
3   22.0 22.29 2.559573
4   23.0 22.76 2.336016
5   22.0 22.22 2.281148
6   22.0 22.51 2.949045
7   24.0 23.43 2.230437
8   23.0 23.10 2.837341
9   22.0 22.12 2.648728
10  23.0 23.02 2.399411
11  24.0 24.25 2.512092
12  23.0 23.25 2.194460
13  24.0 23.84 2.377164
14  22.0 22.06 2.473373
15  22.0 22.36 2.389032
16  23.0 22.88 2.543679
17  23.0 23.11 2.573741
18  23.0 23.30 2.380476
19  22.0 22.41 2.340228
20  22.0 22.46 2.479899
21  23.0 23.31 2.268337
22  23.0 22.64 2.560461
23  23.0 22.82 2.694476
24  23.0 23.10 2.100986
25  22.0 22.17 2.697005
26  22.0 22.30 2.560382
27  23.0 22.86 2.490467
28  23.0 23.08 2.703085
29  23.0 22.83 2.518838
30  23.0 22.83 2.445590
31  23.0 23.15 2.157323
32  23.0 23.03 2.508390
33  23.0 22.70 2.717322
34  23.0 23.15 2.858180
35  23.0 22.65 2.804668
36  22.5 22.91 2.832335
37  22.0 22.32 2.733555
38  23.0 22.72 2.292191
39  23.0 22.74 2.285749
40  23.0 22.65 2.705681
41  23.0 23.05 2.409472
42  23.0 23.19 2.452354
43  22.5 22.45 2.455153
44  22.0 21.97 2.217720
45  23.0 22.72 2.749031
46  22.0 21.75 2.185235
47  23.0 23.07 2.207677
48  23.0 22.95 2.479716
49  22.0 22.48 2.683583
50  22.0 22.52 2.471984
51  23.0 22.82 2.536123
52  22.0 22.11 2.335043
53  24.0 23.98 2.478106
54  23.0 23.45 2.459264
55  22.0 22.59 2.449881
56  22.0 22.32 2.436590
57  22.0 22.39 2.440794
58  23.0 23.00 2.146173
59  23.0 23.00 2.681775
60  23.0 22.94 2.411337
61  24.0 23.39 2.685144
62  22.0 22.30 2.451551
63  22.0 22.30 2.367712
64  23.0 23.52 2.414100
65  23.0 23.00 2.490386
66  22.0 22.39 3.087593
67  22.0 22.17 2.864111
68  23.0 22.89 2.654879
69  22.0 22.31 2.393995
70  23.0 22.57 2.712690
71  22.0 22.39 2.457292
72  23.0 22.58 2.474843
73  22.0 22.20 2.300637
74  23.0 23.14 2.662952
75  22.5 22.47 2.422350
76  23.0 23.32 2.242293
77  24.0 23.88 2.417445
78  23.0 23.23 2.684994
79  22.0 22.09 2.301054
80  23.0 22.86 2.326918
81  23.0 22.79 2.625804
82  23.0 22.43 2.507584
83  22.0 22.26 2.443792
84  22.0 21.95 2.289083
85  22.5 22.81 2.549292
86  22.0 22.44 2.379712
87  22.5 22.61 2.852945
88  22.0 22.68 2.473618
89  22.0 22.40 2.305023
90  23.0 22.80 2.269695
91  23.0 23.23 2.246681
92  22.0 22.06 2.432192
93  23.0 23.16 2.740346
94  22.0 22.58 2.425371
95  23.0 23.02 2.278490
96  23.0 23.12 2.450644
97  23.0 22.62 2.755270
98  22.0 22.09 2.554833
99  23.0 22.62 2.710921
100 23.0 22.81 2.254938

fitData.FPS55
V1    V2       V3 V4
1   18.0 18.73 3.735862 30
2   19.0 18.77 3.460761 30
3   18.5 18.54 3.476966 30
4   20.0 20.03 3.195499 29
5   19.0 19.19 3.116249 29
6   19.0 19.09 3.244560 29
7   19.0 19.53 3.471253 30
8   19.0 19.45 3.376613 30
9   18.0 18.88 3.092121 30
10  18.0 18.38 3.765285 30
11  17.0 18.27 4.024809 30
12  20.0 20.51 3.546217 30
13  19.0 19.55 3.385575 30
14  19.0 19.43 3.926741 30
15  19.0 19.82 4.080949 29
16  19.0 19.93 3.459010 30
17  19.0 18.75 2.875796 30
18  19.0 18.91 3.747309 30
19  18.0 18.97 3.599818 30
20  19.0 19.26 3.857617 30
21  19.0 19.26 3.356345 30
22  19.5 19.57 4.465932 30
23  20.0 20.12 3.898407 29
24  19.0 18.92 3.881268 30
25  20.0 19.84 3.448671 30
26  20.0 19.83 3.408412 30
27  19.0 19.37 3.023995 30
28  18.5 19.63 3.989013 29
29  20.0 21.08 3.433878 30
30  18.0 18.51 4.003773 30
31  18.0 18.37 3.246303 30
32  19.0 18.99 4.210365 30
33  19.0 19.70 3.724069 30
34  19.0 18.92 2.956520 30
35  19.0 19.01 3.917057 30
36  19.0 19.39 3.744073 30
37  20.0 20.32 3.951422 30
38  18.0 18.15 3.985770 30
39  18.0 19.16 3.563904 30
40  18.0 18.24 3.018863 29
41  19.0 19.77 4.087070 30
42  19.0 19.33 3.714713 30
43  20.0 19.66 3.795851 30
44  19.0 18.99 3.888587 30
45  19.0 19.21 3.444495 29
46  21.0 20.73 3.366067 30
47  19.0 19.41 3.607218 30
48  20.0 20.23 3.495611 30
49  18.0 19.10 3.543382 30
50  18.5 19.00 3.709121 30
51  20.0 19.65 3.279489 30
52  19.0 19.84 3.886574 30
53  19.5 19.74 3.960971 30
54  20.0 20.05 3.737268 29
55  19.0 19.41 3.384740 30
56  20.0 20.11 3.645656 29
57  18.5 18.88 3.755884 30
58  19.0 19.20 2.863917 30
59  18.0 18.81 3.925300 30
60  19.0 19.85 3.723730 30
61  20.0 20.27 2.905429 29
62  19.0 19.15 3.712863 29
63  20.0 19.55 3.906418 30
64  19.5 19.97 3.775131 30
65  18.0 18.76 3.405640 30
66  18.0 18.29 3.571195 30
67  21.0 21.01 3.233333 30
68  19.0 19.78 3.494816 29
69  19.0 19.53 3.036662 29
70  18.5 18.70 3.236471 30
71  20.0 19.83 3.507726 30
72  19.0 19.77 4.162980 30
73  20.0 19.96 2.957340 30
74  20.0 20.46 3.791058 30
75  18.0 18.61 3.738673 30
76  19.0 18.75 3.079338 28
77  20.0 19.91 3.206984 30
78  20.0 19.65 3.465195 30
79  18.0 18.94 3.561353 30
80  20.0 19.79 3.539945 29
81  19.0 19.02 4.223503 30
82  19.0 19.27 3.057876 30
83  19.0 19.85 3.577073 30
84  19.0 18.86 4.346181 30
85  19.0 18.97 3.082879 30
86  19.0 19.06 3.448964 30
87  19.0 19.79 3.682596 30
88  19.0 19.25 3.560090 30
89  18.0 18.62 3.087217 30
90  19.0 19.45 3.385575 29
91  19.0 19.11 3.348134 30
92  19.0 19.79 3.482409 30
93  18.0 19.19 3.512445 30
94  20.0 19.87 3.103126 30
95  19.0 19.13 3.410190 30
96  18.5 19.13 3.413150 29
97  18.0 18.61 3.673259 30
98  20.0 20.26 3.218068 30
99  19.0 19.14 3.889951 30
100 20.0 19.78 3.023744 30