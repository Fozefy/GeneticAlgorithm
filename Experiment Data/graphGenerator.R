load("elite.one")
load("elite.one.CoevoComp")
load("std.elite.one")
load("stdSpatial.elite.one")
boxplot(std.elite.one,stdSpatial.elite.one,elite.one.CoevoComp,elite.one, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 1 Elite")


load("elite.two")
load("elite.two.CoevoComp")
load("std.elite.two")
load("stdSpatial.elite.two")
boxplot(std.elite.two,stdSpatial.elite.two,elite.two.CoevoComp,elite.two, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 2 Elite")

load("elite.three")
load("elite.three.CoevoComp")
load("std.elite.three")
load("stdSpatial.elite.three")
boxplot(std.elite.three,stdSpatial.elite.three,elite.three.CoevoComp,elite.three, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 3 Elite")

load("elite.five")
load("elite.five.CoevoComp")
load("std.elite.five")
load("stdSpatial.elite.five")
boxplot(std.elite.five,stdSpatial.elite.five,elite.five.CoevoComp,elite.five, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 5 Elite")

load("elite.ten")
load("elite.ten.CoevoComp")
load("std.elite.ten")
load("stdSpatial.elite.ten")
boxplot(std.elite.ten,stdSpatial.elite.ten,elite.ten.CoevoComp,elite.ten, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 10 Elite")

load("elite.full")
load("elite.full.CoevoComp")
load("std.elite.full")
load("stdSpatial.elite.full")
boxplot(std.elite.full,stdSpatial.elite.full,elite.full.CoevoComp,elite.full, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - Full Elite")

load("elite.one.BigGrid")
load("elite.two.BigGrid")
load("elite.three.BigGrid")
load("elite.five.BigGrid")
load("elite.ten.BigGrid")
load("elite.full.BigGrid")
boxplot(elite.one.BigGrid,elite.two.BigGrid,elite.three.BigGrid,elite.five.BigGrid,elite.ten.BigGrid,elite.full.BigGrid, ylab="Generations", names=c("1", "2", "3", "5","10","full"), main="Elitism on Big Grid",outline=FALSE))

boxplot(elite.one.CoevoComp,elite.two.CoevoComp,elite.three.CoevoComp,elite.five.CoevoComp,elite.ten.CoevoComp,elite.full.CoevoComp, ylab="Generations", names=c("1", "2", "3", "5","10","full"), main="Elitism on CoevoComplete",outline=FALSE)
boxplot(elite.one,elite.two,elite.three,elite.five,elite.ten,elite.full, ylab="Generations", names=c("1", "2", "3", "5","10","full"), main="Elitism on Coevo SmallGrid",outline=FALSE)
boxplot(std.elite.one,std.elite.two,std.elite.three,std.elite.five,std.elite.ten, ylab="Generations", names=c("1", "2", "3", "5","10","full"), main="Elitism on Standard",outline=FALSE)

boxplot(elite.one,elite.one.BigGrid,elite.one.CoevoComp,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 1 Elite")
boxplot(elite.two,elite.two.BigGrid,elite.two.CoevoComp,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 2 Elite")
boxplot(elite.three,elite.three.BigGrid,elite.three.CoevoComp,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 3 Elite")
boxplot(elite.five,elite.five.BigGrid,elite.five.CoevoComp,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 5 Elite")
boxplot(elite.ten,elite.ten.BigGrid,elite.ten.CoevoComp,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 10 Elite")
boxplot(elite.full,elite.full.BigGrid,elite.full.CoevoComp,ylab="Generations", names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - Full Elite")

load("complete.predprey")
load("graph4.predprey")
load("graph8.predprey")
boxplot(std.elite.two,stdSpatial.elite.two,elite.two.CoevoComp,elite.two,generations.4graph.predprey,generations.8graph.predprey,generations.complete.predprey, ylab="Generations", names=c("Standard", "Std Sp", "Coev", "Coev Sp","Coevo Comp Sp","Coevo Comp Sp 8graph","Coevo Comp"), main="Comparisons of GA Types - 2 Elite",outline=FALSE)

boxplot(generations.ring4,generations.random4,elite.two,generations.ring8,generations.random8,elite.two.BigGrid,names=c("Ring 4", "Rand 4","Graph 4","Ring 8","Rand 8", "Graph 8"), main="Comparisons of Spatial Types - 2 Elite",outline=FALSE)

boxplot(generations.ring4,generations.random4,elite.two,generations.ring8,generations.random8,elite.two.BigGrid,generations.rand.10pop.4conn,generations.rand.10pop.8conn,names=c("Ring 4", "Rand 4","Graph 4","Ring 8","Rand 8", "Graph 8","rand 4-10 pop","rand 8 - 10 pop"), main="Comparisons of Spatial Types - 2 Elite",outline=FALSE)

