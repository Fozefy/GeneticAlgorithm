p.adjust(c(wilcox.test(coevo.PureAntiMatching,coevo.pureMatching)$p.value,wilcox.test(coevoSpt.PureAntiMatching,std.PureAntiMatching)$p.value,wilcox.test(coevoSpt.PureAntiMatching,stdSpatial.PureAntiMatching)$p.value,wilcox.test(stdSpatial.PureAntiMatching,std.PureAntiMatching)$p.value,wilcox.test(coevo.PureAntiMatching,std.PureAntiMatching)$p.value,wilcox.test(coevo.PureAntiMatching,stdSpatial.PureAntiMatching)$p.value))
p.adjust(c(wilcox.test(elite.two,elite.two.CoevoComp)$p.value,wilcox.test(elite.two,std.elite.two)$p.value,wilcox.test(elite.two,stdSpatial.elite.two)$p.value,wilcox.test(elite.two.CoevoComp,std.elite.two)$p.value,wilcox.test(elite.two.CoevoComp,stdSpatial.elite.two)$p.value,wilcox.test(std.elite.two,stdSpatial.elite.two)$p.value))
load("elite.one")
load("elite.one.CoevoComp")
load("std.elite.one")
load("stdSpatial.elite.one")
boxplot(std.elite.one,stdSpatial.elite.one,elite.one.CoevoComp,elite.one, ylab="Generations",
        names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 1 Elite")

load("elite.two")
load("elite.two.CoevoComp")
load("std.elite.two")
load("stdSpatial.elite.two")
boxplot(std.elite.two,stdSpatial.elite.two,elite.two.CoevoComp,elite.two, ylab="Generations", 
        names=c("Standard", "Std Spt", "Coev", "Coev Spt"), main="Comparisons of GA Types - 2 Elite")

load("elite.three")
load("elite.three.CoevoComp")
load("std.elite.three")
load("stdSpatial.elite.three")
boxplot(std.elite.three,stdSpatial.elite.three,elite.three.CoevoComp,elite.three, ylab="Generations", 
        names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 3 Elite")

load("elite.five")
load("elite.five.CoevoComp")
load("std.elite.five")
load("stdSpatial.elite.five")
boxplot(std.elite.five,stdSpatial.elite.five,elite.five.CoevoComp,elite.five, ylab="Generations", 
        names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 5 Elite")

load("elite.ten")
load("elite.ten.CoevoComp")
load("std.elite.ten")
load("stdSpatial.elite.ten")
boxplot(std.elite.ten,stdSpatial.elite.ten,elite.ten.CoevoComp,elite.ten, ylab="Generations", 
        names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - 10 Elite")

load("elite.full")
load("elite.full.CoevoComp")
load("std.elite.full")
load("stdSpatial.elite.full")
boxplot(std.elite.full,stdSpatial.elite.full,elite.full.CoevoComp,elite.full, ylab="Generations", 
        names=c("Standard", "Std Sp", "Coev", "Coev Sp"), main="Comparisons of GA Types - Full Elite")

load("elite.one.BigGrid")
load("elite.two.BigGrid")
load("elite.three.BigGrid")
load("elite.five.BigGrid")
load("elite.ten.BigGrid")
load("elite.full.BigGrid")
boxplot(elite.one.BigGrid,elite.two.BigGrid,elite.three.BigGrid,elite.five.BigGrid,elite.ten.BigGrid,elite.full.BigGrid, ylab="Generations", 
        names=c("1", "2", "3", "5","10","full"), main="Coevo GA Elitism on Big Grid",outline=FALSE)

#With Large Grid
load("stdSpatial.elite.one.LargeGrid")
load("stdSpatial.elite.two.LargeGrid")
load("stdSpatial.elite.three.LargeGrid")
load("stdSpatial.elite.five.LargeGrid")
load("stdSpatial.elite.ten.LargeGrid")
load("stdSpatial.elite.full.LargeGrid")
boxplot(stdSpatial.elite.one.LargeGrid,stdSpatial.elite.two.LargeGrid,stdSpatial.elite.three.LargeGrid,stdSpatial.elite.five.LargeGrid,stdSpatial.elite.ten.LargeGrid,elite.full.BigGrid, ylab="Generations",
        names=c("1", "2", "3", "5","10","full"), main="Elitism on Standard GA with 8 Grid")

boxplot(elite.one.CoevoComp,elite.two.CoevoComp,elite.three.CoevoComp,elite.five.CoevoComp,elite.ten.CoevoComp,elite.full.CoevoComp, ylab="Generations", xlab="Number of Elites",
        names=c("1", "2", "3", "5","10","full"), main="Elitism on Coevolutionary GA")

boxplot(elite.one,elite.two,elite.three,elite.five,elite.ten,elite.full, ylab="Generations", xlab="Number of Elites",
        names=c("1", "2", "3", "5","10","full"), main="Elitism on Spatial Coevolutonary GA")

boxplot(std.elite.one,std.elite.two,std.elite.three,std.elite.five,std.elite.ten,std.elite.full, ylab="Generations", xlab="Number of Elites",
        names=c("1", "2", "3", "5","10","full"), main="Elitism on Standard GA")

boxplot(stdSpatial.elite.one,stdSpatial.elite.two,stdSpatial.elite.three,stdSpatial.elite.five,stdSpatial.elite.ten,stdSpatial.elite.full, ylab="Generations", xlab="Number of Elites",
        names=c("1", "2", "3", "5","10","full"), main="Elitism on Spatial Standard GA")

#Spatial effects on coevolution one-max
boxplot(elite.one,elite.one.BigGrid,elite.one.CoevoComp,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 1 Elite")
boxplot(elite.two,elite.two.BigGrid,elite.two.CoevoComp,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 2 Elite")
boxplot(elite.three,elite.three.BigGrid,elite.three.CoevoComp,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 3 Elite")
boxplot(elite.five,elite.five.BigGrid,elite.five.CoevoComp,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 5 Elite")
boxplot(elite.ten,elite.ten.BigGrid,elite.ten.CoevoComp,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - 10 Elite")
boxplot(elite.full,elite.full.BigGrid,elite.full.CoevoComp,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Coevo Spatial Effects - Full Elite")

#Spatial effects on standard one-max
boxplot(stdSpatial.elite.one,stdSpatial.elite.one.LargeGrid,std.elite.one,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Standard Spatial Effects - 1 Elite")
boxplot(stdSpatial.elite.two,stdSpatial.elite.two.LargeGrid,std.elite.two,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Standard Spatial Effects - 2 Elite")
boxplot(stdSpatial.elite.three,stdSpatial.elite.three.LargeGrid,std.elite.three,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Standard Spatial Effects - 3 Elite")
boxplot(stdSpatial.elite.five,stdSpatial.elite.five.LargeGrid,std.elite.five,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Standard Spatial Effects - 5 Elite")
boxplot(stdSpatial.elite.ten,stdSpatial.elite.ten.LargeGrid,std.elite.ten,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Standard Spatial Effects - 10 Elite")
boxplot(stdSpatial.elite.full,stdSpatial.elite.full.LargeGrid,std.elite.full,ylab="Generations", 
        names=c("4 Grid", "8 Grid", "Complete"), main="Standard Spatial Effects - Full Elite")

load("PredPrey/complete.predprey")
load("PredPrey/graph4.predprey")
load("PredPrey/graph8.predprey")
load("PredPrey/ring4.predprey")
load("PredPrey/ring8.predprey")
load("PredPrey/rand4.predprey")
load("PredPrey/rand8.predprey")
boxplot(generations.4graph.predprey,generations.8graph.predprey,generations.complete.predprey,generations.ring4.predprey,generations.ring8.predprey,generations.random4.predprey,generations.random8.predprey, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete","4 Ring","8 Ring","Rand 4","Rand 8"), main="Predator Prey Spatial Effects")

boxplot(generations.4graph.predprey,generations.8graph.predprey,generations.complete.predprey, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete"), main="Competitive One-Max\nHost/Parasite")

load("PredPrey/GridFitness/graph4.GridFitness.predprey")
load("PredPrey/GridFitness/graph8.GridFitness.predprey")
load("PredPrey/GridFitness/complete.GridFitness.predprey")
boxplot(graph4.GridFitness.predprey,graph8.GridFitness.predprey,complete.GridFitness.predprey, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete"), main="Competitive One-Max\nFitness on a Grid")

load("PredPrey/GridFitness/graph4.GP.predprey")
load("PredPrey/GridFitness/graph8.GP.predprey")
load("PredPrey/GridFitness/complete.GP.predprey")
boxplot(graph4.GP.predprey,graph8.GP.predprey,complete.GP.predprey, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete"), main="Competitive One-Max\nPurely Competitive")

load("PredPrey/GridFitness/graph4.GridFitnessComp.predprey")
load("PredPrey/GridFitness/graph8.GridFitnessComp.predprey")
load("PredPrey/GridFitness/graph4.GridFitness8.predprey")
load("PredPrey/GridFitness/graph8.GridFitness4.predprey")
load("PredPrey/GridFitness/complete.GridFitness4.predprey")
load("PredPrey/GridFitness/complete.GridFitness8.predprey")
boxplot(graph4.GridFitness.predprey,graph4.GridFitness8.predprey,graph4.GridFitnessComp.predprey,graph8.GridFitness4.predprey,graph8.GridFitness.predprey,graph8.GridFitnessComp.predprey,complete.GridFitness4.predprey,complete.GridFitness8.predprey,complete.GridFitness.predprey, ylab="Generations", 
        names=c("4 Graph","4 Repo, 8 Fit", "4 Repo, Comp Fit", "8 Graph 4 Fit","8 Graph","8 Repo, Comp Fit","Comp Repo, 4 Fit","Comp Repo, 8 Fit","Complete"), main="Predator Prey\nFitness on Grid w/ Split Reproduction")

boxplot(graph4.GridFitness.predprey,graph4.GridFitness8.predprey,graph8.GridFitness4.predprey,graph8.GridFitness.predprey,complete.GridFitness4.predprey,complete.GridFitness8.predprey, ylab="Generations", 
        names=c("4 Graph","4 Repo, 8 Fit", "8 Graph 4 Fit","8 Graph","Comp Repo, 4 Fit","Comp Repo, 8 Fit"), main="Predator Prey\nFitness on Grid w/ Split Reproduction (No Comp)")


load("PredPrey/complete.predprey.NoElite")
load("PredPrey/graph4.predprey.NoElite")
load("PredPrey/graph8.predprey.NoElite")
boxplot(graph4.predprey.NoElite,graph8.predprey.NoElite,complete.predprey.NoElite, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete"), main="Host Parasite\nNo Elite")

load("PredPrey/complete.predprey.10Elite")
load("PredPrey/graph4.predprey.10Elite")
load("PredPrey/graph8.predprey.10Elite")
boxplot(graph4.predprey.10Elite,graph8.predprey.10Elite,complete.predprey.10Elite, ylab="Generations",
        names=c("4 Graph","8 Graph","Complete"), main="Host Parasite\n10 Elite")

load("PredPrey/complete.predprey.FullElite")
load("PredPrey/graph4.predprey.FullElite")
load("PredPrey/graph8.predprey.FullElite")
boxplot(graph4.predprey.FullElite,graph8.predprey.FullElite,complete.predprey.FullElite, ylab="Generations",
        names=c("4 Graph","8 Graph","Complete"), main="Host Parasite\nFull Elite")

load("PredPrey/complete.predprey")
load("PredPrey/graph4.predprey")
load("PredPrey/graph8.predprey")
boxplot(generations.4graph.predprey,generations.8graph.predprey,generations.complete.predprey, ylab="Generations",
        names=c("4 Graph","8 Graph","Complete"), main="Host Parasite\n2 Elite")

boxplot(graph4.predprey.NoElite,generations.4graph.predprey,graph4.predprey.10Elite,graph4.predprey.FullElite, ylab="Generations", xlab="Elite",
        names=c("0","2","10","Full"), main="Host Parasite\n4 Graph")

boxplot(graph8.predprey.NoElite,generations.8graph.predprey,graph8.predprey.10Elite,graph4.predprey.FullElite, ylab="Generations", xlab="Elite",
        names=c("0","2","10","Full"), main="Host Parasite\n8 Graph")

boxplot(complete.predprey.NoElite,generations.complete.predprey,complete.predprey.10Elite,complete.predprey.FullElite, ylab="Generations", xlab="Elite",
        names=c("0","2","10","Full"), main="Host Parasite\nComplete")

load("PredPrey/complete.GridFitness.NoElite.predprey")
load("PredPrey/graph4.GridFitness.NoElite.predprey")
load("PredPrey/graph8.GridFitness.NoElite.predprey")
boxplot(graph4.GridFitness.NoElite.predprey,graph8.GridFitness.NoElite.predprey,complete.GridFitness.NoElite.predprey, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete"), main="Predator Prey - Grid Fitness\nNo Elite")


load("PredPrey/complete.predprey.InnerMatch")
load("PredPrey/graph4.predprey.InnerMatch")
load("PredPrey/graph8.predprey.InnerMatch")
boxplot(graph4.predprey.InnerMatch,graph8.predprey.InnerMatch,complete.predprey.InnerMatch, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete"), main="Predator Prey - Inner Match")

load("PredPrey/GP Style/graph4.GP.predprey")
load("PredPrey/GP Style/graph8.GP.predprey")
load("PredPrey/GP Style/complete.GP.predprey")
boxplot(graph4.GP.predprey,graph8.GP.predprey,complete.GP.predprey, ylab="Generations", 
        names=c("4 Graph","8 Graph","Complete"), main="GP Host Parasite")




load("coevo.rand4.2elite")
load("coevo.rand8.2elite")
load("rand.2pop.4conn")
load("rand.2pop.8conn")
load("rand.4pop.4conn")
load("rand.4pop.8conn")
load("rand.10pop.4conn")
load("rand.10pop.8conn")
boxplot(generations.random4,generations.random8,generations.rand.2pop.4conn,generations.rand.2pop.8conn,generations.rand.4pop.4conn,generations.rand.4pop.8conn,generations.rand.10pop.4conn,generations.rand.10pop.8conn,ylab="Generations",
        names=c("1P 4C","1P 8C","2P 4C","2P 8C","4P 4C","4P 8C","10P 4C","10P 8C"), main="Random Split Pop")
boxplot(generations.rand.2pop.4conn,generations.rand.2pop.8conn,generations.rand.4pop.4conn,generations.rand.4pop.8conn,generations.rand.10pop.4conn,generations.rand.10pop.8conn,elite.two.CoevoComp,elite.two,elite.two.BigGrid,ylab="Generations",
        names=c("2P 4C","2P 8C","4P 4C","4P 8C","10P 4C","10P 8C","Complete","4Graph","8Graph"), main="Random Split Pop")

load("coevo.ring2")
load("coevo.ring4.2elite")
load("coevo.ring8.2elite")
boxplot(coevo.ring2,generations.ring4,generations.random4,elite.two,generations.ring8,generations.random8,elite.two.BigGrid, elite.two.CoevoComp,
        names=c("Ring 2","Ring 4", "Rand 4","Grid 4","Ring 8","Rand 8", "Grid 8", "Complete"), main="Comparisons of Spatial Types\nCoevolutionary GA ")

load("std.ring2")
load("std.Ring4.2elite")
load("std.Ring8.2elite")
load("std.Rand4.2elite")
load("std.Rand8.2elite")
boxplot(std.ring2,std.Ring4.2elite,std.Rand4.2elite,stdSpatial.elite.two,std.Ring8.2elite,std.Rand4.2elite,stdSpatial.elite.two.LargeGrid,std.elite.two,
        names=c("Ring 2","Ring 4","Rand 4","Grid 4","Ring 8","Rand 8","Grid 8","Complete"), main="Comparisons of Spatial Types\nStandard GA")

load("Matching Data/coevo.matching")
load("Matching Data/stdSpatial.matching")
load("Matching Data/std.matching")
load("Matching Data/coevoSpt.matching")
boxplot(std.matching,stdSpatial.matching,coevo.matching,coevoSpt.matching, ylab="Generations",
        names=c("Standard","Standard Spatial","Co-evolution","Co-evolution Spatial"), main="One-Max Matching")

load("Matching Data/coevo.extraMatching")
load("Matching Data/stdSpatial.extraMatching")
load("Matching Data/std.extraMatching")
load("Matching Data/coevoSpt.extraMatching")
boxplot(std.extraMatching, stdSpatial.extraMatching, coevo.extraMatching, coevoSpt.extraMatching, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="GA Types - Extra Matching")

load("Matching Data/coevo.InnerMatchingMix")
load("Matching Data/stdSpatial.InnerMatchingMix")
load("Matching Data/std.InnerMatchingMix")
load("Matching Data/coevoSpt.InnerMatchingMix")
boxplot(std.InnerMatchingMix, stdSpatial.InnerMatchingMix, coevo.InnerMatchingMix, coevoSpt.InnerMatchingMix, ylab="Generations",
        names=c("Standard","Standard Spatial","Co-evolution","Co-evolution Spatial"), main="One-Max Inner Matching")

load("Matching Data/coevo.PureInnerMatching")
load("Matching Data/stdSpatial.PureInnerMatching")
load("Matching Data/std.PureInnerMatching")
load("Matching Data/coevoSpt.PureInnerMatching")
boxplot(std.PureInnerMatching, stdSpatial.PureInnerMatching, coevo.PureInnerMatching, coevoSpt.PureInnerMatching, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="GA Types - Only Inner Matching")

load("Matching Data/coevo.pureMatching")
load("Matching Data/stdSpatial.pureMatching")
load("Matching Data/std.pureMatching")
load("Matching Data/coevoSpt.pureMatching")
boxplot(std.pureMatching, stdSpatial.pureMatching, coevo.pureMatching, coevoSpt.pureMatching, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="GA Types - Only Matching")

load("Matching Data/coevo.ExtraInnerMatching")
load("Matching Data/stdSpatial.ExtraInnerMatching")
load("Matching Data/std.ExtraInnerMatching")
load("Matching Data/coevoSpt.ExtraInnerMatching")
boxplot(std.ExtraInnerMatching,stdSpatial.ExtraInnerMatching,coevo.ExtraInnerMatching, coevoSpt.ExtraInnerMatching, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="GA Types - Extra Inner Matching")

load("InnerMatching Elitism/coevo.ExtraInnerMatching.FullElite")
load("InnerMatching Elitism/stdSpatial.ExtraInnerMatching.FullElite")
load("InnerMatching Elitism/std.ExtraInnerMatching.FullElite")
load("InnerMatching Elitism/coevoSpt.ExtraInnerMatching.FullElite")
boxplot(std.ExtraInnerMatching.FullElite,stdSpatial.ExtraInnerMatching.FullElite,coevo.ExtraInnerMatching.FullElite, coevoSpt.ExtraInnerMatching.FullElite, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="Inner Matching\n100 Elite")

load("InnerMatching Elitism/coevo.ExtraInnerMatching.50Elite")
load("InnerMatching Elitism/stdSpatial.ExtraInnerMatching.50Elite")
load("InnerMatching Elitism/std.ExtraInnerMatching.50Elite")
load("InnerMatching Elitism/coevoSpt.ExtraInnerMatching.50Elite")
boxplot(std.ExtraInnerMatching.50Elite,stdSpatial.ExtraInnerMatching.50Elite,coevo.ExtraInnerMatching.50Elite, coevoSpt.ExtraInnerMatching.50Elite, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="Inner Matching\n50 Elite")

load("InnerMatching Elitism/coevo.ExtraInnerMatching.10Elite")
load("InnerMatching Elitism/stdSpatial.ExtraInnerMatching.10Elite")
load("InnerMatching Elitism/std.ExtraInnerMatching.10Elite")
load("InnerMatching Elitism/coevoSpt.ExtraInnerMatching.10Elite")
boxplot(std.ExtraInnerMatching.10Elite,stdSpatial.ExtraInnerMatching.10Elite,coevo.ExtraInnerMatching.10Elite, coevoSpt.ExtraInnerMatching.10Elite, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="Inner Matching\n10 Elite")

load("InnerMatching Elitism/coevo.ExtraInnerMatching.2Elite")
load("InnerMatching Elitism/stdSpatial.ExtraInnerMatching.2Elite")
load("InnerMatching Elitism/std.ExtraInnerMatching.2Elite")
load("InnerMatching Elitism/coevoSpt.ExtraInnerMatching.2Elite")
boxplot(std.ExtraInnerMatching.2Elite,stdSpatial.ExtraInnerMatching.2Elite,coevo.ExtraInnerMatching.2Elite, coevoSpt.ExtraInnerMatching.2Elite, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="Inner Matching\n2 Elite")

load("InnerMatching Elitism/coevo.ExtraInnerMatching.NoElite")
load("InnerMatching Elitism/stdSpatial.ExtraInnerMatching.NoElite")
load("InnerMatching Elitism/std.ExtraInnerMatching.NoElite")
load("InnerMatching Elitism/coevoSpt.ExtraInnerMatching.NoElite")

boxplot(std.ExtraInnerMatching.NoElite,std.ExtraInnerMatching.2Elite,std.ExtraInnerMatching.10Elite,std.ExtraInnerMatching.50Elite, std.ExtraInnerMatching.FullElite, ylab="Generations", xlab="Number of Elites",
        names=c("0","2","10","50","100"), main="Elitism on Standard GA\nOne-Max-Matching")
boxplot(stdSpatial.ExtraInnerMatching.NoElite,stdSpatial.ExtraInnerMatching.2Elite,stdSpatial.ExtraInnerMatching.10Elite,stdSpatial.ExtraInnerMatching.50Elite, stdSpatial.ExtraInnerMatching.FullElite, ylab="Generations", xlab="Number of Elites",
        names=c("0","2","10","50","100"), main="Elitism on Spatial Standard GA\nOne-Max-Matching")
boxplot(coevo.ExtraInnerMatching.NoElite,coevo.ExtraInnerMatching.2Elite,coevo.ExtraInnerMatching.10Elite,coevo.ExtraInnerMatching.50Elite, coevo.ExtraInnerMatching.FullElite, ylab="Generations", xlab="Number of Elites",
        names=c("0","2","10","50","100"), main="Elitism on Coevolutionary GA\nOne-Max-Matching")
boxplot(coevoSpt.ExtraInnerMatching.NoElite,coevoSpt.ExtraInnerMatching.2Elite,coevoSpt.ExtraInnerMatching.10Elite,coevoSpt.ExtraInnerMatching.50Elite, coevoSpt.ExtraInnerMatching.FullElite, ylab="Generations", xlab="Number of Elites",
        names=c("0","2","10","50","100"), main="Elitism on Spatial Coevo GA\nOne-Max-Matching")



load("Matching Data/AntiMatching/coevo.AntiMatching")
load("Matching Data/AntiMatching/stdSpatial.AntiMatching")
load("Matching Data/AntiMatching/std.AntiMatching")
load("Matching Data/AntiMatching/coevoSpt.AntiMatching")
boxplot(std.AntiMatching, stdSpatial.AntiMatching, coevo.AntiMatching, coevoSpt.AntiMatching, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="GA Types - Anti Matching")

load("Matching Data/AntiMatching/coevo.PureAntiMatching")
load("Matching Data/AntiMatching/stdSpatial.PureAntiMatching")
load("Matching Data/AntiMatching/std.PureAntiMatching")
load("Matching Data/AntiMatching/coevoSpt.PureAntiMatching")
boxplot(std.PureAntiMatching, stdSpatial.PureAntiMatching, coevo.PureAntiMatching, coevoSpt.PureAntiMatching, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="GA Types - Pure Anti Matching")

load("RoyalRoad/coevo.RoyalRoad")
load("RoyalRoad/stdSpatial.RoyalRoad")
load("RoyalRoad/std.RoyalRoad")
load("RoyalRoad/coevoSpt.RoyalRoad")
boxplot(std.RoyalRoad,stdSpatial.RoyalRoad,coevo.RoyalRoad, coevoSpt.RoyalRoad, ylab="Generations",
        names=c("Standard","Standard Spatial","Co-evolution","Co-evolution Spatial"), main="Cooperative Royal Road")

load("RoyalRoad/coevo.BonusRoyalRoad")
load("RoyalRoad/stdSpatial.BonusRoyalRoad")
load("RoyalRoad/std.BonusRoyalRoad")
load("RoyalRoad/coevoSpt.BonusRoyalRoad")
boxplot(std.BonusRoyalRoad,stdSpatial.BonusRoyalRoad,coevo.BonusRoyalRoad, coevoSpt.BonusRoyalRoad, ylab="Generations",
        names=c("Standard","Std Spt","Coevo","Coevo Spt"), main="GA Types - Royal Road as Bonus")

load("Hard Elite/hard.elite.one")
load("Hard Elite/hard.elite.two")
load("Hard Elite/hard.elite.ten")
boxplot(hard.elite.one,hard.elite.two,hard.elite.ten, ylab="Generations", xlab="Elite",
        names=c("1","2","10"), main="Coevo Spatial - Hard Elite")

load("Hard Elite/hard.elite.one.CoevoComp")
load("Hard Elite/hard.elite.two.CoevoComp")
load("Hard Elite/hard.elite.three.CoevoComp")
load("Hard Elite/hard.elite.ten.CoevoComp") #Missing this value
boxplot(hard.elite.one.CoevoComp,hard.elite.two.CoevoComp,hard.elite.three.CoevoComp,ylab="Generations", xlab="Elite",
        names=c("1","2","3"), main="Coevo - Hard Elite")

load("Hard Elite/hard.std.elite.one")
load("Hard Elite/hard.std.elite.two")
load("Hard Elite/hard.std.elite.ten")
boxplot(hard.std.elite.one,hard.std.elite.two,hard.std.elite.ten, ylab="Generations", xlab="Elite",
        names=c("1","2","10"), main="Standard - Hard Elite")

load("Hard Elite/hard.stdSpatial.elite.one")
load("Hard Elite/hard.stdSpatial.elite.two")
load("Hard Elite/hard.stdSpatial.elite.ten")
boxplot(hard.stdSpatial.elite.one,hard.stdSpatial.elite.two,hard.stdSpatial.elite.ten, ylab="Generations",xlab="Elite",
        names=c("1","2","10"), main="Standard Spatial - Hard Elite")

load("Hard Elite/hard.stdSpatial.elite.two.LargeGrid")
load("Hard Elite/hard.stdSpatial.elite.full.LargeGrid")
boxplot(hard.stdSpatial.elite.one,hard.stdSpatial.elite.two,hard.stdSpatial.elite.ten, ylab="Generations",xlab="Elite",
        names=c("1","2","10"), main="Standard Large Spatial - Hard Elite")

load("
