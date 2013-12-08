chr0 <- new("chromosome", 8)
ga.print(chr0)
ga.print(chr0, gene.sep="")

ga.env1 <- new.GA.env()
chr1 <- new.chromosome(ga.env1)
ga.print(chr1, gene.sep="")

ga.env2 <- new.GA.env(encoding.args = new.encoding.args(chr.length = 8, 
								chr.encode.type = "symbolic", 
								gene.alphabet = c("a", "b", "c")))
chr2 <- new.chromosome(ga.env2)
ga.print(chr2, gene.sep="")
ga.print(chr2[3:5], gene.sep="")
chr2a <- duplicate(chr2)
chr2a[3:5] <- c("d", "e", "f")
ga.print(chr2, gene.sep="")
ga.print(chr2a, gene.sep="")

pop1 <- new("population", ga.env1)
ga.print(pop1)
evaluate(pop1, one.max.fn)
ga.print(pop1)

pop2 <- new("population", ga.env2)
ga.print(pop2, gene.sep = "")
genes(pop2[[98]])
ga.print(pop2[95:100], gene.sep="")

pop2a <- new.population(ga.env2)

ga.env3 <- new.GA.env(pop.size = 20,
								encoding.args = new.encoding.args(chr.encode.type = "dist", chr.length = 10, rdist = rbinom, rdist.args = list(10, 0.3)))
pop3 <- new("population", ga.env3)
ga.print(pop3, gene.sep=" ")
evaluate(pop3, one.max.fn)
ga.print(pop3, gene.sep=" ")


pop4 <- duplicate(pop1)
ga.print(pop4, gene.sep = "", genes.delimiters = "<>")
evaluate(pop4, new.fitness.fn(finite.min.fn, 1))
ga.print(pop4, gene.sep = "", genes.delimiters = "<>")
fitness(pop1[[97]])
fitness(pop4[[97]])

fitness(pop4[10:20])

pop5 <- duplicate(pop1)
pop5[[4]] <- new.chromosome(ga.env1)
ga.print(pop1[[4]])
ga.print(pop5[[4]])
evaluate(pop5, new.fitness.fn(finite.min.fn, 1))
fitness(pop1) + fitness(pop5)

chr2a <- new.chromosome(ga.env2)
chr2b <- new.chromosome(ga.env2)
chr2c <- duplicate(chr2a)
chr2d <- duplicate(chr2b)
swap.genes(chr2c, chr2d, c(T, T, F, F, T, T, F, F))
ga.print(chr2a)
ga.print(chr2b)
ga.print(chr2c)
ga.print(chr2d)

size(pop1)
size(pop3)

### testing mutation
mutationEnv1<- new.GA.env(pop.size = 10, 
                   encoding.args = new.encoding.args(chr.length = 8,chr.encode.type = "binary"),
                   mutation.args = new.mutation.args(mutation.type = "binary", prob.mutation = 0.2))
mutationEnv2 <- new.GA.env(pop.size = 10, 
                   encoding.args = new.encoding.args(chr.length = 8,chr.encode.type = "binary"),
                   mutation.args = new.mutation.args(mutation.type = "binary", prob.mutation = 1))
mutPop1 <- new("population", mutationEnv1)
mutPop2 <- new("population", mutationEnv2)
basePop1 <- duplicate(mutPop1)
basePop2 <- duplicate(mutPop2)
mresults1 <- reproduction.env(mutationEnv1)$mutate(mutPop1[1:10])
mresults2 <- reproduction.env(mutationEnv2)$mutate(mutPop2[1:10])
evaluate(basePop1, one.max.fn)
evaluate(basePop2, one.max.fn)
evaluate(mutPop1, one.max.fn)
evaluate(mutPop2, one.max.fn)
ga.print(basePop1, gene.sep = "")
ga.print(mutPop1, gene.sep = "")
ga.print(mresults1)
ga.print(basePop2, gene.sep = "")
ga.print(mutPop2, gene.sep = "")
ga.print(mresults2)

ga.print(mresults1[[1]])

ga.env4 <- new.GA.env(pop.size = 10, 
                      encoding.args = new.encoding.args(chr.length = 8,chr.encode.type = "symbolic",gene.alphabet = c("a", "b", "c")),
                      mutation.args = new.mutation.args(mutation.type = "symbolic", prob.mutation = 0.3))
mutate <- mutate.fgen(ga.env4$reproduction.env)
pop4a <- new.population(ga.env4)
pop4b <- duplicate(pop4a)
mr <- mutate(pop4b[1:5])
ga.print(pop4a[1:5])
ga.print(pop4b[1:5])
ga.print(mr)

ga.env4 <- new.GA.env(pop.size = 10, 
                      encoding.args = new.encoding.args(chr.length = 8,chr.encode.type = "symbolic",gene.alphabet = c("a", "b", "c")),
                      mutation.args = new.mutation.args(mutation.type = "symbolic", prob.mutation = 0.3))
with(ga.env4, {
	pop.a <- new.population(GA.env)
	pop.b <- duplicate(pop.a)
	mr <- mutate(pop.b[1:5])
	ga.print(pop.a[1:5])
	ga.print(pop.b[1:5])
	ga.print(mr)
})

######WORKING UP TO HERE
# chr2.vector <- duplicate(pop2)
# mutate2.fn <- mutation.genes.fgen(mutation.genes.creep, max=10, min=0)
# mutate2 <- mutation.fgen(prob.mutation = 0.2, mutation.genes.fn = mutate2.fn)
# mresults2 <- mutate2(chr2.vector)
# ga.print(pop2)
# ga.print(chr2.vector)
# ga.print(mresults2)
# 
# chr3.vector <- duplicate(pop2)
# mutate3.fn <- mutation.genes.fgen(mutation.genes.integer, 0:9)
# mutate3 <- mutation.fgen(prob.mutation = 0.8, mutation.genes.fn = mutate3.fn)
# mresults3 <- mutate3(chr3.vector)
# ga.print(pop2)
# ga.print(chr3.vector)
# ga.print(mresults3)
# 
# new.chr.spin <- new.chromosome.fgen(new.chromosome.spin)
# pop4 <- new.random.population(10, 5, new.chr.spin)
# evalutate(pop4, one.max.fn)
# ga.print(pop4)
# chr4.vector <- duplicate.chromosomes(pop4)
# mutate4 <- mutation.fgen(prob.mutation = 0.2, mutation.genes.fn = mutation.genes.spin)
# mresults4 <- mutate4(chr4.vector)
# ga.print(pop4)
# ga.print(chr4.vector)
# ga.print(mresults4)

### testing crossover
ga.env5a <- new.GA.env(pop.size = 10, 
								encoding.args = new.encoding.args(chr.length = 8, gene.alphabet = c("a", "b", "c"), chr.encode.type = "symbolic"),
                mutation.args = new.mutation.args(mutation.type = "symbolic", prob.mutation = 0.3),
                xover.args = new.xover.args(xover.type = "one.pt", xover.prob = 1.0))
xover <- xover.fgen(ga.env5a$reproduction.env$xover.swapMask)
pop5a.org <- new.population(ga.env5a)
pop5a.dup <- duplicate(pop5a.org)
p5a.dup.v1 <- pop5a.dup[(1:10)%%2==0]
p5a.dup.v2 <- pop5a.dup[(1:10)%%2==1]
xresults1 <- xover(p5a.dup.v1, p5a.dup.v2)
ga.print(pop5a.org, gene.sep = "")
ga.print(pop5a.dup, gene.sep = "")
ga.print(xresults1, print.xpts = TRUE)

ga.env5b <- new.GA.env(pop.size = 10, 
                encoding.args = new.encoding.args(gene.alphabet = c("a", "b", "c"),chr.length = 8, chr.encode.type = "symbolic"),
								mutation.args = new.mutation.args(mutation.type = "symbolic", prob.mutation = 0.3),
								xover.args = new.xover.args(xover.type = "k.pt",xover.k = 2,xover.prob = 1.0))
xover <- xover.fgen(ga.env5b$reproduction.env$xover.swapMask)
pop5b.org <- new.population(ga.env5b)
pop5b.dup <- duplicate(pop5b.org)
p5b.dup.v1 <- pop5b.dup[(1:10)%%2==0]
p5b.dup.v2 <- pop5b.dup[(1:10)%%2==1]
xresults2 <- xover(p5b.dup.v1, p5b.dup.v2)
ga.print(pop5b.org, gene.sep = "")
ga.print(pop5b.dup, gene.sep = "")
ga.print(xresults2, print.xpts = TRUE)

chr2.vector <- duplicate(pop2)
p1.vector <- chr2.vector[(1:10)%%2==0]
p2.vector <- chr2.vector[(1:10)%%2==1]
ga.print(chr2.vector)
unif.xover <- xover.fgen(xover.swapMask.fgen(xover.mask.uniform, 0.3))
xresults2 <- unif.xover(p1.vector, p2.vector)
ga.print(chr2.vector)
ga.print(xresults2)

chr1.vector <- duplicate(pop1)
p1.vector <- chr1.vector[(1:10)%%2==0]
p2.vector <- chr1.vector[(1:10)%%2==1]
ga.print(chr1.vector)
threePt.xover <- xover.fgen(xover.swapMask.fgen(xover.mask.kpoint, 3, TRUE))
xresults2 <- threePt.xover(p1.vector, p2.vector)
ga.print(chr1.vector)
ga.print(xresults2, print.xpts = TRUE)


xoverTest.GA=new.GA.env(pop.size = 10, GA.base.args=new.GA.base.args(6),
                        encoding.args = new.encoding.args(chr.length = 8, gene.alphabet = c("a", "b", "c","d","e","f","g"), chr.encode.type = "symbolic"),
                        mutation.args = new.mutation.args(mutation.type = "symbolic", prob.mutation = 0),
                        xover.args = new.xover.args(xover.type = "one.pt", xover.prob = 1.0), fitness.args=new.fitness.args(fitness.fn=abc.fit),
                        reporting.fn=reportAll.reporting.fn)
generational.ga(xoverTest.GA)
ga.print(xoverTest.GA$reported.data[[2]]@currentGen.results[[1]]@chromosomes$values)
ga.print(xoverTest.GA$reported.data[[3]]@currentGen.results[[1]]@chromosomes$values)
xoverTest.GA$reported.data[[2]]@currentGen.results[[4]]@returnList$values
#xoverTest.GA$reported.data[[2]]@currentGen.results[[4]]@returnList$p1
#xoverTest.GA$reported.data[[2]]@currentGen.results[[4]]@returnList$p2

### Selection
#Tournament selection runs, not sure how to know if its 'correct'
ga.env6a <- new.GA.env(pop.size = 20)
pop3 <- new.population(ga.env6a)
evaluate(pop3, one.max.fn)
ga.env6b <- new.GA.env(pop.size = 30)
pop4 <- new.population(ga.env6b)
evaluate(pop4, one.max.fn)
ga.print(pop3)
tournament.selection(selection.size=8, tourn.size = 2, pop = pop3, decreasing = TRUE, verbose = TRUE)
tournament.selection(selection.size=8, tourn.size = 2, pop = pop3, prob.select.worse = 0.2, decreasing = TRUE, verbose = TRUE)
tournament.selection(selection.size=8, tourn.size = 2, pop = pop3, prob.select.worse = 0.2, decreasing = TRUE)
simple.tournament.selection(selection.size=8, tourn.size = 2, pop = pop3, decreasing = TRUE, verbose = TRUE)
simple.tournament.selection(selection.size=8, tourn.size = 2, pop = pop3, decreasing = TRUE)
st.selection <- select.population.fgen(tournament.selection, tourn.size = 2, prob.select.worse = 0.2, decreasing = TRUE)
st.selection(8, pop3)
st.selection(25, pop4)


#Not sure what this section is doing, the run but do they work correctly?
fitnessProportional.setup(pop3)
cumulative.fitness(pop3)
fitnessProportional.selection(8, pop3, verbose = TRUE)
fitnessProportional.selection(8, pop3)

ga.print(pop4)
elite.selection(pop4, 3)
ga.print(pop3)
elite.selection(pop3, 3)
top4 <- select.elite.population.fgen(elite.selection, elite.size=4)
top4(pop4)

#Time the GA
ptm <- proc.time()
ga<-new.GA.env()
generational.ga(ga)
proc.time() - ptm
