#Default fitness function
one.max.fn <- function(organism, ...){
  sum(organism@chromosome$genes)
}

finite.min.fn <- function(genes, gene.max){
  length(genes) * gene.max - sum(genes)
}

#Variant of one.max with two pops, takes a gene of the same id in the other pop and adds at half value to fitness
twoPop.one.max.withCoupling <- function(coupling)
{
  twoPop.one.max <- function(organism, popNum, otherPops, externalConnectionsMatrix){
    otherGenes = otherPops[[1]]@organisms$values[[externalConnectionsMatrix[organism@index$value, popNum]]]@chromosome$genes
    sum(organism@chromosome$genes)*coupling + sum(otherGenes)*(1 - coupling)
  }
}

twoPop.one.max.seperate <- function(organism, pop, otherPops, externalConnectionsMatrix){
  sum(organism@chromosome$genes)*3
}

twoPop.one.max.predPrey <- function(organism, popNum, otherPops, externalConnectionsMatrix)
{
  otherGenes = otherPops[[1]]@organisms$values[[externalConnectionsMatrix[organism@index$value, popNum]]]@chromosome$genes
  
  if(popNum == 1)
  {
    #We are the primary Population (predator)
    #Fitness = geneLength - each 0 where prey has 1
    fitness = length(organism@chromosome$genes) - sum(organism@chromosome$genes == 0 & otherGenes == 1)
  }
  else
  {
    #We are the secondary Population (prey)
    #Fitness = for each 1 where predator has 0
    fitness = sum(organism@chromosome$genes == 1 & otherGenes == 0)    
  }
  
  if (sum(organism@chromosome$genes) == 30)
  {
    fitness = 31
  }
  
  return(fitness)
}

abc.fit <-function(organism)
{
  chr = organism@chromosome$genes
  total = 0
  for(i in 1: length(chr))
  {
    if (chr[[i]] == 'a')
    {
      total = total + 2
    }
    else if (chr[[i]] == 'b')
    {
      total = total + 1
    }
    else
    {
      total = total
    }
  }
  
  return(total)
}

#Goal functions
simpleGoal<- function(goal, epsilon, maximizing = TRUE)
{
  goalFunction <- function(popFit)
  {
    #The default goal function only checks the first population's fitness
    if (maximizing)
    {
      if (max(popFit[[1]]) >= (goal - epsilon)) return(TRUE) else return(FALSE)
    }
    else
    {
      if (min(popFit[[1]]) <= (goal + epsilon)) return(TRUE) else return(FALSE)
    }
  }
}

noGoal <- function(...)
{
  FALSE
}

#Elitism
truncation.selection <- elite.selection

#De Jong Test Functions
#F1
#This will be represented by a 100 digit long binary number filling the range -5.12<= xi <5.12
#Fitness Function
DeJong.F1.fitness <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 10
  chromosomeLength = 100
  geneSize = chromosomeLength/n
  range = 2^(geneSize) #the number of ints in the range -5.12<= xi < 5.12
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (GrayCode.to.Decimal(genes[((i - 1)*geneSize + 1):(i*geneSize)])*5.12*2/range) - 5.12    
  }
  
  total = 0
  for (i in 1:n)
  {
    total = total + xVals[[i]]^2
  }

  total
}

#dejongGA.F1=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=DeJong.F1.fitness, goal = 0),selection.args=new.selection.args(maximizing = FALSE))

DeJong.F2.fitness <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  chromosomeLength = 24
  geneSize = chromosomeLength/2
  range = 2^(geneSize) #the number of ints in the range -2.048<= x,y < 2.048
  
  x=GrayCode.to.Decimal(genes[1:12])*2.048*2/range - 2.048    
  y=GrayCode.to.Decimal(genes[13:24])*2.048*2/range - 2.048
  
  fitness = 100*(x^2 - y^2)^2 + (1-x)^2
  fitness
}

#dejongGA.F2=new.GA.env(encoding.args=new.encoding.args(chr.length=24, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=DeJong.F2.fitness, goal = 0),selection.args=new.selection.args(maximizing = FALSE))

DeJong.F3.fitness <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 10
  chromosomeLength = 100
  geneSize = chromosomeLength/n
  range = 2^(geneSize) #the number of ints in the range -5.12<= xi < 5.12
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (Binary.to.Decimal(genes[((i - 1)*geneSize + 1):(i*geneSize)])*5.12*2/range) - 5.12    
  }
  xVals = as.vector(xVals, mode = "numeric")
  max(xVals)
}

#dejongGA.F3=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=DeJong.F3.fitness, goal = 5.12))

DeJong.F4.fitness <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 10
  chromosomeLength = 80
  geneSize = chromosomeLength/n
  range = 2^(geneSize) #the number of ints in the range -5.12<= xi < 5.12
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (Binary.to.Decimal(genes[((i - 1)*(geneSize) + 1):(i*geneSize)])*1.28*2/range) - 1.28 
  }
  
  total = 0
  for (i in 1:n)
  {
    total = total + i*xVals[[i]]^4
  }
  
  return(total + rnorm(1))
}

#dejongGA.F4=new.GA.env(encoding.args=new.encoding.args(chr.length=80, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=DeJong.F4.fitness, goal.fn=noGoal))

DeJong.F5.fitness.generator <- function()
{
  a = matrix(data = runif(25*25)*65.536*2 - 65.536,nrow=25,ncol=25)
  
  DeJong.F5.fitness <- function(organism, ...)
  {
    genes = organism@chromosome$genes
    
    n = 25
    chromosomeLength = 425
    geneSize = chromosomeLength/n
    range = 2^(geneSize)
    
    xVals = vector("list",n)
    for (i in 1:n)
    {
      xVals[i] = (Binary.to.Decimal(genes[((i - 1)*(geneSize) + 1):(i*geneSize)])*65.536*2/range) - 65.536 
    }

    total = 0.002
    for (j in 1:n)
    {
      sumOfI = 0
      for (i in 1:n)
      {
        sumOfI = sumOfI + (xVals[[i]] - a[[i,j]])^6
      }
      total = total + sumOfI
    }

    total
  }
}

#dejongGA.F5=new.GA.env(encoding.args=new.encoding.args(chr.length=425, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=DeJong.F5.fitness.generator(), goal.fn = noGoal))

Rastrigin.fitness.fn <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 10
  chromosomeLength = 100
  geneSize = chromosomeLength/n
  range = 2^(geneSize)
  
  A = 10
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (Binary.to.Decimal(genes[((i - 1)*geneSize + 1):(i*geneSize)])*5.12*2/range) - 5.12    
  }
  
  total = A*n
  for (i in 1:n)
  {
    total = total + xVals[[i]]^2 - A*cos(2*pi*xVals[[i]])
  }
  403.5329-total
}


#rastriginGA=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rastrigin.fitness.fn, goal = 0), selection.args=new.selection.args(maximizing = FALSE))

Rosenbrock.fitness.fn <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 2
  chromosomeLength = 100
  x = GrayCode.to.Decimal(genes[1:(chromosomeLength/n)])
  y = GrayCode.to.Decimal(genes[(chromosomeLength/n + 1) : chromosomeLength])
  
  total = (1 - x)^2 + 100*(y - x^2)^2
  
  total
}

#rosenbrockGA=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Rosenbrock.fitness.fn, goal = 0), selection.args=new.selection.args(maximizing = FALSE))

Schwefel.fitness.fn <-function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 10
  chromosomeLength = 100
  geneSize = chromosomeLength/n
  range = 2^(geneSize)
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (Binary.to.Decimal(genes[((i - 1)*geneSize + 1):(i*geneSize)])) - 512    
  }
  
  total = 418.9829*n
  for (i in 1:n)
  {
    total = total - xVals[[i]]*sin(sqrt(abs(xVals[[i]])))
  }
  
  total
}

#shwefelGA=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=Schwefel.fitness.fn, goal = 0), selection.args=new.selection.args(maximizing = FALSE))

F8.Fitness <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 10
  chromosomeLength = 100
  geneSize = chromosomeLength/n
  range = 2^(geneSize)
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (Binary.to.Decimal(genes[((i - 1)*(geneSize) + 1):(i*geneSize)])*512*2/range) - 512
  }
  total = cos(xVals[[1]]/sqrt(1))
  for (i in 2:n)
  {
    tempTotal = tempTotal*cos(xVals[[i]]/sqrt(i))
  }
  
  total = 1 - total
  for (i in 1:n)
  {
    total = total + (xVals[[i]]^2)/4000
  }
  
  return(total)
}

Simple.F2.fitness <- function(genes)
{
  chromosomeLength = 24
  geneSize = chromosomeLength/2
  range = 2^(geneSize) #the number of ints in the range -2.048<= x,y < 2.048
  
  x=GrayCode.to.Decimal(genes[1:12])*2.048*2/range - 2.048    
  y=GrayCode.to.Decimal(genes[13:24])*2.048*2/range - 2.048
  
  fitness = 100*(x^2 - y^2)^2 + (1-x)^2
  fitness
}

Simple.F8.Fitness <- function(value)
{
  1 + value^2/4000 - cos(value)
}

F8F2.fitness.fn <- function(organism, ...)
{
  genes = organism@chromosome$genes
  
  n = 20
  chromosomeLength = 240
  geneSize = chromosomeLength/n
  range = 2^(geneSize)
  
  total = 0
  for(i in 1:(n-1))
  {
    total = total + Simple.F8.Fitness(Simple.F2.fitness(genes[((i-1)*12 + 1):((i+1)*12)]))
  }
  total = total + Simple.F8.Fitness(Simple.F2.fitness(c(genes[((n-1)*12):chromosomeLength],genes[1:12])))

  783.8844-total
}

# f8f2tester <- function(x,y)
# {
#   value = 100*(x^2 - y^2)^2 + (1-x)^2
#   1 + value^2/4000 - cos(value)
# }
# test = NULL
# for (i in 90:100)
# {
#   for (j in 1:100)
#   {
#     test[(i-90)*100+j] = f8f2tester(i*2.048/100,j*2.048/100)
#   }
# }
#F8F2GA=new.GA.env(encoding.args=new.encoding.args(chr.length=240, chr.encode.type="binary"), fitness.args=new.fitness.args(fitness.fn=F8F2.fitness.fn, goal = 0), selection.args=new.selection.args(maximizing = FALSE))


#Convert a binary number to a decimal number
Binary.to.Decimal <- function(binaryNumb)
{
  L = length(binaryNumb)
  sum(2L^(seq_along(binaryNumb)-1L) * rev(binaryNumb))
}

#Use grayCode instead of standard binary conversion
GrayCode.to.Decimal <- function(grayNumb)
{
  binaryNumb = vector("numeric",length(grayNumb))
  binaryNumb[1] = grayNumb[1]
  for (i in 2:length(grayNumb))
  {
    binaryNumb[i] = xor(grayNumb[i], binaryNumb[i - 1])
  }
  
  return(Binary.to.Decimal(binaryNumb))
}
