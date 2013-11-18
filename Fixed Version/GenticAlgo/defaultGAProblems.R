#Decode Functions
straight.decode <- function(chr){
  chr
}

one.max.decode <- straight.decode

#Default fitness function
one.max.fn <- function(genes){
  sum(genes)  
}

finite.min.fn <- function(genes, gene.max){
  length(genes) * gene.max - sum(genes)
}

#Goal functions
simpleGoal<- function(goal, epsilon)
{
  goalFunction <- function(popFit)
  {
    if (max(popFit) >= goal + epsilon) return(TRUE) else return(FALSE)    
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
DeJong.F1.fitness <- function(genes)
{
  n = 10
  chromosomeLength = 100
  geneSize
  range = 2^(geneSize) #the number of ints in the range -5.12<= xi < 5.12
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (BCD.to.Decimal(genes[((i - 1)*geneSize + 1):(i*geneSize)])*5.12*2/range) - 5.12    
  }
  
  total = 0
  for (i in 1:n)
  {
    total = total + xVals[[i]]^2
  }

  -total #return a negative fitness to attempt to minimize
}
#TODO-use the decode function
DeJong.F1.Decode <-function(genes)
{
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (BCD.to.Decimal(genes[((i - 1)*n + 1):(i*n)])*5.12*2/range) - 5.12    
  }
}
#TODO - create goal function

#dejongGA=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"),
#fitness.args=new.fitness.args(fitness.fn=DeJong.F1.fitness, decode.fn=straight.decode, goal = 0))

DeJong.F2.fitness <- function(genes)
{
  chromosomeLength = 24
  geneSize = chromosomeLength/2
  range = 2^(geneSize) #the number of ints in the range -2.048<= x,y < 2.048
  
  x=BCD.to.Decimal(genes[1:12])*2.048*2/range - 2.048    
  y=BCD.to.Decimal(genes[13:24])*2.048*2/range - 2.048
  
  fitness = 100*(x^2 - y^2)^2 + (1-x)^2
  -fitness #return a negative fitness to attempt to minimize
}

#dejongGA=new.GA.env(encoding.args=new.encoding.args(chr.length=24, chr.encode.type="binary"),
#fitness.args=new.fitness.args(fitness.fn=DeJong.F2.fitness, decode.fn=straight.decode, goal = 0))

DeJong.F3.fitness <- function(genes)
{
  n = 10
  chromosomeLength = 100
  geneSize = chromosomeLength/n
  range = 2^(geneSize) #the number of ints in the range -5.12<= xi < 5.12
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (BCD.to.Decimal(genes[((i - 1)*geneSize + 1):(i*geneSize)])*5.12*2/range) - 5.12    
  }
  xVals = as.vector(xVals, mode = "numeric")
  max(xVals)
}

#dejongGA=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"),
#fitness.args=new.fitness.args(fitness.fn=DeJong.F3.fitness, decode.fn=straight.decode, goal = 5.12))

DeJong.F4.fitness <- function(genes)
{
  n = 10
  chromosomeLength = 80
  geneSize = chromosomeLength/n
  range = 2^(geneSize) #the number of ints in the range -5.12<= xi < 5.12
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (BCD.to.Decimal(genes[((i - 1)*(geneSize) + 1):(i*geneSize)])*1.28*2/range) - 1.28 
  }
  
  total = 0
  for (i in 1:n)
  {
    total = total + i*xVals[[i]]^4
  }
  
  return(total + rnorm(1))
}

#dejongGA=new.GA.env(encoding.args=new.encoding.args(chr.length=80, chr.encode.type="binary"),
                    #fitness.args=new.fitness.args(fitness.fn=DeJong.F4.fitness, decode.fn=straight.decode, goal.fn=noGoal))

DeJong.F5.fitness.generator <- function()
{
  a = matrix(data = runif(25*25)*65.536*2 - 65.536,nrow=25,ncol=25)
  
  DeJong.F5.fitness <- function(genes)
  {
    n = 25
    chromosomeLength = 425
    geneSize = chromosomeLength/n
    range = 2^(geneSize)
    
    xVals = vector("list",n)
    for (i in 1:n)
    {
      xVals[i] = (BCD.to.Decimal(genes[((i - 1)*(geneSize) + 1):(i*geneSize)])*65.536*2/range) - 65.536 
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

#dejongGA=new.GA.env(encoding.args=new.encoding.args(chr.length=425, chr.encode.type="binary"),
#fitness.args=new.fitness.args(fitness.fn=DeJong.F5.fitness.generator(), decode.fn=straight.decode, goal.fn = noGoal))


#Convert a BCD decimal number to a decimal number
BCD.to.Decimal <- function(binaryNumb)
{
  binaryLength = length(binaryNumb)
  decimalNumb = 0
  for(i in 1:binaryLength)
  {
    if ( binaryNumb[i] == 1)
      decimalNumb = decimalNumb + 2^(binaryLength - i)
  }
  
  decimalNumb
}

BCD.to.Decimal <- function(binaryNumb)
{
  L = length(binaryNumb)
  sum(2L^(seq_along(binaryNumb)-1L) * rev(binaryNumb))
}

#Use grayCode instead of BCD conversion
grayCode.to.Decimal <- function(binaryNumb)
{

}