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
  range = 2^(chromosomeLength/n) #the number of ints in the range -5.12<= xi < 5.12
  
  xVals = vector("list",n)
  for (i in 1:n)
  {
    xVals[i] = (BCD.to.Decimal(genes[((i - 1)*n + 1):(i*n)])*5.12*2/range) - 5.12    
  }
  
  total = 0
  for (i in 1:n)
  {
    total = total + xVals[[i]]^2
  }

  total
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

dejong.F1.Goal <- function(popFit)
{
  FALSE
}

#dejongGA=new.GA.env(encoding.args=new.encoding.args(chr.length=100, chr.encode.type="binary"),
#fitness.args=new.fitness.args(fitness.fn=DeJong.F1.fitness, decode.fn=straight.decode, goal = -1000, goal.fn=dejong.F1.Goal))

DeJong.F2.fitness <- function(genes)
{
  chromosomeLength = 24
  range = 2^(chromosomeLength/2) #the number of ints in the range -2.048<= x,y < 2.048
  x=BCD.to.Decimal(genes[1:12])*2.048*2/range - 2.048    
  y=BCD.to.Decimal(genes[13:24])*2.048*2/range - 2.048
  
  fitness = 100*(x^2 - y^2)^2 + (1-x)^2
  fitness
}

#dejongGA=new.GA.env(encoding.args=new.encoding.args(chr.length=24, chr.encode.type="binary"),
#fitness.args=new.fitness.args(fitness.fn=DeJong.F2.fitness, decode.fn=straight.decode, goal = -1000, goal.fn=dejong.F1.Goal))

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

grayCode.to.Decimal <- function(binaryNumb)
{
  
}