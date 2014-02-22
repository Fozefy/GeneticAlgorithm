numbersGame <- function(organism, popNum, otherPops, externalConnectionsMatrix){

  otherGenes = otherPops[[1]]@organisms$values[[externalConnectionsMatrix[organism@index$value, popNum]]]@chromosome$genes
  
  numLength = length(organism@chromosome$genes)
  firstNum1 = organism@chromosome$genes[1:(numLength/2)]
  firstNum2 = otherGenes[1:(numLength/2)]
  

  secondNum1 = organism@chromosome$genes[(numLength/2):numLength]
  secondNum2 = organism@chromosome$genes[(numLength/2):numLength]
  
  if (abs(firstNum1 - firstNum2) > abs(secondNum1 - secondNum2))
  {
    #Go by the biggest difference in numbers to determine fitness
    if (firstNum1 > firstNum2)
    {
      #Our fitness being measured is bigger
      return(1)
    }
    
    return(0)
  }
  else
  {
    if (secondNum1 > secondNum2)
    {
      #Our fitness being measured is bigger
      return(1)
    }
    
    return(0)
  }
}

numbersGameGoalFunction <- function(popFit)
{
  #TODO - figure out a goal function
}