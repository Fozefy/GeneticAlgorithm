#Functions used for creating spatial components for the GA

#Graph connecting 'next' 4 values
testfun<-function(value, index)
{
  if(index == 97)
  {
    c(98:100,1)
  }
  else if (index == 98)
  {
    c(99,100,1:2)
  }
  else if (index == 99)
  {
    c(100,1:3)
  }
  else if (index == 100)
  {
    1:4
  }
  else
  {
    1:4 + index
  }
}
#test = 1:100
#graph = t(mapply(testfun,test, seq_along(test)))

#Complete Graph
complete.graph <- function(popSize)
{
  testfun<-function(value)
  {
    1:popSize
  }
  
  graph = mapply(testfun,1:popSize)
  
  return(t(graph))
}

gridConstructor.withDiag <-function(popSize)
{
  sideLength = sqrt(popSize)
  graph = matrix(nrow=popSize,ncol=8)
  for (i in 1:popSize)
  {
    #Connections above
    if(i <= sideLength)
    {
      graph[i,1] = popSize - sideLength + i
      
      if (i %% sideLength == 1)
      {
        graph[i,2] = popSize
        graph[i,3] = popSize - sideLength + i + 1
      }
      else if(i %% sideLength == 0)
      {
        graph[i,2] = popSize - sideLength + i - 1
        graph[i,3] = popSize - sideLength + 1
      }
      else
      {
        graph[i,2] = popSize - sideLength + i - 1
        graph[i,3] = popSize - sideLength + i + 1
      }
      
    }
    else
    {
      graph[i,1] = i - sideLength
      
      if (i %% sideLength == 1)
      {
        graph[i,2] = i - 1
        graph[i,3] = i - sideLength + 1
      }
      else if(i %% sideLength == 0)
      {
        graph[i,2] = i - sideLength - 1
        graph[i,3] = i - sideLength*2 + 1
      }
      else
      {
        graph[i,2] = i - sideLength - 1
        graph[i,3] = i - sideLength + 1
      }   
    }
    
    #Connections below
    if(i > popSize - sideLength)
    {
      graph[i,4] = sideLength + i - popSize
      
      if (i %% sideLength == 1)
      {
        graph[i,5] = sideLength
        graph[i,6] = sideLength + i - popSize + 1
      }
      else if(i %% sideLength == 0)
      {
        graph[i,5] = sideLength + i - popSize - 1
        graph[i,6] = 1
      }
      else
      {
        graph[i,5] = sideLength + i - popSize - 1
        graph[i,6] = sideLength + i - popSize + 1
      }
    }
    else
    {
      graph[i,4] = i + sideLength
      
      if (i %% sideLength == 1)
      {
        graph[i,5] = i + sideLength*2 - 1
        graph[i,6] = i + sideLength + 1
      }
      else if(i %% sideLength == 0)
      {
        graph[i,5] = i + sideLength - 1
        graph[i,6] = i + 1
      }
      else
      {
        graph[i,5] = i + sideLength - 1
        graph[i,6] = i + sideLength + 1
      }
    }
    
    #Connection to the left
    if (i %% sideLength == 1)
    {
      graph[i,7] = i + sideLength - 1
    }
    else
    {
      graph[i,7] = i - 1
    }
    
    #Connection to the right
    if (i %% sideLength == 0)
    {
      graph[i,8] = i - sideLength + 1
    }
    else
    {
      graph[i,8] = i + 1
    }
  }
  
  graph
}


#Creates a grid structure - popSize must be a perfect square
gridConstructor <- function(popSize)
{
  sideLength = sqrt(popSize)
  graph = matrix(nrow=popSize,ncol=4)
  for (i in 1:popSize)
  {
    #Connection above
    if(i <= sideLength)
    {
      graph[i,1] = popSize - sideLength + i
    }
    else
    {
      graph[i,1] = i - sideLength
    }
    
    #Connection below
    if(i > popSize - sideLength)
    {
      graph[i,2] = sideLength + i - popSize
    }
    else
    {
      graph[i,2] = i + sideLength
    }
    
    #Connection to the left
    if (i %% sideLength == 1)
    {
      graph[i,3] = i + sideLength - 1
    }
    else
    {
      graph[i,3] = i - 1
    }
    
    #Connection to the right
    if (i %% sideLength == 0)
    {
      graph[i,4] = i - sideLength + 1
    }
    else
    {
      graph[i,4] = i + 1
    }
  }
  
  graph
}
#graph = gridConstructor(100)

#Test the spatial constructor
#ga = new.GA.env(selection.args=new.selection.args(adjMatrix=graph))

#Spatial Coevolutionary
#ga = new.GA.env(GA.base.args=new.GA.base.args(numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph))
