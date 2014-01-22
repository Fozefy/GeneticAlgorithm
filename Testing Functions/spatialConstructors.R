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

ring.graph <- function(popSize)
{
  graph = matrix(nrow=popSize,ncol=4)
  for (i in 1:popSize)
  {
    #Less
    if (i == 1)
    {
      graph[i,1] = popSize
      graph[i,2] = popSize - 1
    }
    else if (i == 2)
    {
      graph[i,1] = popSize
      graph[i,2] = 1
    }
    else
    {
      graph[i,1] = i - 1
      graph[i,2] = i - 2
    }
    
    #More
    if (i == popSize)
    {
      graph[i,3] = 1
      graph[i,4] = 2
    }
    else if (i == popSize - 1)
    {
      graph[i,3] = popSize
      graph[i,4] = 1
    }
    else
    {
      graph[i,3] = i + 1
      graph[i,4] = i + 2
    }
  }
  
  graph
}

ring.graph.extra <- function(popSize)
{
  graph = matrix(nrow=popSize,ncol=8)
  for (i in 1:popSize)
  {
    #Less
    if (i == 1)
    {
      graph[i,1] = popSize
      graph[i,2] = popSize-1
      graph[i,3] = popSize-2
      graph[i,4] = popSize-3
    }
    else if (i == 2)
    {
      graph[i,1] = i - 1
      graph[i,2] = popSize
      graph[i,3] = popSize-1
      graph[i,4] = popSize-2
    }
    else if (i == 3)
    {
      graph[i,1] = i - 1
      graph[i,2] = i - 2
      graph[i,3] = popSize
      graph[i,4] = popSize-1
    }
    else if (i == 4)
    {
      graph[i,1] = i - 1
      graph[i,2] = i - 2
      graph[i,3] = i - 3
      graph[i,4] = popSize
    }
    else
    {
      graph[i,1] = i - 1
      graph[i,2] = i - 2
      graph[i,3] = i - 3
      graph[i,4] = i - 4
    }
    
    #More
    if (i == popSize)
    {
      graph[i,5] = 1
      graph[i,6] = 2
      graph[i,7] = 3
      graph[i,8] = 4
    }
    else if (i == popSize - 1)
    {
      graph[i,5] = popSize
      graph[i,6] = 1
      graph[i,7] = 2
      graph[i,8] = 3
    }
    else if (i == popSize - 2)
    {
      graph[i,5] = i+1
      graph[i,6] = i+2
      graph[i,7] = 1
      graph[i,8] = 2
    }
    else if (i == popSize - 3)
    {
      graph[i,5] = i+1
      graph[i,6] = i+2
      graph[i,7] = i+3
      graph[i,8] = 1
    }
    else
    {
      graph[i,5] = i + 1
      graph[i,6] = i + 2
      graph[i,7] = i + 3
      graph[i,8] = i + 4
    }
  }
  
  graph
}

#Create a ring with some nodes connected at random to any other nodes
#Watts Strogatx model
ring.graph.smallWorld <- function(popSize)
{
  graph = ring.graph(popSize)
  graph = cbind(graph,0)
  
  for (i in 1:popSize)
  {
    #1 in 5 nodes will have an extra connecion (making it 2 in 5 as we need a reciever as well)
    if (sample(1:5,1) == 1)
    {
      connector = sample(1:(popSize-1),1)
      if (connector == i) connector = popSize #We don't want a self connection
      for(j in 5:length(graph[connector,]))
      {
        if (graph[connector,j] == 0)
        {
          graph[connector,j] = i
          break
        }
        else if (j == length(graph[connector,]))
        {
          #Need to add another column
          graph = cbind(graph,0)
          graph[connector,(j+1)] = i
        }
      }
      
      for(j in 5:length(graph[i,]))
      {
        if (graph[i,j] == 0)
        {
          graph[i,j] = connector
          break
        }
        else if (j == length(graph[i,]))
        {
          #Need to add another column
          graph = cbind(graph,0)
          graph[i,(j+1)] = connector
        }
      }
    }
  }
  graph
}

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

#Larger grid
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

#Create a single ring with no additional connection
singleRing <- function(popSize)
{
  graph = matrix(0,nrow=popSize,ncol=2)
  
  graph[1,1] = 2
  graph[1,2] = popSize

  graph[popSize,1] = 1
  graph[popSize,2] = popSize - 1
  
  for (i in 2:(popSize-1))
  {
    graph[i,1] = i + 1
    
    graph[i,2] = i - 1
  }
  graph
}

#Creates a 'line' connection with the rest of the graph being randomly generated
randomConstructor.withLine <-function(avgConnections, popSize)
{
  graph = matrix(0,nrow=popSize,ncol=2)
  
  for (i in 1:(popSize-1))
  {
    if (i == 1)
    {
      #1 doesn't have anything to fill its first node
      graph[i,1] = i+1
    }
    else
    {
      graph[i,2] = i+1
    }
    graph[i+1,1] = i
  }
  
  for (i in 1:(popSize*(avgConnections-2)/2 + 1))
  {
    isValid = FALSE
    while(!isValid)
    {
      firstNode = sample(1:popSize,1)
      secondNode = sample(1:popSize,1)
      if (secondNode != firstNode && sum(graph[firstNode,]==secondNode) == 0)
      {
        isValid = TRUE
      }
    }
    
    #Now that we have valid connections, add them!
    for(j in 1:length(graph[secondNode,]))
    {
      if (graph[secondNode,j] == 0)
      {
        graph[secondNode,j] = firstNode
        break
      }
      else if (j == length(graph[secondNode,]))
      {
        #Need to add another column
        graph = cbind(graph,0)
        graph[secondNode,(j+1)] = firstNode
      }
    }
    
    for(j in 1:length(graph[firstNode,]))
    {
      if (graph[firstNode,j] == 0)
      {
        graph[firstNode,j] = secondNode
        break
      }
      else if (j == length(graph[firstNode,]))
      {
        #Need to add another column
        graph = cbind(graph,0)
        graph[firstNode,(j+1)] = secondNode
      }
    }
  }
  graph
}

randomConstructor <- function(avgConnections, popSize)
{
  graph = matrix(0,nrow=popSize,ncol=1)
  
  for (i in 1:(popSize*avgConnections/2 - 1))
  {
    if (i <= popSize)
    {
      #Make sure ever node is selected at least once
      firstNode = i
    }
    else
    {
      firstNode = sample(1:(popSize-1),1)
    }
    
    secondNode = sample(1:(popSize-1),1)
    if (secondNode == firstNode) secondNode = popSize #We don't want a self connection
    
    for(j in 1:length(graph[secondNode,]))
    {
      if (graph[secondNode,j] == 0)
      {
        graph[secondNode,j] = firstNode
        break
      }
      else if (j == length(graph[secondNode,]))
      {
        #Need to add another column
        graph = cbind(graph,0)
        graph[secondNode,(j+1)] = firstNode
      }
    }
    
    for(j in 1:length(graph[firstNode,]))
    {
      if (graph[firstNode,j] == 0)
      {
        graph[firstNode,j] = secondNode
        break
      }
      else if (j == length(graph[firstNode,]))
      {
        #Need to add another column
        graph = cbind(graph,0)
        graph[firstNode,(j+1)] = secondNode
      }
    }
  }
  graph
}



#graph = gridConstructor(100)

#Test the spatial constructor
#ga = new.GA.env(selection.args=new.selection.args(adjMatrix=graph))

#Spatial Coevolutionary
#ga = new.GA.env(GA.base.args=new.GA.base.args(numPop=2), fitness.args=new.fitness.args(fitness.fn=twoPop.one.max, goal=90, externalConnectionsMatrix=matrix(c(1:100, 1:100), nrow=100, ncol=2)), xover.args = new.xover.args(keepSecondaryParent=FALSE), selection.args=new.selection.args(elitism=TRUE,adjMatrix=graph))
