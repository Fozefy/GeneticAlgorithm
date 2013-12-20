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
test = 1:100
graph = mapply(testfun,test, seq_along(test))

#Complete Graph
testfun<-function(value)
{
  1:100
}
test = 1:100
graph = mapply(testfun,test)

#Test the spatial constructor
ga = new.GA.env(selection.args=new.selection.args(adjMatrix=graph))