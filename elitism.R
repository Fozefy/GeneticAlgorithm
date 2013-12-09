#### Elitism

select.elite.population.fgen <- function(elite.fn, elite.size, decreasing, ...){
  function(pop)
    elite.fn(pop = pop, elite.size = elite.size, decreasing = decreasing, ...)
}

#note: elite.selection only works with integer or floating point fitness ordered by <= or >=
elite.selection <- function(pop, elite.size = 1, decreasing = TRUE, pop.fit = NULL, verbose = FALSE){
  print(decreasing)
  #Sort the organisms to put the 'best' at the top of our list
  sortedOrganisms = pop@organisms$values
  class(sortedOrganisms) <- "organismList"
  sortedOrganisms = sort(sortedOrganisms, decreasing = decreasing)

  #Fill our elite list with the top fitness values
  eliteList = vector("list", elite.size)
  eliteList[1] = sortedOrganisms[1]
  if (elite.size > 1)
  {
    elitesFilled = 1
    for(i in 2:length(sortedOrganisms))
    {
      #Check if we already have this elite
      foundMatch = FALSE
      for (j in 1:elitesFilled)
      {
        if (identical(eliteList[[j]]@chromosome$genes, sortedOrganisms[[i]]@chromosome$genes))
        {
          foundMatch = TRUE
        }
      }
      
      if (!foundMatch)
      {
        elitesFilled = elitesFilled + 1
        eliteList[elitesFilled] = sortedOrganisms[[i]]
      }      
      
      if (elitesFilled == elite.size) break
      else if (i == length(sortedOrganisms)) cat("Too many duplicates in population to generate",elite.size, "elites, Returning: ",elitesFilled)
    }
  }
cat("Elite Fitnes:", eliteList[[1]]@fitness$value)
  return(eliteList)
}

#Used for sortation
`[.organismList` <- function(x, i) {
  class(x) <- "list"
  structure(x[i], class="organismList")
}

`>.organismList` <- function(e1, e2) {
  e1[[1]]@fitness$value > e2[[1]]@fitness$value
}

`==.organismList` <- function(e1, e2) {
  e1[[1]]@fitness$value == e2[[1]]@fitness$value
}  