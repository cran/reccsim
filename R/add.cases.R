## Tells you how many additional cases you would expect from the groups
## compared to the comparison group.

add.cases <- function( pop.size, pop.risk, pop.percentages, relative.risks ){
  risk.diff <- risk.difference( pop.risk, pop.percentages, relative.risks )
  pop.percentages <- c( 1 - sum(pop.percentages), pop.percentages )
  add.cases <- pop.percentages * pop.size * risk.diff
  return(add.cases)
}
