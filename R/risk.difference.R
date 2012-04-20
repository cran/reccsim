risk.difference <- function( pop.risk, pop.percentages, relative.risks ){
  absolute.risk <- absolute.risk( pop.risk, pop.percentages, relative.risks )
  risk.diff <- absolute.risk-absolute.risk[1]
  return(risk.diff)
}
