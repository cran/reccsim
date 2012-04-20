absolute.risk <- function( pop.risk, pop.percentages, relative.risks ){
  ## Population Risk
  if ( !is.double( pop.risk ) )
    stop("The population risk must be a number between 0 and 1.")
  if ( pop.risk < 0)
    stop("pop.risk must be between 0 and 1.")
  if ( pop.risk > 1)
    stop("pop.risk must be between 0 and 1.")
  
  ## Population Percentages
  if ( sum( pop.percentages ) > 1 )
    stop("The sum of your population percentages is more than 100%.")
  if ( is.double( pop.percentages ) != T )
    stop("Population percentages have to be doubles between 0 and 1.")
  if ( length( which( pop.percentages < 0 ) ) != 0 )
    stop("You gave at least one negative population percentage.")
  
  ## Relative Risks
  if( is.double( relative.risks ) != T )
    stop("Relative risks have to be doubles.")
  if ( length( which( relative.risks < 0 ) ) != 0 )
    stop("You gave at least one negative relative risk.")
  
  pop.percentages <- c( 1 - sum(pop.percentages), pop.percentages )
  relative.risks <- c( 1, relative.risks )
  
  absolute.risk <- pop.risk/( sum( pop.percentages * relative.risks ) ) * relative.risks
  return(absolute.risk)
}
