build.population <- function( formula, pop.size, pop.risk, pop.percentages, relative.risks )
  {

    ## Exception handling

    ## Model
    if ( class( formula ) != "formula")
        stop( paste( formula, " is not a formula." ) )

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


    ## Gruppen finden.
    groups <- get.groups( formula )

    ## Exception handling: Passen die gegebenen Vektoren zu den Gruppen?
    if ( length(groups) != length(pop.percentages) )
      stop( paste( "Your model ", length(groups), " groups but ",
                  length(pop.percentages), "population percentages. Use 'get.groups(model)' to see all groups in your model." ) )
    if ( length(groups) != length(relative.risks) )
      stop( paste( "Your model has ", length(groups), " groups but ",
                  length(relative.risks), "relative risks. Use 'get.groups(model)' to see all groups in your model." ) )

    ## Modell inklusive Basisgruppe
    groups <- c( "0", groups )
    pop.percentages <- c( 1 - sum(pop.percentages), pop.percentages )
    relative.risks <- c( 1, relative.risks )

    ## Berechnung des Risikos in der Basisgruppe
    base.risk <- pop.risk / ( sum ( pop.percentages * relative.risks ) )
    ## Alle Risiken
    risks <- relative.risks * base.risk

    ## Gruppengrößen
    group.sizes <- pop.size * pop.percentages

    ## PopulationAtRisk Objekt erstellen
    pop.df <- data.frame( group = groups, size = group.sizes, perc = pop.percentages, RR = relative.risks, risk = risks, lambda = group.sizes * risks )
    population <- list( model=formula, size=pop.size, risk=pop.risk, table=pop.df)
    class(population) <- "PopulationAtRisk"
    return(population)
  }
