get.groups <- function( formula )
  {
    if ( class( formula ) != "formula" )
      {
        stop( paste( formula, " is not a formula." ) )
      }

    ## Schreibweise vereinfachen:
    f <- formula

    ## Maximalmodell mit den vorhandenen Haupteffekten
    f.expanded <- expand( f )

    ## Alle möglichen Gruppen
    terms <- terms( f.expanded )
    labels <- attr(terms,"term.labels")
    
    return( labels )
  }
