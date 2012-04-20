expand <- function( formula )
  {
    main <- get.main( formula )

    ## Aus Haupteffekten die Formel mit allen Kreuzklassen bilden:
    rhs <- paste( main, collapse = "*" )
    lhs <- as.character( formula[[2]] )
    relation <- "~"
    f.full <- paste( lhs, relation, rhs )
    f.full <- as.formula( f.full )
    
    ## Ergebnis zurückgeben
    return( f.full )
  }

