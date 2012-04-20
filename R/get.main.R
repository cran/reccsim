get.main <- function( formula )
  {
    if ( class( formula ) != "formula" )
      stop( paste( formula, " is not a formula." ) )
    
    ## Schreibweise vereinfachen:
    f <- formula

    terms <- terms( f )
    labels <- attr( terms, "term.labels" )
    
    ## Welche sind Kreuzeffekte?
    interact <- grep( ":", labels )

    ## Sind Kreuzeffekte enthalten?
    do <- ( length( interact ) > 0 )

    ## Haupteffekte auslesen:
    if ( do == T )
      {
        main <- labels[-interact]
      }
    else
      {
        main <- labels
      }
    return( main )
  }
