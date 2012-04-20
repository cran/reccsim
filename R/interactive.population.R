interactive.population <- function( formula )
  {
    if ( class( formula ) != "formula" )
      stop( paste( formula, " is not a formula." ) )
    ## Zugriff vereinfachen
    f <- formula
    
    ## Liste einrichten, um die Ergebnisse/Eingaben (Parameter) zu sammeln:
    par.list <- vector("list",6)

    ## Erster Eintrag ist das Risikomodell
    par.list[[1]] <- f
    names(par.list)[1] <- "RiskModel"
    
    pop.size <- NA
    ## Zweiter Eintrag ist die Bevölkerungszahl
    ## (Benutzereingabe)
    while( class(pop.size) != "integer" )
      pop.size <- as.integer( readline("Please enter population size and press ENTER: ") )
    ## Zur Übersicht nochmal anzeigen:
    print( paste( "You gave a population of", pop.size, "." ) )

    ## Und als zweite Liste in der Parameterliste speichern
    par.list[[2]] <- pop.size
    names(par.list)[2] <- "population.size"

    ## Das Gesamtrisiko der Population einlesen
    pop.risk <- -1
    while (pop.risk < 0)
      {
        pop.risk <- as.double(
                              readline(
                                      paste("Enter the population's overall risk for",
                                            as.character(par.list$RiskModel[[2]]),":")
                                      )
                              )
        ## Von vorne, falls Wert zu groß.
        if ( pop.risk > 1 )
          pop.risk <- -1
      }
    par.list[[3]] <- pop.risk
    names(par.list)[3] <- "pop.risk"
    
    ## Modell mit allen Interaktionen
    full.model <- expand( f )
    groups <- get.groups( full.model )

    ## Bevölkerungsgruppen speichern (inkl. Nullgruppe)
    par.list[[4]] <- c("0",groups)
    names(par.list)[4] <- "groups"
    ## Gruppen anzeigen.
    print("Your population consists of the following groups:")
    print(par.list$groups)

    yesno <- readline("Are all main effects independently distributed among the population? Answer 'y' or 'n': ")
    yesno <- "n"
    if (yesno == "y")
      {
      }
    if (yesno == "n")
      {
        print("Well, then get the your population percentages ready...")
        ## Vektor der Prozente einrichten. -1 wird unten für die while-Schleife benutzt.
        perc <- rep( -1, length( par.list$groups ) )
        
        for (i in 2:length( par.list$groups ) )
          {
            
            while ( perc[i] < 0 )
              {
                perc[i] <- as.double(
                                     readline(
                                              cat("What part of the population does have ONLY factor(s)",
                                                    par.list$groups[i],
                                                    "?\n Please enter as e.g. 0.17 for 17 per cent: ")
                                              )
                                     )
                if ( perc[i] > 1)
                  perc[i] <- -1
              }# Prozentsatz i einlesen.
            
            if ( sum( perc[-1] ) > 1 )
              stop("You fucked up. Your population is more than 100 per cent.")
          }
        perc[1] <- 1 - sum( perc[-1] )
      }
    par.list[[5]] <- perc
    names( par.list )[5] <- "pop.percentages"
    
    print( t(t(paste(par.list$groups,par.list$pop.percentages,sep=" : "))) )
    
    rr <- rep(-1, length( par.list$groups ) )
    
    for (i in 2:length( par.list$groups ) )
      {  
        rr[i] <- as.double(
                           readline(
                                    cat("What is the relative risk of the population\n","with ONLY factor(s)",
                                          par.list$groups[i],
                                          "?\n Please enter as e.g. 2.5 for  2.5 times risk compared to zero-group: ")
                                    )
                           )
        if ( rr[i] < 0)
          perc[i] <- -1
      }# RR i einlesen.
    rr[1] <- 1
  
    par.list[[6]] <- rr
    names( par.list )[6] <- "relative.risks"

    print( t(t(paste(par.list$groups,par.list$rr,sep=" : "))) )

    PaR <- build.population( par.list[[1]], par.list[[2]], par.list[[3]], par.list[[5]][-1], par.list[[6]][-1] )
    
    return( PaR )
  }
