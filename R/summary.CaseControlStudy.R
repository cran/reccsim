summary.CaseControlStudy <- function( object, ... )
  {
    cc <- object
    endog <- cc[[1]][[1]][[2]]
      
    flattable <- ftable(xtabs(as.formula(paste(endog,"~.")),cc[[2]]))
    print(cc[[1]][[1]])
    print(flattable)
    print(cc[[1]][[4]])
  }

