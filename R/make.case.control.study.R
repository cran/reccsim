make.case.control.study <- function( PaR, cases, controls )
  {
    cases.controls <- rbind( cases, controls )
    case.control <- list( PaR, cases.controls )
    class( case.control ) <- "CaseControlStudy"
    return( case.control )
  }


