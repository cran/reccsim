rccs <- function(
                 PaR,
                 ctc = 5,
                 requireAllGroups.cases = FALSE,
                 requireAllGroups.controls = FALSE
                 )
  {
    cases <- make.cases( PaR, requireAllGroups.cases )
    controls <- make.controls( PaR, cases, ctc, requireAllGroups.controls )
    ccs <- make.case.control.study( PaR, cases, controls )
    
    return(ccs)
  }
