## The 'Zelig' package is neccessary and will be loaded if available.
## If it is not available, please install it and retry.
readline("Press enter to continue.")      

require(Zelig)
require(reccsim)
rm(list=ls())
graphics.off()

## Set risk model
## Here the outcome Y depends on two exogenous factors A and B

risk.model <- Y~A+B

## Set the overall population risk.
## This is used as an restriction to compute the risks among the population groups
pop.risk <- 10^(-7)

## risk.model defines the population groups.
## For Y~A+B these groups are:
## 0 : exhibist neither A nor B
## A : exhibits only A
## B : exhibits only B
## AB: exhibits A and B
## These groups can be viewed by using get.groups( risk.model )
## From the percentages for the non-0 groups the percentage
## for group 0 is automatically computed.
pop.perc <- c(.20,.20,.10)

## The population can now be represented as a table:
##
## |A/B |  0  |  1  |
## |----------------|
## | 0  | 0.5 | 0.2 |
## | 1  | 0.2 | 0.1 |
## 

## What is the overall population size?
pop.size <- 1000 * 10^6

## We are dealing with a population of 1 billion where the risk factors
## A and B are distributed according to this table:
##
## |A/B |  0  |  1  |
## |----------------|
## | 0  | 0.5 | 0.2 |
## | 1  | 0.2 | 0.1 |
##
readline("Press enter to continue.")
##
##Random case-control studies will now be drawn from this population.
## To make it interesting, the relative risks between the groups are altered.
## For group '0' the rr ist set at 1. For group 'A' the rr is varied from
## 1 to 10 with stepsize 1. The rr of group 'B' is from the set {1,2,5,10}.
## All possible rr combinations are tried. No interaction effects are modeled,
## therefore the rr of group 'AB' is (very closely) approximated by rr_A * rr_B.
## Overall population risk is set as one in ten millions.
## For each case generated five random controls are drawn.
## All of this is done 10 times for each set of relative risks.
readline("Press enter to start the simulation. Progress will be shown.")

## What CTC (cases-to-controls) ratio shall be used?
## E.g. a CTC of 5 means, for each case five random controls are drawn
## and added to the study.
CTC <- 5

## How many simulations shall be conducted for each set of relative risks?
trials <- 10

## Now we define the variation of the relative risks for the two
## single factor groups.
## This means for group A we try all relative risks (relative to the 0 group)
## from 1 (meaning the same risk as the zero group) to 10 with a stepsize of 1.
## For the B group we try a few chosen relative risks.
rr.A <- seq( from = 1, to = 10, by = 1)
rr.B <- c(1,2,5,10)

## Now we set up the grid with all possible combinations of relative
grid <- expand.grid( rr.A, rr.B )

## For reasonably small probabilities the product of the individual
## relative risks approximates (very closely) the combined relative risk.
grid <- cbind(grid,apply(grid,1,prod))
names(grid)<-c("A","B","A:B")

## Cleanup
rm(rr.A)
rm(rr.B)

## We want to save our simulation results for each set of relative risks.
## All values in these arrays are arbitrarily but conveniently (see below)
## set to one.
model.estimates <- array(
                         1,
                         c( trials, 15, nrow(grid) )
                         )


## Show the number of rr-sets to the user
cat( "There are ",nrow( grid )," different sets of relative risks.\n" )
flush.console()

## Initialize a progress bar to show how many of the rr-sets are done.
pbi <- txtProgressBar(min = 1, max = nrow(grid), style = 3)
i <- 1
setTxtProgressBar(pbi, i)

## Initialize a progress bar to show how many trials of one
## rr-set are done
pbj <- winProgressBar(min = 1, max = trials)

## Now we go through all rr-sets in 'grid':
for (i in 1:nrow( grid ) ){
  
  ## first we create the PopulationAtRisk object by using build.population()
  PaR <- build.population( risk.model, pop.size, pop.risk, pop.perc, unlist( grid[i,] ) )

  ## now from this PopulationAtRisk we draw 'trials' random case-control studies
  for ( j in 1:trials )
    {
      ## There are extremely rare occurences where King's correction seems to produce completely
      ## unreasonable extimates.
      setWinProgressBar(pbj, j)
      
      ## Zelig enlightens us with its citation recommendation each time it is invoked.
      ## This gets boring soon, so I sink these outputs.
      sink("DeleteMe.txt")

      ## This will always be the case prior to estimation
      ## as all these values are set to one.
      while ( model.estimates[j,1,i] > 0 ){
        
        ## Here the case-control study is randomly generated.
        ## It is an object of type CaseControlStudy.
        cc <- rccs( PaR, CTC, requireAllGroups.cases = TRUE, requireAllGroups.controls = TRUE )

        ## With 'tau = pop.risk' it is assumed we correctly estimated
        ## the population risk.
        my.glm <- zelig(
                        Y ~ A+B,
                        data = cc[[2]],
                        model = "relogit",
                        tau = pop.risk,
                        bias.correct = T
                        )
        
        ## For the models.AB.* the estimated coefficients are stored
        ## in the [,1:3,] positions of the arrays.
        model.estimates[j,1:3,i] <- summary(my.glm)$coefficients[,1]

        ## The [,4:6,] positions are filled with the corresponding p-values.
        model.estimates[j,4:6,i] <- summary(my.glm)$coefficients[,4]

        ## The [,7:15,] positions are filled with the covariance matrix
        ## of the estimators.
        model.estimates[j,7:15,i] <- as.vector(summary(my.glm)$cov.unscaled)

        ## There are rare occurences where King's rare events correction
        ## produces very large positive regression estimates
        ## for the intercept when they should be well below zero.
        ## We filter those occurences here and inform the user:
        if (model.estimates[j,1,i]>0){
          sink()
          warning("Singularity detected. Retrying.", immediate. = T)
          sink("DeleteMe.txt")
        }
      }
      sink() # finish sinking.
    } # all 'trials' are done for this set of relative risks
  unlink("DeleteMe.txt") # delete the sink file
  setTxtProgressBar(pbi, i)
} # Ende des grid-Eintrages
close(pbi)
close(pbj)
cat("\n")

## The estimation results are now saved in model.estimates.
## For a description of the measures in model.estimates have a look
## at the source code.
## This allows us to visualize how the estimator behaves for
## different relative risk sets.
## A plot will now be generated where the average estimate of the linear effect
## of factor A is plotted dependent on group A's rr and group B's rr.
readline("Press enter to plot.")

## Now we can visualize how the estimation of the slope
## parameter for factor A behaves with regard to
## the actual relative risk of group A and the relative
## risk of group B.

require(lattice)
require(grid)

trellis.device()
plot1 <- xyplot( apply(model.estimates[,2,],2,mean) ~ grid[,1],
       groups=grid[,2], # We group by the different relative risks in B.
       type="l",
       plot.points=T,
       ref=T,
       xlab=expression(pi[A]/pi[0]),
       lty=1:4,
       col=1:4,
       key = list(
         space="right",
         title=expression(pi[B]/pi[0]),
         text=list(c("1","2","5","10")),
         lines=list(lty=1:4,col=1:4),
         points=F
         ),
       main="Factor A estimates",
       ylab=expression( bar( hat(beta) )[A] ),
       )
print(plot1)
