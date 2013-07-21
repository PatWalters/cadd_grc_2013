# confidence_plots.R
# Pat Walters wpwalters@gmail.com
# This script generates a nice set of plots showing the relationship between Pearson's r, the number of datapoints, and the 95% confidence interval.  This might make a nice t-shirt

# The script requires the psychometric library, if you don't have this library, it can be installed using
# install.packages("psychometric")

# To run the script, execute the following commands in R
# source("confidence_plots.R")
# makeFigure(0)

library(psychometric)

makeFigure <- function(start){
      par(mfrow=c(2,2))
      plotCI(start,0.6)
      plotCI(start,0.7)
      plotCI(start,0.8)
      plotCI(start,0.9)
}

plotCI <- function(start,r) {
      lower = c()
      upper = c()
      i = 1
      for (j in start:100){
          res = CIr(r,j,0.95)
	  lower[i] = res[1]
	  upper[i] = res[2]
	  i = i+1
      }
      plot(start:100,lower,type="l",xlim=c(0,100),ylim=c(0,1),xlab="Number of Points",ylab="95% Confidence Interval",main=paste("r = ",r))
      polygon(c(start:100,100:start),c(upper,rev(lower)),col="lightgrey")
      lines(start:100,upper)
      for (i in seq(0,100,10)) {
          abline(v=i,lty="dotted")
      }
      for (i in seq(0.0,1,0.1)) {
          abline(h=i,lty="dotted")
      }
}