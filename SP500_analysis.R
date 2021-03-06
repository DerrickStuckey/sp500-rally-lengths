#Derrick Stuckey
#derrickstuckey@gmail.com
#S&P 500 rally/correction analysis

library(MASS)

#Constants
correctionFactor = 0.9
rallyFactor = (1.0/0.9)
rallyType = "rally"
correctionType = "correction"

sp500_history <- read.csv("./sp500_history.csv")
#sort the data chronologically
sp500_history <- sp500_history[rev(rownames(sp500_history)),]

#run type = "rally" or "correction"
#data series begins in a rally (determined manually)
runs <- data.frame("StartDate"=sp500_history$Date[1], "EndDate"=sp500_history$Date[1], 
                   "StartVal"=sp500_history$High[1], "EndVal"=sp500_history$High[1],
                   "Type"=rallyType)

inCorrection <- function(runs,j) {runs$Type[j] == correctionType}

#partition time period into a set of rallies and a set of corrections
i <- 1
j <- 1
for (i in 1:dim(sp500_history)[1]) {
  if (inCorrection(runs,j)) {
    #the current run is a correction
    if (sp500_history$Low[i] <= runs$EndVal[j]) {
      #a new low is reached
      runs$EndVal[j] <- sp500_history$Low[i]
      runs$EndDate[j] <- sp500_history$Date[i]
    } else {
      if (sp500_history$High[i] > rallyFactor*runs$EndVal[j]) {
        #a new rally has begun
        nextRun <- data.frame("StartDate"=runs$EndDate[j], "EndDate"=sp500_history$Date[i], 
                     "StartVal"=runs$EndVal[j], "EndVal"=sp500_history$High[i],
                     "Type"=rallyType)
        runs <- rbind(runs, nextRun)
        j <- j+1
      }
    }
  } else {
    #the current run is a rally
    if (sp500_history$High[i] >= runs$EndVal[j]) {
      #a new high is reached
      runs$EndVal[j] <- sp500_history$High[i]
      runs$EndDate[j] <- sp500_history$Date[i]
    } else {
      if (sp500_history$Low[i] < correctionFactor*runs$EndVal[j]) {
        #a new correction has begun
        nextRun <- data.frame("StartDate"=runs$EndDate[j], "EndDate"=sp500_history$Date[i], 
                     "StartVal"=runs$EndVal[j], "EndVal"=sp500_history$Low[i],
                     "Type"=correctionType)
        runs <- rbind(runs, nextRun)
        j <- j+1
      }
    }
  }
}

rallies <- runs[runs$Type == rallyType,]
corrections <- runs[runs$Type == correctionType,]

#trim the 1st rally as we don't have its beginning captured
rallies <- rallies[2:dim(rallies)[1],]

#calculate the length of each rally
rallyLengths <- as.numeric(rallies$EndDate) - as.numeric(rallies$StartDate)

#fit and plot exponential distribution
rallyLengthsExpEst <- fitdistr(rallyLengths, "exponential")$estimate
hist(rallyLengths, breaks=30, prob=TRUE, main="Probability Density of Rally Lengths", xlab="Rally Length (Days)", ylab="Probability")
curve(dexp(x, rate = rallyLengthsExpEst), add = TRUE, col = "blue", lwd = 2)

#fit and plot weibull distribution
weibullFit <- fitdistr(rallyLengths, "weibull")
curve(dweibull(x, shape=weibullFit$estimate[1], scale=weibullFit$estimate[2]), add = TRUE, col = "red", lwd = 2)

#fit log normal dist and plot
#lognormFit <- fitdistr(rallyLengths, "lognormal")
#curve(dlnorm(x, meanlog=lognormFit$estimate[1], sdlog=lognormFit$estimate[2]), add = TRUE, col = "green", lwd = 2)

#draw example normal curve
curve(dnorm(x,mean=3,sd=1),from=0,to=6,col="blue",xlab="Length of Rally",ylab="Probability",frame.plot=TRUE,axes=FALSE)
#draw example log-normal curve
curve(dlnorm(x,meanlog=2,sdlog=0.5),from=0,to=25,col="blue",xlab="Length of Rally",ylab="Probability",frame.plot=TRUE,axes=FALSE)
#draw example log-normal curve
curve(dexp(x,rate=1),from=0,to=6,col="blue",xlab="Length of Rally",ylab="Probability",frame.plot=TRUE,axes=FALSE)

