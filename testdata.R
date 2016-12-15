batch <- 25
records <- 4*batch          # small dataset for quick results
timepoints <- 48
cos.amplitude <- 2          # large enough so most records "look" cyclic
cos.period.min <- 20
cos.period.max <- 30
outlier.amplitude <- 10     # strong enough to disrupt Fisher's g-test

set.seed(9)                 # initialize RNG so we can compare results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outliers.pos <- rep(c(outlier.amplitude,rep(0,timepoints-1)),batch)
outliers.neg <- rep(c(-outlier.amplitude,rep(0,timepoints-1)),batch)
outliers.non <- rep(0,2*batch*timepoints)
outliers <- c(outliers.pos,outliers.neg,outliers.non)
outliers <- t(apply(outliers,2,sample))       # randomize outlier timing
outliers <- outliers[sample(1:records),]      # randomize outlier type (pos, neg, non)
dim(outliers) <- c(timepoints,records)

cos.lag.factors <- runif(records)
cos.periods <- runif(records,cos.period.min,cos.period.max)
cos.parameters <- as.matrix(cbind(cos.periods,cos.lag.factors*cos.periods,rowSums(outliers)))
signals <- t(apply(cos.parameters,1,function(par) {
  cos.amplitude*cos((1:timepoints -par[2])*2*pi/par[1])
}))

noise <- rnorm(records*timepoints)
dim(noise) <- c(records,timepoints)

test.data <- signals +noise +outliers
