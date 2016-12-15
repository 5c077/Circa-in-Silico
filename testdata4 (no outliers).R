
batch <- 128                # larger batch for statistically significant testing
records <- 8*batch
timepoints <- 48
cos.amplitude.max <- 6      # maximum Affymetrix log2 oscillation amplitude
cos.amplitude.min <- 1      # generally invisible by eye
cos.period.min <- 20
cos.period.max <- 30
outlier.amplitude <- 0     # same as before so higher amplitudes can be more reliable

set.seed(111)               # initialize RNG so we can compare results (previously 7)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
noise <- rnorm(records*timepoints)
dim(noise) <- c(records,timepoints)

outliers.pos <- rep(c(outlier.amplitude,rep(0,timepoints-1)),2*batch)
outliers.neg <- rep(c(-outlier.amplitude,rep(0,timepoints-1)),2*batch)
outliers.non <- rep(0,4*batch*timepoints)
outliers <- c(outliers.pos,outliers.neg,outliers.non)
dim(outliers) <- c(timepoints,records)
outliers <- t(apply(outliers,2,sample))       # randomize outlier timing
outliers <- outliers[sample(1:records),]      # randomize outlier type (pos, neg, non)

cos.amplitudes <- c(runif(4*batch,cos.amplitude.min,cos.amplitude.max),rep(0,4*batch))
cos.periods <- c(runif(4*batch,cos.period.min,cos.period.max),rep(1,4*batch))
cos.lag.factors <- c(runif(4*batch),rep(0,4*batch))
cos.parameters <- as.matrix(cbind(cos.amplitudes,cos.periods,cos.lag.factors*cos.periods,rowSums(outliers)))
signals <- t(apply(cos.parameters,1,function(par) {
           par[1]*cos((1:timepoints -1 -par[3])*2*pi/par[2])
}))
shuffle <- sample(1:records)
signals <- signals[shuffle,]

#################
cos.amplitudes <-cos.amplitudes[shuffle]
cos.periods <-cos.periods[shuffle]
cos.lag.factors <- cos.lag.factors[shuffle]
#################

cos.parameters <- cos.parameters[shuffle,]

test.data4 <- signals +noise +outliers
output<-data.frame(test.data4, cos.amplitudes, cos.periods, cos.lag.factors)
write.table(output, "test_data_output4.csv", sep=",")

