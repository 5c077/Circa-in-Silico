function(input, output, session)  observe(
  {
  
  reps<-input$replicates
  sample.interval<-input$sample
  batch <-input$batch                # larger batch for statistically significant testing
  timepoints <- input$timepoints*input$replicates/input$sample
  records <- input$batch
  cos.amplitude.max <- input$cos.amplitude.max      # maximum Affymetrix log2 oscillation amplitude
  cos.amplitude.min <- input$cos.amplitude.min      # generally invisible by eye
  cos.period.min <- input$cos.period.min
  cos.period.max <- input$cos.period.max
  outlier.amplitude <- input$outlier.amplitude # same as before so higher amplitudes can be more reliable
  
  set.seed(111)               # initialize RNG so we can compare results (previously 7)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  noise <- rnorm(records*timepoints)
  noize<- matrix(noise, records, timepoints)  #dim(noise) <- c(records,timepoints) 
  
  outliers.pos <- rep(c(outlier.amplitude,rep(0,timepoints/reps*sample.interval-1)),batch*0.25*reps/sample.interval) 
  outliers.neg <- rep(c(-outlier.amplitude,rep(0,timepoints/reps*sample.interval-1)),batch*0.25*reps/sample.interval) 
  outliers.non <- rep(c(rep(0, timepoints/reps*sample.interval)),batch*0.5*reps/sample.interval)
  outliers <- c(outliers.pos,outliers.neg,outliers.non)
  outliers <- matrix(outliers, timepoints, records)        
  outliers <- t(apply(outliers,2,sample))       # randomize outlier timing
  outliers <- outliers[sample(1:records),]      # randomize outlier type (pos, neg, non)
  
  cos.amplitudes <- c(runif(
    4*batch,cos.amplitude.min,cos.amplitude.max),rep(0,batch*4))
  cos.periods <- c(runif(
    4*batch,cos.period.min,cos.period.max),rep(1,batch*4))
  cos.lag.factors <- c(runif(
    4*batch),rep(0,4*batch))
  cos.parameters <- as.matrix(cbind(cos.amplitudes,cos.periods,cos.lag.factors*cos.periods,rowSums(outliers)))
  signals <- t(apply(cos.parameters,1,function(par) {
             par[1]*cos((1:timepoints -1 -par[3])*2*pi/par[2])
  }))
  shuffle <- sample(1:records)
  signals <- signals[shuffle,]
  
  ####################
  cos.amplitudes <-cos.amplitudes[shuffle]
  cos.periods <-cos.periods[shuffle]
  cos.lag.factors <- cos.lag.factors[shuffle]
  ####################
  
  cos.parameters <- cos.parameters[shuffle,]
  
  test.data4 <-data.table(signals +noize +outliers)

  output1<-data.table(test.data4, cos.amplitudes, cos.periods, cos.lag.factors)
  colnames(output1)<- c(apply(expand.grid("Rep:",1:input$replicates, "Hr:", seq(from=sample.interval, to=input$timepoints, by=sample.interval)),1, paste, collapse = ""), "Cosine Amplitude", "Cosine Periods", "Cosine Lag Factors")#c(rep(1:input$replicates, input$timepoints))
  output$downloadData<-downloadHandler(filename = function(){ paste('CIS_data_', Sys.Date(), '.csv', sep='')}, content=function(file) {write.csv(output1, file)}) 
  
  enableBookmarking = "url"})