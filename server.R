function(input, output, session)  observe(
  {
  reps<-input$replicates
  reps[is.na(reps)]<- 1
  reps[reps<=1]<-1
  sample.interval<-input$sample
  sample.interval[is.na(sample.interval)]<- 1
  sample.interval[sample.interval<=1]<-1
  batch <-input$batch # larger batch for statistically significant testing
  batch[is.na(batch)]<- 1
  batch[batch<=1]<-1
  timepoints <- input$timepoints*reps/sample.interval
  timepoints[is.na(timepoints)]<- 999
  timepoints[timepoints==0]<-2
  timepoints[timepoints<=1]<-2
  records <- input$batch
  records[is.na(records)]<- 999
  records[records==0]<-2
  records[records<=1]<-2
  cos.amplitude.max <- input$cos.amplitude.max      # maximum Affymetrix log2 oscillation amplitude
  cos.amplitude.min <- input$cos.amplitude.min      # generally invisible by eye
  cos.period.min <- input$cos.period.min
  cos.period.max <- input$cos.period.max
  outlier.amplitude <- input$outlier.amplitude # same as before so higher amplitudes can be more reliable
  percent.rhythmic<-input$rhythm/100
  display<-input$timepoints/input$sample
  display[is.na(display)]<-"?"
  
  output$calc.timepoints<-renderText({c("Timepoints =",display)})
  set.seed(111)               # initialize RNG so we can compare results (previously 7)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  noise <- rnorm(records*timepoints)
  noise<- matrix(noise, records, timepoints)  #dim(noise) <- c(records,timepoints) 
  
  outliers.pos <- rep(c(outlier.amplitude,rep(0,timepoints/reps*sample.interval-1)),batch*0.25*reps/sample.interval) 
  outliers.neg <- rep(c(-outlier.amplitude,rep(0,timepoints/reps*sample.interval-1)),batch*0.25*reps/sample.interval) 
  outliers.non <- rep(c(rep(0, timepoints/reps*sample.interval)),batch*0.5*reps/sample.interval)
  outliers <- c(outliers.pos,outliers.neg,outliers.non)
  outliers <- matrix(outliers, timepoints, records)        
  outliers <- t(apply(outliers,2,sample))       # randomize outlier timing
  outliers <- outliers[sample(1:records),]# randomize outlier type (pos, neg, non)
  
  #outliers<-outliers[,seq(from=sample.interval, to=timepoints, by=sample.interval)]
  
  cos.amplitudes <- c(runif(
    percent.rhythmic*batch,cos.amplitude.min,cos.amplitude.max),rep(0,batch))
  cos.periods <- c(runif(
    percent.rhythmic*batch,cos.period.min,cos.period.max),rep(1,batch))
  cos.lag.factors <- c(runif(
    percent.rhythmic*batch),rep(0,batch))
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
  
  test.data4 <-data.table(signals +noise +outliers)
  #test.data4<-test.data4[,seq(from=sample.interval, to=timepoints, by=sample.interval),with=FALSE]
  #test.data4<-test.data4[,seq(from=reps, to=timepoints, by=reps),with=FALSE]

  output1<-data.table(test.data4, cos.amplitudes, cos.periods, cos.lag.factors) %>%
    setNames(c(apply(expand.grid("Rep:", 1:reps,"Hr:",seq(from=sample.interval, to= timepoints/reps*sample.interval, by=sample.interval)),1, paste, collapse=" "),"Cosine Amplitude", "Cosine Periods", "Cosine Lag Factors"))
  
  #graph<-rowMeans(test.data4)
  #graph<-data.frame(test.data4)
  
  #output$average_graph<-renderPlot({qplot("points", "expr",data=graph, geom="line")})
  #colnames(output1)<- c(apply(expand.grid(seq(from=sample.interval, to=input$timepoints, by=sample.interval),seq(from=1, to=reps, by=1)),1, paste, collapse = "."), "Cosine Amplitude", "Cosine Periods", "Cosine Lag Factors")#c(rep(1:input$replicates, input$timepoints))
  output$downloadData<-downloadHandler(filename = function(){ paste('CIS_data_', Sys.Date(), '.csv', sep='')}, content=function(file) {write.csv(output1, file)}) 
  
  enableBookmarking = "url"
  })
