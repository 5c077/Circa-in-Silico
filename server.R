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
  timepoints <- input$timepoints*reps
  timepoints[is.na(timepoints)]<- 999
  timepoints[timepoints==0]<-2
  timepoints[timepoints<=1]<-2
  duration<-input$timepoints
  duration[is.na(duration)]<-5
  duration[duration<=sample.interval]<-sample.interval
  records <- input$batch
  records[is.na(records)]<- 5
  records[records==0]<-2
  records[records<=1]<-2
  cos.amplitude.max <- input$cos.amplitude.max      # maximum Affymetrix log2 oscillation amplitude
  cos.amplitude.min <- input$cos.amplitude.min      # generally invisible by eye
  cos.period.min <- input$cos.period.min
  cos.period.max <- input$cos.period.max
  outlier.amplitude <- input$outlier.amplitude # same as before so higher amplitudes can be more reliable
  percent.rhythmic<-input$rhythm/100
  subset<-seq(from=sample.interval, to=duration, by=sample.interval)
  display<-input$timepoints/input$sample
  display[is.na(display)]<-"?"
  output$calc.timepoints<-renderText({c("Timepoints =",display)})
  set.seed(111)               # initialize RNG so we can compare results (previously 7)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  noise <- rnorm(records*timepoints)
  noise<- matrix(noise, records, timepoints)  #dim(noise) <- c(records,timepoints) 
  
  outliers.pos <- rep(c(outlier.amplitude,rep(0,timepoints/reps-1)),batch*0.25*reps) 
  outliers.neg <- rep(c(-outlier.amplitude,rep(0,timepoints/reps-1)),batch*0.25*reps) 
  outliers.non <- rep(c(rep(0, timepoints/reps)),batch*0.5*reps)
  outliers <- c(outliers.pos,outliers.neg,outliers.non)
  outliers <- matrix(outliers, records, timepoints)        
  outliers <- apply(outliers,2,sample)       # randomize outlier timing
  outliers <- outliers[,sample(1:timepoints)]#[sample(1:records),]# randomize outlier type (pos, neg, non)
  
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
  names<-c(apply(expand.grid(seq(from=sample.interval, to=duration, by=sample.interval), 1:reps),1, paste, collapse="."),"Cosine Amplitude", "Cosine Periods", "Cosine Lag Factors")
  
  sample.interval[sample.interval>=ncol(test.data4)]<-ncol(test.data4)
  cal<-rep(subset,reps)*1:reps
    cal[cal>ncol(test.data4)]<-ncol(test.data4)
  cal.wrong<-rep(subset,reps)
    cal.wrong[cal.wrong>ncol(test.data4)]<-ncol(test.data4)
  if (input$cat==TRUE) {test.data5<-test.data4[, c(cal.wrong), with=FALSE]}
  else {test.data5<-test.data4[, c(cal), with=FALSE]}
  
  output1<-data.table(test.data5, cos.amplitudes, cos.periods, cos.lag.factors) %>%
    setNames(names)
  if (input$sort.by==FALSE) {setcolorder(output1, c(mixedsort(colnames(output1[,1:ncol(test.data5)]),decreasing = FALSE),"Cosine Amplitude", "Cosine Periods", "Cosine Lag Factors"))}
  
  
  output$downloadData<-downloadHandler(filename = function(){ paste('CIS_data_', Sys.Date(), '.csv', sep='')}, content=function(file) {write.csv(output1, file)}) 
 
  
  #output$average_graph<-renderPlot({ggplot(data=melted,aes(1, 1))+geom_line()+geom_point()})
  
  enableBookmarking = "url"
  })
