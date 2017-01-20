# server.R
function(input, output, session)  observe({
    
    batch <-input$batch                # larger batch for statistically significant testing
    records <- c(input$replicates*batch) #8*batch
    timepoints <- input$timepoints
    cos.amplitude.max <- input$cos.amplitude.max      # maximum Affymetrix log2 oscillation amplitude
    cos.amplitude.min <- input$cos.amplitude.min      # generally invisible by eye
    cos.period.min <- input$cos.period.min
    cos.period.max <- input$cos.period.max
    outlier.amplitude <- input$outlier.amplitude     # same as before so higher amplitudes can be more reliable
  
  set.seed(111)               # initialize RNG so we can compare results (previously 7)
    #############################################################################################
  noise <- rnorm(records*timepoints)
    dim(noise) <- c(records,timepoints) #SAME dim AS "OUTLIERS" (ie. this is (columns*rows) in the .csv)

  outliers.pos <- rep(c(outlier.amplitude,rep(0,timepoints-1)),input$replicates*0.25*batch) #2 * batch)
    outliers.neg <- rep(c(-outlier.amplitude,rep(0,timepoints-1)),input$replicates*0.25*batch) #2 * batch)
    outliers.non <- rep(0, input$replicates*0.5*batch*timepoints)#4 * batch*timepoints)
    outliers <- c(outliers.pos,outliers.neg,outliers.non)
    dim(outliers) <- c(timepoints,records)        #SAME dim AS NOISE
    outliers <- t(apply(outliers,2,sample))       # randomize outlier timing
    outliers <- outliers[sample(1:records),]      # randomize outlier type (pos, neg, non)
    #############################################################################################
  cos.amplitudes <- c(runif(#4*batch
                            input$replicates*0.5*batch,cos.amplitude.min,cos.amplitude.max),rep(0,input$replicates*0.5))#4*batch))
    cos.periods <- c(runif(#4*batch
                         input$replicates*0.5*batch,cos.period.min,cos.period.max),rep(1,input$replicates*0.5))#4*batch))
    cos.lag.factors <- c(runif(#4*batch
                             input$replicates*0.5*batch),rep(0,input$replicates*0.5*batch))#4*batch))
    cos.parameters <- as.matrix(cbind(cos.amplitudes,cos.periods,cos.lag.factors*cos.periods,rowSums(outliers)))
  
  signals <- t(apply(cos.parameters,1,function(par) {
    par[1]*cos((1:timepoints -1 -par[3])*2*pi/par[2])}))
    shuffle <- sample(1:records)
    signals <- signals[shuffle,]
  
  cos.amplitudes <-cos.amplitudes[shuffle]
    cos.periods <-cos.periods[shuffle]
    cos.lag.factors <- cos.lag.factors[shuffle]
    cos.parameters <- cos.parameters[shuffle,]
  
  test.data4 <- signals +noise +outliers
    output1<-data.frame(test.data4, cos.amplitudes, cos.periods, cos.lag.factors)

    output$downloadData<-downloadHandler(filename = function(){ paste('CIS_data_', Sys.Date(), '.csv', sep='')}, content=function(file) {write.csv(output1, file)}) 
  
enableBookmarking = "url"})
