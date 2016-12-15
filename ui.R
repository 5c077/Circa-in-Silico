# ui.R
  
fluidPage(
    
    titlePanel(title=h2("Circa in Silico (alpha)"), windowTitle="CircaInSilico"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId="batch", "Batch", min = 10, max = 500, value = 128),
        numericInput(inputId="records", "Records", min=1,max=5000,value=1024),
        numericInput(inputId="timepoints", "Timepoints:", min=24,max=4800, value=48),
        numericInput(inputId="cos.amplitude.max", "Maximum Amplitude", min = 1, max = 10, value = 6),
        numericInput(inputId="cos.amplitude.min", "Minimum Amplitude", min = 1, max = 10, value = 1),
        numericInput(inputId="outlier.amplitude", "Outlier Amplitude", min = 0, max = 10, value = 0),
        numericInput(inputId="cos.period.max", "Maximum Period Length", min = 1, max = 100, value = 30),
        numericInput(inputId="cos.period.min", "Minimum Period Length", min = 1, max = 100, value = 20)
        #numericInput("seed", "set.seed", min = 1, max = 20, value = 9),
        ),
    mainPanel( downloadButton("downloadData", "Download"), h1("<Citations>"))))