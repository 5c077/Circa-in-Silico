# ui.R
function(request)  {

  fluidPage(
    titlePanel(title=HTML("<strong>Circa in Silico <small>(alpha)</small></strong>"), windowTitle="CircaInSilico"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId="batch", "Transcripts", min = 10, max = 500, value = 128),
        numericInput(inputId="replicates", "Replicates", min=1,max=5000,value=4),
        numericInput(inputId="timepoints", "Timepoints:", min=3,max=9999, value=48),
        numericInput(inputId="cos.amplitude.max", "Maximum Amplitude", min =1, max = 10, value = 6),
        numericInput(inputId="cos.amplitude.min", "Minimum Amplitude", min =0, max = 10, value = 1),
        numericInput(inputId="outlier.amplitude", "Outlier Amplitude", min =0, max = 10, value = 0),
        numericInput(inputId="cos.period.max", "Maximum Period Length", min = 1, max = 100, value = 30),
        numericInput(inputId="cos.period.min", "Minimum Period Length", min = 1, max = 100, value = 20),
        numericInput(inputId="sample", "Sample Interval",min=1, max=10, value=1)
        #numericInput("seed", "set.seed", min = 1, max = 20, value = 9),
        ),
    mainPanel( downloadButton("downloadData", "Download" ),bookmarkButton())),
    
    wellPanel(helpText( a("Need help getting started?", target="_blank", href="https://github.com/5c077/Circa-in-Silico"))))}