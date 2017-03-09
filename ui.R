# ui.R
function(request)  {

  fluidPage(
    tags$head(includeScript("google-analytics.js")),
    titlePanel(title=HTML("<strong>Circa in Silico <small>(beta)</small></strong>"), windowTitle="CircaInSilico"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId="timepoints", "Duration", min=1,max=72, value=48),
        numericInput(inputId="batch", "Time Series", min = 10, max = 500, value = 128),
        numericInput(inputId="replicates", "Number of Independant Samples (Replicates)", min=1,max=9999,value=1),
        numericInput(inputId="sample", "Sampling Interval",min=1, max=12, value=1),
        numericInput(inputId="cos.amplitude.max", "Maximum Amplitude", min =1, max = 10, value = 6),
        numericInput(inputId="cos.amplitude.min", "Minimum Amplitude", min =0, max = 10, value = 1),
        numericInput(inputId="outlier.amplitude", "Outlier Amplitude", min =0, max = 10, value = 0),
        numericInput(inputId="cos.period.max", "Maximum Period Length", min = 1, max = 100, value = 24),
        numericInput(inputId="cos.period.min", "Minimum Period Length", min = 1, max = 100, value = 24),
        sliderInput(inputId="rhythm", label="Percent Rhythmic", min=0, max=100, value=50)
        #numericInput("seed", "set.seed", min = 1, max = 20, value = 9),
        ),
    mainPanel(checkboxInput("cat", "Replicate by Concatenation?", value = FALSE),checkboxInput("sort.by", "Sort by Replicate?", value = FALSE), downloadButton("downloadData", "Download"),bookmarkButton(), h3(textOutput("calc.timepoints"), plotOutput("average_graph"))
    )),
    
    wellPanel(helpText( a("Need help getting started?", target="_blank", href="https://github.com/5c077/Circa-in-Silico/blob/master/README.md"))))}
