library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Market Basket Analysis-verudix"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.samp=='Sample'",
        numericInput("nrule", 'Number of Rules', 5), br()
      ),
      conditionalPanel(
        condition = "input.mytab=='graph'",
        radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
      ),
      
      conditionalPanel(
        condition = "input.lhsv=='Subset'", 
        uiOutput("choose_lhs"), br()
      ),
      
      conditionalPanel(
        condition = "input.rhsv=='Subset'", 
        uiOutput("choose_rhs"), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab=='grouped'",
        sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')", 
        radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
        uiOutput("choose_columns"), br(),
        sliderInput("supp", "Support:", min = 0, max = 1, value = 0.1 , step = 1/10000), br(),
        sliderInput("conf", "Confidence:", min = 0, max = 1, value = 0.5 , step = 1/10000), br(),
        selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
        numericInput("minL", "Min. items per set:", 2), br(), 
        numericInput("maxL", "Max. items per set::", 3), br(),
        radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
        radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br(),
        downloadButton('downloadData', 'Download Rules as CSV')
      )
      #sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                  tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                  tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                  tabPanel('Parallel Coordinates', value='paracoord', plotOutput("paracoordPlot", width='100%', height='100%')),
                  tabPanel('Matrix', value='matrix', plotOutput("matrixPlot", width='100%', height='100%')),
                  tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                  tabPanel('Table', value='table', verbatimTextOutput("rulesTable")),
                  tabPanel('Data Table', value='datatable', dataTableOutput("rulesDataTable"))
      )
      #plotOutput("distPlot")
    )
  )
))
