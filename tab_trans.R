library(shiny)
library(ggsignif)
library(DT)
library(ggplot2)
library(tidyverse)
library(shinyBS)
library(shinyjs)
library(colourpicker)
library(shinythemes)
library(plotly)

ui <- fluidPage(theme = shinytheme('flatly'),
                tabPanel(title = 'Transcriptome'),
                fluidPage(tabsetPanel(
                  tabPanel(title = 'Getting Started'),
                  tabPanel(title = 'Preview your data',br(),
                           fluidPage(sidebarLayout(
                             sidebarPanel(width = 3,
                                          titlePanel(title = 'Upload your data'),
                                                     downloadLink(outputId = 'trans_download_example',
                                                                  label = 'Download example data'),
                                                     hr(),
                                          radioButtons(inputId = 'trans_upload_type','Choose your data type',
                                                       choices = c('Expression data: Gene counts'='trans_gene_count',
                                                                   'Normalized data: FPKM/TPM etc.'='trans_normalized_data',
                                                                   'Microarray'='microarray'
                                                                   ))

                                          ),
                             mainPanel(
                               fluidPage(
                                 tabsetPanel(
                                   tabPanel(title = 'View your data',
                                            br(),
                                            fileInput('trans_datafile', 
                                                      'Choose your file (.csv/txt)',
                                                      buttonLabel = 'Upload',
                                                      placeholder = 'Upload data here',
                                                      accept=c( 'text/csv',
                                                                'text/comma-separated-values',
                                                                'text/tab-separated-values',
                                                                'text/plain',
                                                                '.csv',
                                                                '.tsv')),
                                            hr(),
                                            DT::dataTableOutput(outputId = 'trans_datafile'),
                                            hr(),
                                            plotOutput(outputId = 'trans_preview')
                               
                               
                               
                             )
                           )
                           
                           
                           )
                           
                           )))),
                  tabPanel(title = 'Pre-processing'),
                  tabPanel(title = 'Principle Component Analysis'),
                  tabPanel(title = 'Diffential Analysis'),
                  tabPanel(title = 'Heatmap'),
                  tabPanel(title = 'Functional Annotation')

                  
                ))

  
)

server <- function(input, output, session) {
  
 Trans <- reactive({
      transfile <- input$trans_datafile
      req(transfile)
      transfile <- transfile$datapath
      x <- read.csv(transfile,header = T)
      if(dim(x)[2] == 1) 
        x <- read.csv(transfile,header = T,sep='\t')
     return(x)
    })
output$trans_datafile <- DT::renderDataTable({
  req(Trans())
  Trans()[,1:6]
})

output$trans_preview <- renderPlot({
  req(Trans())
  boxplot(Trans()[,-1],col = "red",xaxt = "n",outline = F)
})


}



shinyApp(ui, server)



