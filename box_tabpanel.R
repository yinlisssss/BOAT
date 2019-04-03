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
  tabPanel(title = 'Box Plots',
                        fluidPage(
                          tabsetPanel(
                            tabPanel(title = 'Introduction'),
                            tabPanel(title = 'Two-group comparison',
                                     br(),
                                     fluidPage(sidebarLayout(
                                       sidebarPanel(width = 3,
                                         titlePanel(title = 'Upload Data'),
                                         hr(),
                                         h4('Tips'),
                                         tags$ul(
                                           tags$li("MOAT accepts csv/txt files"), 
                                           tags$li("File must have a header")),
                                         radioButtons(inputId = 'boxstyle',label = 'Choose style',
                                                      choices = c(`Style 1 (Box)`='boxstyle1',
                                                                  `Style 2 (Box+Jitter)`='boxstyle2',
                                                                  `Style 3 (Box+Violin)`='boxstyle3')),
                                         conditionalPanel(condition = "input.boxstyle =='boxstyle1'",
                                                          textInput(inputId = 'box_style1_main_title','Main title'),  
                                                          sliderInput(inputId = 'box_style1_main_title_size','Main title size',
                                                                      min = 0,
                                                                      max = 30,
                                                                      step = 1,
                                                                      value = 10),
                                         textInput(inputId = 'box_style1_legend_title','Legend title'),
                                         sliderInput(inputId = 'box_style1_legend_title_size','Legend title size',
                                                     min = 0,
                                                     max = 30,
                                                     step = 1,
                                                     value = 10),
                                         sliderInput(inputId = 'box_style1_legend_size','Legend size',
                                                     min = 0,
                                                     max = 30,
                                                     step = 1,
                                                     value = 10),sliderInput(inputId = 'box_style1_width','Box width',
                                                     min = 0,
                                                     max = 1,
                                                     step = 0.01,
                                                     value = 0.3),
                                         selectInput(inputId = 'box_style1_statistical_method','Statistical method',
                                                     choices = c('Student\'s t test'='t.test',
                                                                 'Wilcox test'='wilcox.test'
                                                                 ))
                                       ),
                                       conditionalPanel(condition = "input.boxstyle =='boxstyle2'",
                                                        textInput(inputId = 'box_style2_main_title','Main title'),  
                                                        sliderInput(inputId = 'box_style2_main_title_size','Main title size',
                                                                    min = 0,
                                                                    max = 30,
                                                                    step = 1,
                                                                    value = 10),
                                                        textInput(inputId = 'box_style2_legend_title','Legend title'),
                                                        sliderInput(inputId = 'box_style2_legend_title_size','Legend title size',
                                                                    min = 0,
                                                                    max = 30,
                                                                    step = 1,
                                                                    value = 10),
                                                        sliderInput(inputId = 'box_style2_legend_size','Legend size',
                                                                    min = 0,
                                                                    max = 30,
                                                                    step = 1,
                                                                    value = 10),
                                                        sliderInput(inputId = 'box_style2_width','Box width',
                                                                                            min = 0,
                                                                                            max = 1,
                                                                                            step = 0.01,
                                                                                            value = 0.3),
                                                        sliderInput(inputId = 'box_style2_point_width','Point width',
                                                                    min = 0,
                                                                    max = 1,
                                                                    step = 0.01,
                                                                    value = 0.1),
                                                        selectInput(inputId = 'box_style2_statistical_method','Statistical method',
                                                                    choices = c('Student\'s t test'='t.test',
                                                                                'Wilcox test'='wilcox.test'
                                                                    ))
  
                                                        
                                                        
                                                        ),
                                       conditionalPanel(condition = "input.boxstyle =='boxstyle3'",
                                                        textInput(inputId = 'box_style3_main_title','Main title'),  
                                                        sliderInput(inputId = 'box_style3_main_title_size','Main title size',
                                                                    min = 0,
                                                                    max = 30,
                                                                    step = 1,
                                                                    value = 10),
                                                        textInput(inputId = 'box_style3_legend_title','Legend title'),
                                                        sliderInput(inputId = 'box_style3_legend_title_size','Legend title size',
                                                                    min = 0,
                                                                    max = 30,
                                                                    step = 1,
                                                                    value = 10),
                                                        sliderInput(inputId = 'box_style3_legend_size','Legend size',
                                                                    min = 0,
                                                                    max = 30,
                                                                    step = 1,
                                                                    value = 10),
                                                        sliderInput(inputId = 'box_style3_width','Box width',
                                                                    min = 0,
                                                                    max = 1,
                                                                    step = 0.01,
                                                                    value = 0.15),
                                                        sliderInput(inputId = 'box_style3_violin_width','Violin width',
                                                                    min = 0,
                                                                    max = 1,
                                                                    step = 0.01,
                                                                    value = 0.5),
                                                        selectInput(inputId = 'box_style3_statistical_method','Statistical method',
                                                                    choices = c('Student\'s t test'='t.test',
                                                                                'Wilcox test'='wilcox.test'
                                                                    ))

                                                        ),
                                       actionButton(inputId = 'button','GoMOAT!')
                                       ),
                                       mainPanel(
                                         fluidPage(br(),
                                                   tabsetPanel(tabPanel(title = 'View Data',
                                                                        br(),
                                                                        fileInput('boxplot_datafile', 'Choose your file (.csv/txt)',
                                                                                  buttonLabel = 'Upload',
                                                                                  placeholder = 'Upload data here',
                                                                                  accept=c('text/csv', 
                                                                                           'text/comma-separated-values,text/plain', 
                                                                                           '.csv')),hr(),
                                                                        dataTableOutput(outputId = 'boxplot_datafile')),
                                                               tabPanel('Box plots',
                                                                        fluidPage(
                                                                          br(),
                                                                          conditionalPanel(condition = "input.boxstyle =='boxstyle1'",
                                                                          wellPanel(
                                                                        fluidRow(column(3,textInput(inputId = 'box_style1_xlab','X-axis label')),
                                                                                 column(3,sliderInput(inputId = 'box_style1_xlab_size','Label size (X)',
                                                                                                      min = 0,
                                                                                                      max = 30,
                                                                                                      step = 1,
                                                                                                      value = 10)),
                                                                                 column(3,sliderInput(inputId = 'box_style1_x_axis_size','Text size (X)',
                                                                                                       min = 0,
                                                                                                       max = 30,
                                                                                                       step = 1,
                                                                                                       value = 10)),
                                                                                 column(3,colourInput(inputId = 'box_style1_color1','Color 1',
                                                                                                      value = '#9ecfc5',showColour = 'background'))),
                                                                        fluidRow(column(3,textInput(inputId = 'box_style1_ylab','Y-axis label')),
                                                                                 column(3,sliderInput(inputId = 'box_style1_ylab_size','Label size (Y)',
                                                                                                      min = 0,
                                                                                                      max = 30,
                                                                                                      step = 1,
                                                                                                      value = 10)),
                                                                                 column(3,sliderInput(inputId = 'box_style1_y_axis_size','Text size (Y)',
                                                                                                      min = 0,
                                                                                                      max = 30,
                                                                                                      step = 1,
                                                                                                      value = 10)),
                                                                                 column(3,colourInput(inputId = 'box_style1_color2','Color 2',
                                                                                                      value = '#D3d662',showColour = 'background'))))),
                                                                      
                                                                        conditionalPanel(condition = "input.boxstyle =='boxstyle2'",
                                                                                         wellPanel(
                                                                                           fluidRow(column(3,textInput(inputId = 'box_style2_xlab','X-axis label')),
                                                                                                    column(3,sliderInput(inputId = 'box_style2_xlab_size','Label size (X)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,sliderInput(inputId = 'box_style2_x_axis_size','Text size (X)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,colourInput(inputId = 'box_style2_color1','Color 1',
                                                                                                                         value = '#9ecfc5',showColour = 'background'))),
                                                                                           fluidRow(column(3,textInput(inputId = 'box_style2_ylab','Y-axis label')),
                                                                                                    column(3,sliderInput(inputId = 'box_style2_ylab_size','Label size (Y)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,sliderInput(inputId = 'box_style2_y_axis_size','Text size (Y)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,colourInput(inputId = 'box_style2_color2','Color 2',
                                                                                                                         value = '#D3d662',showColour = 'background'))),
                                                                                           
                                                                                           fluidRow(column(3,sliderInput(inputId = 'box_style2_point_size','Point size',
                                                                                                                         min = 0,
                                                                                                                         max = 3,
                                                                                                                         step = 0.1,
                                                                                                                         value = 2)),
                                                                                                    column(3,sliderInput(inputId = 'box_style2_point_transparency','Point transparency',
                                                                                                                         min = 0,
                                                                                                                         max = 1,
                                                                                                                         step = 0.1,
                                                                                                                         value = 1)),
                                                                                                    column(3,colourInput(inputId = 'box_style2_point_color1','Point color 1',
                                                                                                                         value = 'red',showColour = 'background')),
                                                                                                    column(3,colourInput(inputId = 'box_style2_point_color2','Point color 2',
                                                                                                                         value = 'purple',showColour = 'background')))
                                                                                           
                                                                                           
                                                                                           )),
                                                                        conditionalPanel(condition = "input.boxstyle =='boxstyle3'",
                                                                                         wellPanel(
                                                                                           fluidRow(column(3,textInput(inputId = 'box_style3_xlab','X-axis label')),
                                                                                                    column(3,sliderInput(inputId = 'box_style3_xlab_size','Label size (X)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,sliderInput(inputId = 'box_style3_x_axis_size','Text size (X)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,colourInput(inputId = 'box_style3_color1','Violin color 1',
                                                                                                                         value = '#9ecfc5',showColour = 'background'))),
                                                                                           fluidRow(column(3,textInput(inputId = 'box_style3_ylab','Y-axis label')),
                                                                                                    column(3,sliderInput(inputId = 'box_style3_ylab_size','Label size (Y)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,sliderInput(inputId = 'box_style3_y_axis_size','Text size (Y)',
                                                                                                                         min = 0,
                                                                                                                         max = 30,
                                                                                                                         step = 1,
                                                                                                                         value = 10)),
                                                                                                    column(3,colourInput(inputId = 'box_style3_color2','Violin color 2',
                                                                                                                         value = '#D3d662',showColour = 'background'))),
                                                                                           
                                                                                           fluidRow(column(3,sliderInput(inputId = 'box_style3_violin_size','Violin size',
                                                                                                                         min = 0,
                                                                                                                         max = 1,
                                                                                                                         step = 0.1,
                                                                                                                         value = 0.5)),
                                                                                                    column(3,sliderInput(inputId = 'box_style3_violin_transparency','Violin transparency',
                                                                                                                         min = 0,
                                                                                                                         max = 1,
                                                                                                                         step = 0.1,
                                                                                                                         value = 0.5)),
                                                                                                    column(3,colourInput(inputId = 'box_style3_violin_color1','Box color 1',
                                                                                                                         value = 'black',showColour = 'background')),
                                                                                                    column(3,colourInput(inputId = 'box_style3_violin_color2','Box color 2',
                                                                                                                         value = 'black',showColour = 'background')))
                                                                                           
                                                                                           )),
                                                                        
                                                                       
                                                                        hr(),
                                                                        fluidRow(column(6,offset = 3,plotOutput(outputId = 'box_plot',height = 550,width = 600))),
                                                                        hr(),
                                                                        downloadButton(outputId = 'download_box_style1','DOWNLOAD')
                                                                        ))
                                                               )))))),
                            tabPanel(title = 'Multi-group comparison'),
                            tabPanel(title = 'Multi-variable comparison')
                          )
                           
                          
                           )
                           
           )
           
  )
 

server <- function(input, output, session,plottt) {

output$boxplot_datafile <- renderDataTable({
    boxfile <- input$boxplot_datafile
    req(boxfile)
    read.csv(boxfile$datapath,header = T)
  })

  
plottt <- eventReactive(input$button,{
    if (input$boxstyle=='boxstyle1') {
    boxfile <- input$boxplot_datafile
    req(boxfile)
    boxfile <- read.csv(boxfile$datapath,header = T)
    ggplot(data=boxfile,aes(x=boxfile[,2],y=boxfile[,1]))+
      geom_boxplot(aes(fill=boxfile[,2]),width=input$box_style1_width,outlier.alpha = 0)+
      xlab(input$box_style1_xlab)+ylab(input$box_style1_ylab)+
      scale_fill_manual(values=c(input$box_style1_color1,input$box_style1_color2))+
      labs(fill = input$box_style1_legend_title,title = input$box_style1_main_title)+
      ylim(c(ymin=min(boxfile[,1]),ymax=max(boxfile[,1]*1.3)))+
      theme_bw(base_size = 12)+
      theme(plot.title = element_text(hjust = 0.5,color="black", size= input$box_style1_main_title_size),
            axis.text.x = element_text(color="black", size= input$box_style1_x_axis_size),    #各个字体大小
            axis.text.y = element_text(color="black", size= input$box_style1_y_axis_size),
            axis.title.x = element_text(color="black", size= input$box_style1_xlab_size),
            axis.title.y = element_text(color="black", size=input$box_style1_ylab_size),
            legend.text = element_text(color="black", size=input$box_style1_legend_size),
            legend.title = element_text(color="black", size=input$box_style1_legend_title_size))+
      geom_signif(comparisons = list(c(unique(as.character(boxfile[,2]))[1],
                                       unique(as.character(boxfile[,2]))[2])),map_signif_level = F,
                  y_position = c(max(as.numeric(boxfile[,1])*1.2)),test = input$box_style1_statistical_method)
    
  } else if (input$boxstyle=='boxstyle2'){

      boxfile <- input$boxplot_datafile
      req(boxfile)
      boxfile <- read.csv(boxfile$datapath,header = T)
      ggplot(data=boxfile,aes(x=boxfile[,2],y=boxfile[,1]))+
        geom_boxplot(aes(fill=boxfile[,2]),width=input$box_style2_width,outlier.alpha = 0)+
        geom_jitter(aes(color=boxfile[,2]),
          width =input$box_style2_point_width,
                    size=input$box_style2_point_size,
                    alpha=input$box_style2_point_transparency)+
        xlab(input$box_style2_xlab)+ylab(input$box_style2_ylab)+
        scale_fill_manual(values=c(input$box_style2_color1,input$box_style2_color2))+
        scale_color_manual(values=c(input$box_style2_point_color1,input$box_style2_point_color2))+
        labs(fill = input$box_style2_legend_title,title = input$box_style2_main_title)+
        labs(color='')+
        ylim(c(ymin=min(boxfile[,1]),ymax=max(boxfile[,1]*1.3)))+
        theme_bw(base_size = 12)+   
        theme(axis.text.x = element_text(face="bold", color="black", size= input$box_style2_x_axis_size),    
              axis.text.y = element_text(face="bold",  color="black", size= input$box_style2_y_axis_size),
              axis.title.x = element_text(face="bold", color="black", size= input$box_style2_xlab_size),
              axis.title.y = element_text(face="bold",color="black", size=input$box_style2_ylab_size),
              legend.text = element_text(face="bold", color="black", size=11),
              panel.border = element_rect(colour = 'black',size=1.4))+
        geom_signif(comparisons = list(c(unique(as.character(boxfile[,2]))[1],
                                         unique(as.character(boxfile[,2]))[2])),map_signif_level = F,
                    y_position = c(max(as.numeric(boxfile[,1])*1.2)),test = input$box_style2_statistical_method)
  } else {
      boxfile <- input$boxplot_datafile
      req(boxfile)
      boxfile <- read.csv(boxfile$datapath,header = T)
      ggplot(data=boxfile,aes(x=boxfile[,2],y=boxfile[,1]))+
        geom_violin(aes(fill=boxfile[,2]),width=input$box_style3_violin_width,
                    size=input$box_style3_violin_size,
                    alpha=input$box_style3_violin_transparency)+
        geom_boxplot(aes(color=boxfile[,2]),width=input$box_style3_width,outlier.alpha = 0)+
        xlab(input$box_style3_xlab)+ylab(input$box_style3_ylab)+
        scale_fill_manual(values=c(input$box_style3_color1,input$box_style3_color2))+
        scale_color_manual(values=c(input$box_style3_violin_color1,input$box_style3_violin_color2))+
        labs(fill = input$box_style3_legend_title,title = input$box_style3_main_title)+
        labs(color='')+
        ylim(c(ymin=min(boxfile[,1]),ymax=max(boxfile[,1]*1.3)))+
        theme_bw(base_size = 12)+   
        theme(axis.text.x = element_text(face="bold", color="black", size= input$box_style3_x_axis_size),    
              axis.text.y = element_text(face="bold",  color="black", size= input$box_style3_y_axis_size),
              axis.title.x = element_text(face="bold", color="black", size= input$box_style3_xlab_size),
              axis.title.y = element_text(face="bold",color="black", size=input$box_style3_ylab_size),
              legend.text = element_text(face="bold", color="black", size=11),
              panel.border = element_rect(colour = 'black',size=1.4))+
        geom_signif(comparisons = list(c(unique(as.character(boxfile[,2]))[1],
                                         unique(as.character(boxfile[,2]))[2])),map_signif_level = F,
                    y_position = c(max(as.numeric(boxfile[,1])*1.2)),test = input$box_style3_statistical_method)
  }
})


output$box_plot <- renderPlot({
  req(plottt())
  print(plottt())
})

output$download_box_style1 <- downloadHandler(
  filename = function() {
    paste0("Moat-Box-Style1-",Sys.Date(),'.pdf')
  },
  content = function(file) {
    req(plottt())
    ggsave(file,plottt(),device = "pdf")
  }
)


}

shinyApp(ui = ui, server = server)      


