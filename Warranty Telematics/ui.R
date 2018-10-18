rm(list=ls())
getwd()

library(markdown)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(plotly)

shinyUI(fluidPage(
  # list(tags$head(HTML('<link rel="icon", href="telematics_logo.png",
  #                       type="image/png" />'))),
  # div(style="padding: 1px 0px; width: '100%'",
  #     titlePanel(
  #       title="", windowTitle="Warranty Telematics Solution"
  #     )
  # ),
  navbarPage(id = "teleTabs",
             title = div(img(src = "telematics_logo.png",height=35),
                         style="vertical-align: top;"),
             position = "fixed-top",footer = "",
             header = img(src="cogds.png",height=30, align="right"),fluid = F,
             windowTitle = "Warranty Telematics Solution",selected = "Home",
             tags$style(type="text/css", "body {padding-top: 70px;}"),

             theme = shinytheme("cerulean"),
             #shinythemes::themeSelector(), #themeselector (only for testing)

             ############## About page ####################
             tabPanel("Home",
                      mainPanel(width=12,
                                tabsetPanel(id="Home_pagetab",
                                            tabPanel("Warraty Telematics Solution",
                                                     value = 101,
                                                     br(),
                                                     div(img(src="telematics.png",height=300), style="text-align: center;"),
                                                     fluidRow(
                                                       column(8, offset=2,
                                                              includeMarkdown("www/intro.md")
                                                       ))
                                            ),
                                            tabPanel("Data Snapshot",
                                                     value = 102,
                                                     br(),
                                                     fluidRow(
                                                       column(8, offset=2,
                                                              includeMarkdown("www/Data_Snapshot.md")
                                                       ))
                                                    ),
                                            tabPanel("EDA",
                                                     value = 103,
                                                     br(),
                                                     fluidRow(
                                                       column(8, offset=2,
                                                              includeMarkdown("www/EDA.md")
                                                       ))
                                            ),
                                            tabPanel("Warranty Clusters",
                                                     value = 104,
                                                     br(),
                                                     fluidRow(
                                                       column(8, offset=2,
                                                              includeMarkdown("www/Warranty_Clusters.md")
                                                       ))
                                            ),
                                            tabPanel("DTC Sequences",
                                                     value = 105,
                                                     br(),
                                                     column(8, offset=2,
                                                            includeMarkdown("www/DTC_sequence.md")
                                                     )
                                                     
                                            ),
                                            tabPanel("FAQs",
                                                     value = 106,
                                                     br(),
                                                     fluidRow(
                                                       column(8, offset=2,
                                                              includeMarkdown("www/FAQ.md")
                                                       ))
                                            )
                                ))
                    ),
             
             ############## Reference ####################
             tabPanel("Reference",
                      mainPanel(width=12,
                                br(),
                                fluidRow(
                                  column(8, offset=2,
                                         includeMarkdown("www/Reference.md")
                                  ))
                                )
             ),
             
             ############## Summary ####################
             tabPanel("Summary",
                      sidebarPanel(width=3,
                                   numericInput("dtc_clusters1", label ="Select Cluster",1, min = 1, max = 13),
                                   # numericInput("dtc_error1", label ="Select Cluster",1, min = 1, max = 3),
                                   h4(textOutput("text7")),
                                   h5(textOutput("text6")),
                                   radioButtons("inRadioButtons2", "Select Error Sequence",
                                                c("1", "2", "3"), selected = 1)
                      ),
                       mainPanel(width=9,
                                #Start Drt summary
                                fluidRow(
                                  box(
                                    title = "Cluster Words", solidHeader = TRUE, background = "black",
                                    div(img(src="cluster_warranty.png",height=220), style="text-align: center;")
                                  ),
                                  box(
                                    title = "Reliability Curve", solidHeader = TRUE, background = "black",
                                    div(img(src="Reliability_Curve_All.png",height=220), style="text-align: center;")
                                  )
                                ),
                                fluidRow(
                                  box(
                                    title = "DTC Sequences", solidHeader = TRUE, background = "black",
                                    div(img(src="Cluster_DTC.png",height=230), style="text-align: center;")
                                  ),
                                  box(
                                    title = "Machine Identification", solidHeader = TRUE, background = "black",
                                    h5(textOutput("text4")),
                                    h5(textOutput("text5")),
                                    div(dataTableOutput("Machine_Identification1"), style = "font-size:70%")
                                    # div(dataTableOutput("tableName"), style = "font-size:80%")
                                  )
                                )
                                # ,
                                # bsCollapse(open = "DTC Sequence1",
                                #            multiple = FALSE,id = "collapse101",
                                #            bsCollapsePanel("DTC Sequence",
                                #                           h3(textOutput("text4")),
                                #                           h3(textOutput("text5")),
                                #                           dataTableOutput("Machine_Identification1"),style = "info")
                                #           )
                                )
                      ),
             ############## Data Snapshot ####################
             tabPanel("Data Snapshot",
                      sidebarLayout(
                        #sidebar panel-input
                        sidebarPanel(
                          # 1st Condition - Dataset - Collapsable Pane ----------------------#

                          conditionalPanel(
                            condition = " input.teleTabs == 'Data Snapshot' & input.tabs ==  'Dataset'",

                            bsCollapse(
                              bsCollapsePanel(
                                "Warranty",
                                style = "primary",
                                selectInput('ft1', "Select the file type", c("csv", "xlsx")),
                                fileInput(inputId = 'file1',
                                          label='Upload Warranty File',
                                          accept=c('text/csv', 'text/comma-separated-values','text/plain', '.csv'))
                              )
                            ),

                            # Customer Collapse Pane -------------------------------------------------- #

                            bsCollapse(
                              bsCollapsePanel(
                                "DTC",
                                style = "primary",
                                selectInput('ft2', "Select the file type", c("csv", "xlsx")),

                                fileInput(inputId = 'file2'
                                          , label='Upload DTC File'
                                          , accept=c('text/csv', 'text/comma-separated-values','text/plain', '.csv'))
                              )
                            ),

                            selectInput(
                              "type",
                              "Select the dataset to Display",
                              c("Warranty", "DTC")
                            )

                          ) 
                        ),
                        mainPanel(tabsetPanel(
                          type = "tab",
                          id = 'tabs',
                          tabPanel(
                            "Dataset",
                            fluidPage(
                              theme = shinytheme("cerulean"),
                              h4(strong("Data Snapshot:")),
                              #-------------------------------------

                              dataTableOutput('Dataset_snapshot'),
                              list(tags$head(
                                tags$style("body {background-color: #F0F8FF; }")
                              ))
                            )
                          )
                        ))

                      )
             ),
            ############## Exploratory Data Analysis ####################
            
            tabPanel("Exploratory Data Analysis",
                     tags$head(
                       tags$style(HTML("
                                       .shiny-output-error-validation {
                                       color: red;
                                       }
                                       "))),
                     sidebarLayout(
                       sidebarPanel(
                         #1st condition
                         conditionalPanel(
                           condition = "input.teleTabs == 'Exploratory Data Analysis' & input.tabs1 == 51 ",
                           selectInput(
                             "eda_uni_var",
                             "Select the dataset:",
                             c("Warranty Data", "DTC Data")
                           ),
                           selectInput(
                             "eda_uni_var1",
                             "Select the Variable",
                             ""
                           )
                         ),
                         # 2nd COndition
                         conditionalPanel(
                           condition = "input.teleTabs == 'Exploratory Data Analysis' & input.tabs1 == 52",
                           selectInput(
                             "eda_bi_var",
                             "Select the dataset:",
                             c("Warranty Data", "DTC Data")),
                           
                           selectInput(
                             "eda_bi_var1", "Select First variable:",
                             ""
                           ),
                           selectInput(
                             "eda_bi_var2", "Select second variable:",
                             ""
                           )
                           
                           #                               selectInput("select2", label = h3("Select box"),
                           #                                           choices = list("Choice 1" = 1, "Choice 2" = 2,"Choice 3" = 3), 
                           #                                           selected = 1)
                           
                         ),
                         #3rd Condition
                         conditionalPanel(
                           condition = "input.teleTabs == 'Exploratory Data Analysis' & input.tabs1 == 53",
                           
                           selectInput(
                             "eda_dist_var",
                             "Select the dataset:",
                             c("Warranty Data", "DTC Data")),
                           
                           selectInput("eda_distri", label = h3("Select Variables"),
                                       choices = "", 
                                       multiple = TRUE)
                           
                         )
                       ),
                       mainPanel(
                         tabsetPanel(# type = "tab",
                           id = "tabs1",
                           tabPanel( "Univariate Analysis",
                                     value = 51,
                                     strong("Plot for the variable"),
                                     plotOutput('univariate_plot')
                                     
                           ),
                           tabPanel( "Bivariate Analysis",
                                     value = 52,
                                     strong("Plot for the variables"),
                                     plotOutput('bivariate_plot')
                                     
                           ),
                           tabPanel("Distribution graphs",
                                    value = 53,
                                    strong("Plot for the variables"),
                                    plotOutput('distri_graph')
                           )
                         )
                       )
                     )
            ),
            
############ Warranty Clusters ####################
             tabPanel("Warranty Clusters",
                      sidebarLayout(

                      sidebarPanel(width=3,
                                   conditionalPanel(
                                     condition = "input.teleTabs == 'Warranty Clusters' & input.dataTabs31 == 31 ",
                                   textInput(inputId = "stopwords",
                                             label = "Enter Stopwords separated by comma",
                                             value = ""),
                                   actionButton("actionbu", "Submit"),p(""),br(),
                                   sliderInput("slider1", label = "No of Clusters",
                                               min = 0, max = 20, value = 13),
                                  numericInput("numberin", label ="Select Cluster",1, min = 1, max = 13)

                                  #submitButton('Submit')

                      )),
                      mainPanel(width=9,
                                tabsetPanel(id="dataTabs31",
                                            tabPanel("Clusters",
                                                     value = 31,
                                                     bsCollapse(open = "Warranty Data",
                                                                multiple = FALSE,id = "collapse1",
                                                                bsCollapsePanel(
                                                                  "Word Cloud",plotOutput("plot"),style = "info"),
                                                                bsCollapsePanel(
                                                                  "Topics",dataTableOutput("Clusters"),style = "info"),
                                                                bsCollapsePanel(
                                                                  "Replacement Rate",dataTableOutput("Replacement_Rate"),style = "info"),
                                                                bsCollapsePanel(
                                                                  "Warranty Hours",plotlyOutput("plot4"),style = "info"),
                                                                bsCollapsePanel(
                                                                  "Reliability Curve",plotlyOutput("plot2"),style = "info"))
                                )
                      )
                      )
             )),
             ############## DTC Sequence ####################
             tabPanel("DTC Sequence",
                      sidebarPanel(width=3,
                                   numericInput("dtc_clusters", label ="Select Cluster",1, min = 1, max = 13),
                                   h4(textOutput("text107")),
                                   h5(textOutput("text106")),
                                   radioButtons("inRadioButtons102", "Select Error Sequence",
                                                c("1", "2", "3"), selected = 1)
                                   # numericInput("dtc_select", "Select DTC", 1, min = 1, max = 2)
                      ),
                      mainPanel(width=9,
                               tabsetPanel(id="dataTabs4",
                               tabPanel("DTC Sequence",
                                        value = 41,
                                                     bsCollapse(open = "DTC Sequence",
                                                                multiple = FALSE,id = "collapse101",
                                                                bsCollapsePanel("DTC sequence Plot",
                                                                                div(img(src="DTC_sequence_Plot.png",height=400), style="text-align: center;"),
                                                                                style = "info"),
                                                                bsCollapsePanel("Summary Box Plot DTC",
                                                                                div(img(src="summary_box_plot_DTC.png",height=400), style="text-align: center;"),
                                                                                style = "info"),
                                                                bsCollapsePanel("Machine Identification",
                                                                                h3(textOutput("text1")),
                                                                                h3(textOutput("text2")),
                                                                                h3(textOutput("text3")),
                                                                                dataTableOutput("Machine_Identification"),
                                                                                style = "info")
                                                                
                                                                )
                               )
                               )
                      )
             )


)))
