# this is the ui of the waste calculation shiny app
#
#    
#

library(shiny)
library(shinythemes)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(fontawesome)
library(tippy)
library(shinyWidgets)

library(knitr)
library(testthat)
library(kableExtra)

library(shinydashboard)
library(tippy)
library(shinycssloaders)
library(shinytest)
library(htmltools)



# Define UI for application that draws a histogram
shinyUI(navbarPage("Waste Prediction Tool - New Zealand Residential Projects", 
                   theme = shinytheme("flatly"),
                   inverse = TRUE, 
                   windowTitle = "WMPT-New Zealand",
                   position = "fixed-top",
                   footer = tags$footer(align = "center", includeHTML("./www/include_footer.html")),
                   header = tags$style(
                     ".navbar-right {
                       float: right !important;
                       }",
                     "body {padding-top: 75px;}"),
                   
                   tabPanel("Calculator",
                            theme = shinytheme("flatly"),
                            fluidPage(
                              shinyjs::useShinyjs(),
                              div(id="form-group has-warning"),
                              fluidRow(
                                h3("Lets get started...")
                              ),
                              fluidRow(
                                #first row is project details
                                column(3,
                                       h3("Enter the project details"),
                                       
                                       p(HTML("<b>Enter the project name</b>"),span(shiny::icon("info-circle"), id = "info_pro"),textInput('project_id', NULL),
                                         tippy::tippy_this(elementId = "info_pro",tooltip = "Enter the name of the project",placement = "right")
                                         
                                       )),
                                column(3,
                                       br(),br(),br(),
                                       
                                       p(HTML("<b>Enter the location/city</b>"),span(shiny::icon("info-circle"), id = "info_city"),textInput('city', NULL),
                                         tippy::tippy_this(elementId = "info_city",tooltip = "Enter the location of the building. E.g. Auckland",placement = "right")
                                         
                                       )),
                                
                                column(3,
                                       br(),br(),br(),
                                       
                                       p(HTML("<b>Select the type of builder/city</b>"),span(shiny::icon("info-circle"), id = "info_builder"),
                                         selectInput("dataSet",NULL,
                                                     c("National builder" = "other",
                                                       "Small builder" = "ryan")),
                                       tippy::tippy_this(elementId = "info_builder",tooltip = "Select the type of the builder from the list",placement = "right")
                                         
                                       )),
                                
                                column(3, br(),br(),br(),p(HTML("<b>Enter the number of expected or actual working days</b>"),span(shiny::icon("info-circle"), id = "info_days"),numericInput('working_days', NULL, 110,min=75,max=350),
                                                           tippy::tippy_this(elementId = "info_days",tooltip = "Number of working days has a limit of 75-350 days",placement = "right"),span(textOutput("errorwd"), style="color:red")
                                                           
                                )
                                
                                )),
                              fluidRow(
                                #first column is basic attributes, project information and roof data
                                column(4,br(),
                                       h3("Basic Attributes"),
                                       
                                       #house type
                                       p(HTML("<b>Select the house type</b>"),span(shiny::icon("info-circle"), id = "info_house_type"),
                                         tippy::tippy_this(elementId = "info_house_type",tooltip = "'House and income' dwellings are categorised as 'house and granny flat'",placement = "right"),
                                         br(),
                                         actionButton(
                                           inputId = c("h"),
                                           label = list(tags$span(icon("home",lib = "font-awesome"), "House")),
                                           block=TRUE,
                                           status = "primary"
                                           #class = "btn-warning"
                                         ),
                                         outputId = "1",
                                         actionButton(
                                           inputId = c("f"),
                                           label = list(tags$span(icon("home",lib = "font-awesome"),icon("dollar-sign",lib = "font-awesome"), "House & Granny flat")),
                                           block=TRUE,
                                           status = "primary"
                                           #class = "btn-warning"
                                         ),
                                         outputId = "0"),
                                       
                                       br(),
                                       
                                       #number of stories
                                       p(HTML("<b>Select the number of stories/levels</b>"),span(shiny::icon("info-circle"), id = "info_stories"),
                                         tippy::tippy_this(elementId = "info_stories",tooltip = "Number of stories/levels has a limit of 1-3",placement = "right"),
                                         br(),
                                         actionButton(
                                           inputId = c("s1"),
                                           label = "1",
                                           status = "primary"
                                         ),
                                         outputId = "1",
                                         
                                         actionButton(
                                           inputId = c("s2"),
                                           label = "2",
                                           status = "primary"
                                         ),
                                         outputId = "2"
                                         
                                         
                                       ),
                                       
                                       
                                       
                                       br(),
                                       
                                       #number of bathrooms
                                       p(HTML("<b>Select the number of bathrooms</b>"),span(shiny::icon("info-circle"), id = "info_bath"),
                                         tippy::tippy_this(elementId = "info_bath",tooltip = "Number of bathrooms has a limit of 1-3",placement = "right"),
                                         br(),
                                    
                                         actionButton(
                                           inputId = c("b2"),
                                           label = "2",
                                           status = "primary"
                                         ),
                                         outputId = "2",
                                         
                                         actionButton(
                                           inputId = c("b2_5"),
                                           label = "2.5",
                                           status = "primary"
                                         ),
                                         outputId = "2.5",
                                         
                                         actionButton(
                                           inputId = c("b3"),
                                           label = "3",
                                           status = "primary"
                                         ),
                                         outputId = "3",
                                         
                                         actionButton(
                                           inputId = c("b3_5"),
                                           label = "3.5",
                                           status = "primary"
                                         ),
                                         outputId = "3.5"),
                                       
                                       br(),
                                       #number of corners
                                       p(HTML("<b>Enter the number of corners</b>"),span(shiny::icon("info-circle"), id = "info_corners"),numericInput('corners', NULL, 12,min=9,max=46),
                                         tippy::tippy_this(elementId = "info_corners",tooltip = "Number of corners has a limit of 9-46",placement = "right"),span(textOutput("errorco"), style="color:red")
                                         
                                         
                                         
                                       )),
                                
                                
                                column(4, br(),
                                       h3("Measurements"),
                                       p(HTML("<b>Enter the internal wall length (m)</b>"),span(shiny::icon("info-circle"), id = "info_intW"),numericInput('int_wall_length', NULL, 59,min=35,max=80),
                                         tippy::tippy_this(elementId = "info_intW",tooltip = "Internal wall length has a limit of 13m - 120m",placement = "right"),span(textOutput("errorip"), style="color:red")
                                       ),
                                       
                                       
                                       p(HTML("<b>Enter the External perimeter (m)</b>"),span(shiny::icon("info-circle"), id = "info_exP"),numericInput('ext_perimeter', NULL, 70,min=45,max=100),
                                         tippy::tippy_this(elementId = "info_exP",tooltip = "External perimeter has a limit of 40m - 132m",placement = "right"),span(textOutput("errorep"), style="color:red")
                                       ),
                                       
                                       p(HTML("<b>Enter the floor area (m2)</b>"),span(shiny::icon("info-circle"), id = "info_fa"),numericInput('floor_area', NULL, 208,min=130,max=280),
                                         tippy::tippy_this(elementId = "info_fa",tooltip = "Floor area has a limit of 89m2 - 337m2",placement = "right"),span(textOutput("errorfa"), style="color:red")
                                       ),
                                       
                                       p(HTML("<b>Enter the roof area (m2)</b>"),span(shiny::icon("info-circle"), id = "info_ra"),numericInput('roof_area', NULL, 300,min=200,max=380),
                                         tippy::tippy_this(elementId = "info_ra",tooltip = "Roof area has a limit of 211m2 to 460m2",placement = "right"),span(textOutput("errorra"), style="color:red")
                                       )),
                                
                                
                                column(4, br(),
                                       h3("Cladding details"),
                                       
                                       #house type
                                       p(HTML("<b>Select the roof cladding type</b>"),
                                         br(),
                                         actionButton(
                                           inputId = c("S"),
                                           label = "Steel",
                                           status = "primary"
                                         ),
                                         outputId = "1",
                                         actionButton(
                                           inputId = c("PM"),
                                           label = "Pressed metal tiles",
                                           status = "primary"
                                         ),
                                         outputId = "0"),
                                       
                                       p(HTML("<b>Enter the area of sheet cladding (m2)</b>"),numericInput('sheet_cladding', NULL, 210,min=0,max=240)
                                       ),
                                       p(HTML("<b>Enter the area of board cladding (m2)</b>"),numericInput('board_cladding', NULL, 0,min=0,max=140)
                                       ),
                                       p(HTML("<b>Enter the area of brick and/or stone (m2)</b>"),numericInput('brick_or_stone', NULL, 0,min=0,max=250)
                                       )
                                       
                                ),
                                
                              ),
                              fluidRow(
                                #first column is basic attributes, project information and roof data
                                column(3
                                ),
                                
                                
                                
                                column(5, br(), br(),br(), br(), tags$hr(htmlOutput("calWaste")), tags$hr(),
                                       p(actionButton("btn_go", "Calculate waste")),
                                       ),
                                column(4,
                                       br(), br(),br(), br(), br(),br(),br(),br(), br(), uiOutput("ui_dlbtn")
                                )
                                
                                
                              )
                              
                            )),
                   tabPanel("User Guide",
                            theme = shinytheme("flatly"),
                            navlistPanel(
                              tabPanel("Introduction",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("This application intends to predict how much waste will be generated from your residential building. Currently, there are limitations to the attributes we can use to predict with this tool.",style="text-align:justify;padding:10px"),
                                                br(),
                                                p("Eg., we cannot use the tool if your building has four bathrooms. This is mainly because of the limitation in the dataset that we used to create the tool. If you enter any value that is out of the tool's scope, it will return an error message. Please navigate the list on your left to learn more about calculating the quantities of design parameters to use in the tool.", style="text-align:justify;padding:10px"),
                                                br(),
                                                p(icon("hand-point-left",lib = "font-awesome"),"Please navigate through the list on your left to learn more about how to calculate the features used in the tool.",style="text-align:justify;padding:10px"),
                                                br())),
                              "Basic attributes",
                              tabPanel("House type",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider two types of dwellings.",style="text-align:justify;padding:10px"),
                                                br(),
                                                br(),
                                                p(icon("home",lib = "font-awesome"),"House - A detached single-family dwelling",style="text-align:justify;padding:10px"),
                                                br(),
                                                p(icon("home",lib = "font-awesome"),icon("dollar-sign",lib = "font-awesome"),"House and granny flat – A house with a separate unit for rentable space for additional income. Also known as house and income dwellings",style="text-align:justify;padding:10px"),
                                                br())),
                              tabPanel("Number of stories or floors",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider the number of stories / ﬂoors in the dwelling. The number of stories can range from 1 to 2.",style="text-align:justify;padding:10px"),
                                                br())),
                              tabPanel("Number of corners",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider the total number of internal and external corners within the exterior wall length. The number of corners can range from 9 to 46. See the example below.",style="text-align:justify;padding:10px"),
                                                br(),
                                                tags$img(src="Corners.PNG",width="719px",height="300px"))),
                              tabPanel("Number of bathrooms",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider the number of bathroom ﬂoors in the dwelling. The number of bathrooms can range from 1 to 3.5.",style="text-align:justify;padding:10px"),
                                                br())),
                              "Measurements",
                              tabPanel("Internal wall length",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider the total interior wall length (m). The total interior wall length can range from 13m to 120m. See the example below.",style="text-align:justify;padding:10px"),
                                                br(),
                                                tags$img(src="Internal walls.png",width="719px",height="300px"))),
                              tabPanel("External perimeter",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider the total exterior wall length (m). The total exterior perimeter can range from 40m to 132m. See the example below.",style="text-align:justify;padding:10px"),
                                                br(),
                                                tags$img(src="External walls.PNG",width="719px",height="300px"))),
                              tabPanel("Floor area",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider the total constructed area of the dwelling. Should be veriﬁed through plan review (m2). The total floor area can range from 89.6m2 to 337.9m2. See the example below.",style="text-align:justify;padding:10px"),
                                                br(),
                                                tags$img(src="floor.PNG",width="719px",height="300px"))),
                              tabPanel("Roof area",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider total area of roof covering of the dwelling. The total floor area can range from 211m2 to 460m2. See the example below.",style="text-align:justify;padding:10px"),
                                                br(),
                                                tags$img(src="Roof.PNG",width="719px",height="300px"))),
                              "Cladding types",
                              tabPanel("Roof cladding types",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider two types of roof cladding",style="text-align:justify;padding:10px"),
                                                br(),
                                                br(),
                                                p("Pressed metal tiles – roof covering with pressed metal tiles",style="text-align:justify;padding:10px"),
                                                br(),
                                                p("Steel – steel roof covering",style="text-align:justify;padding:10px"))),
                              tabPanel("Wall cladding types",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                h4("Sheet cladding"),
                                                p("The total combined area of Polyplaster, Linea, Stria, Axon, Oblique and Coloursteel claddings (m2).", strong("Enter Zero if not applicable."),style="text-align:justify;padding:10px"),
                                                br(),
                                                h4("Board cladding"),
                                                p("The total combined area of Linea and Cedar weatherboard claddings (m2).", strong("Enter Zero if not applicable."),style="text-align:justify;padding:10px"),
                                                br(),
                                                h4("Brick and Stone cladding"),
                                                p("The total combined area of a brick veneer and schist cladding (m2)", strong("Enter Zero if not applicable."),style="text-align:justify;padding:10px"))),
                              "Project Details",
                              tabPanel("Working days",
                                       fluidRow(br(),
                                                br(),
                                                br(),
                                                p("Here we consider the number of working days spent to build the house. You may enter the predicted number of days for the project",style="text-align:justify;padding:10px"))),
                            ),
                            
                   )
                   
                   
                   
                   
                   
                   
                   
))
