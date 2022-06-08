library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Regression Line"),
                    dashboardSidebar(
                      sidebarMenu(
                        id ="tabs",
                        menuItem("Prerequistes",tabName = "prerequisite2",icon = icon("book")),
                        menuItem("Overview", tabName = "overview2",icon = icon("dashboard")),
                        menuItem("Challenge", tabName = "regression",icon = icon("cogs"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
                        
                          tags$style(HTML(
                            '.popover-title{
                            color:black;
                            font-size:18px;
                            background-color: orange
                            }'
                          ))
                        
                      ),
                      
                      tabItems(
                        
                        #Prerequiste tab
                        tabItem(tabName = "prerequisite2",
                                h3(strong("Background: Regression Line")),br(),
                                h4(strong("Correlation:")),
                                h4(tags$li("Correlation measures the strength of the linear association between two variables.")),
                                h4(tags$li("Correlation is always between -1 and +1 (-1 when the points fall exactly on a downhill line and +1 when the points fall exactly on an uphill line).")),
                                h4(tags$li("Correlation does not change when you change the units of measurement by adding/subtracting/multiplying/dividing every value by the same number.")),
                                h4(tags$li("Correlation is not appropriate as a measure of association for non-linear patterns.")),
                                h4(tags$li("Correlation is strongly affected by outliers.")),
                                h4(strong("Regression Lines:")),
                                h4(tags$li("The regression line provides a straight line that describes how the average for a response variable Y changes as an explanatory variable X changes. ")),
                                h4(tags$li("The regression line has the linear form telling you the best prediction for Y when you know that the variable X has a specific value x:")),
                                div(style = "text-align:center",h4("Predicted Y value = a + b*x")),
                                h4("- In this case “a” is the intercept (what you predict for Y when x = 0) and “b” is the slope (how much the average value of Y goes up for each unit of x).  The slope b will always have the same sign as the correlation."),
                                h4(tags$li("The regression line is the straight line that makes the standard deviation of the vertical distances of the data points from the line as small as possible.  (Remember that standard deviation measures variability and we want to keep the variability around the line as small as possible.)")),
                                h4(tags$li("The regression line is not appropriate when the relationship between x and y is not described well by a straight line.")),
                                h4(tags$li("The regression line is not appropriate when an outlier is driving the relationship.")),
                                br(),
                               
                                div(style = "text-align:center",
                                    bsButton("nextbutton", "Go to the overview", icon("wpexplorer"), size = "medium",style = "warning"))
                        ),
                        
                        # Overview tab content
                        tabItem(tabName = "overview2",
                                tags$a(href='http://stat.psu.edu/',   tags$img(src='logo.png', align = "left", width = 180)),
                                br(),
                                br(),
                                br(),
                                h3(strong("About:")),
                                h4("In this App, you will explore how the correlation and regression line relate to the points on a scatterplot."),
                                br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("Click New Challenge to get a challenge.")),
                                h4(tags$li("Create your own line by entering the values for both slope and intercept.")),
                                h4(tags$li("Create points by clicking in the plot.")),
                                h4(tags$li("Show the regression line to compute and display the regression line.")),
                                h4(tags$li("Click RESET to clean both points and regression lines.")),
                                div(style = "text-align:center",
                                    bsButton("start", "GO", icon("bolt"), size = "large",style = "warning")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Caihui Xiao and further updated by Zhiliang Zhang and Jiajun Gao.",
                                   " This app is based on extending the idea in the WH freeman applet at http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_5_correg.html.",
                                   " Special thanks to Sitong Liu for help on some programming issues.")
                                
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "regression",
                                                                                                
                                #change the color of slide
                                
                                tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange}")),
                                tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge,.js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: orange}")),
                                
                                #buttons
                                  div(style="display: inline-block;vertical-align:top;",
                                      div(style="display: inline-block;vertical-align:top;",
                                          tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                      ),
                                      circleButton("inst2",icon = icon("info"), status = "myClass",size = "xs")
                                      
                                  ),
                                
                                 # Add a title
                                 titlePanel("Explore the regression line"),
                            
                                br(),
                                
                                 
                                 verbatimTextOutput("question1"),
                                 tags$style(type='text/css', '#question1 {font-weight:bold;font-family: sans-serif;font-size: 20px;background-color: #ffffff; color: black;}'), 
                                
                                wellPanel(style = "background-color: #ffffff;",
                                          
                                          # fluidRow(
                                          #   div(style="display: inline-block;vertical-align:top;",
                                          #       circleButton("inst2",icon = icon("question",class = "glyphicon glyphicon-info-sign"), size = "xs"),
                                          #       div(style="display: inline-block;vertical-align:top;",
                                          #           tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                                          #       )
                                          #   )
                                          #   
                                          # ),          
                                  
                                  fluidRow(
                                  column(2,    offset=2,       actionButton("newchallenge","New Challenge", style="color: #ffffff; background-color: orange")),
                                       
                                  column(2,    offset=2,       actionButton("clear", "RESET", style="color: #ffffff; background-color: orange"))
                                ),
                                  
                                  
                                  bsPopover("inst2", " ", "Click to show the instruction for this challenge", place = "bottom"),
                                  bsPopover("clear", " ", "Click RESET to start over this challenge", place = "right"),
                                  
                              
                                
                              
                                  
                                  fluidRow(

                                    column(2,
                                          checkboxInput ("yourownline", "Create your own line")),
                                    column(4, sliderInput("intercept", 
                                                          "Choose the intercept:", 
                                                          min = -10,
                                                          max = 10, 
                                                          value = 5,
                                                          step = 0.01)),
                                    column(4, sliderInput("slope", 
                                                          "Choose the slope:", 
                                                          min = -10,
                                                          max = 10, 
                                                          value = 2,
                                                          step = 0.01))
                                    ),
                                    
                                    #numeric input format for slope and intercept
                                    # column(2,
                                    #        numericInput("slope", "Enter Slope", 0,step=0.1)),
                                    # 
                                    # 
                                    # column(2,
                                    #        numericInput("intercept", "Enter Intercept", 0,step=0.1)
                                    # )
                                
                                
                                  fluidRow(

                                    column(2, checkboxInput("regressionline", "Show regression", FALSE))

                                  #  column(8, conditionalPanel(style = "background-color: #ffffff;","input.regressionline !=0",verbatimTextOutput('regression_equation'))),
                                  #  tags$head(tags$style(HTML("#regression_equation {font-size: 18px;background-color: #ffffff}")))
                                    
                                   # tags$style(type='text/css', '#regression_equation {background-color: #ffffff; font-size: 20px;color: black;}') 


                                  ),
                                 fluidRow(


                                   column(2, checkboxInput("correlation", "Show correlation", FALSE))
                                   #column(8, conditionalPanel(style = "background-color: white;","input.correlation !=0",verbatimTextOutput('correlation'))),
                                  # tags$style(type='text/css', '#correlation {background-color: #ffffff;font-size: 18px; color: black;}')
                                   
                                 )
                               ),
                                  
                            
                                  # Add a row for the main content
                                  fluidRow(
                                    tags$style(type='text/css', '#feedback {background-color: #ffffff; color: black;}'),
                                    
                                    # Create a space for the plot output
                                    
                                   column(8, plotOutput(
                                      "clusterPlot", "100%", "500px", click="clusterClick"
                                    )),
                                   
                                   bsPopover("clusterPlot"," ","Click points on the graph to create your plot", trigger = "hover",place="top"),
                                   
                                    column(4,
                                     column(12, conditionalPanel(style = "background-color: white;","input.yourownline !=0",verbatimTextOutput('yourline'))),
                                     column(12, conditionalPanel(style = "background-color: white;","input.regressionline !=0",verbatimTextOutput('regression_equation'))),
                                     column(12, conditionalPanel(style = "background-color: white;","input.correlation !=0",verbatimTextOutput('correlation'))),
                                     br(),
                                     verbatimTextOutput('feedback'),
                                     
                                tags$head(tags$style(HTML("#yourline {font-weight:bold;font-family: sans-serif;font-size: 18px;color: red;background-color: #ffffff}"))),
                                tags$head(tags$style(HTML("#regression_equation {font-weight:bold;font-family: sans-serif;font-size: 18px;background-color: #ffffff}"))),
                                tags$head(tags$style(HTML("#correlation {font-weight:bold;font-family: sans-serif;font-size: 18px;background-color: #ffffff}"))),
                                tags$head(tags$style(HTML("#feedback {font-size: 18px;font-family: sans-serif;background-color: #ffffff}")))
                                
                                )
                            )
                            
                        )
                      )
                    )
)
