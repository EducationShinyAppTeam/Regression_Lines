library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
# Let`s begin
ui <- dashboardPage(
  skin = "yellow", 
  dashboardHeader(
    titleWidth = 250, 
    title = "Regression Line", 
    tags$li(class = "dropdown", 
            actionLink("inst", icon("info",class = "myClass"))), 
    tags$li(
      class = 'dropdown', 
      tags$a(href = "https://shinyapps.science.psu.edu/", 
             icon('home', lib = 'font-awesome'))
    )
  ), 
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      id = "tabs", 
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")), 
      menuItem("Prerequistes", tabName = "prerequisite", icon = icon("book")), 
      menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")), 
      menuItem("References", tabName = "References", icon = icon("leanpub"))
    ), 
    tags$div(class = "sidebar-logo",
             boastUtils::psu_eberly_logo("reversed"))
  ), 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", 
                href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
      ), 
    tabItems(
      ##First tab - Dashboard tab 
      tabItem(
        tabName = "overview", 
        h1("Regression Line"), 
        p(
          "In this App, you will explore how the correlation and 
          regression line relate to the points on a scatterplot."
        ), 
        br(), 
        h2("Instructions"), 
        tags$ol(
          tags$li(
            "Create your own line by entering the values for both slope 
             and intercept."
            ), 
          tags$li("Create points by clicking in the plot."), 
          tags$li(
            "Show the regression line to compute and display
             the regression line."
            ), 
          tags$li("Click RESET to clean both points and regression lines.")
          ), 
        br(), 
        div(
          style = "text-align:center",
          bsButton(
            inputId = "go1",
            label = "GO!",
            icon = icon("bolt"),
            size = "large",
          )
        ), 
        #Acknowledgement
        br(), 
        br(), 
        h2("Acknowledgements"), 
        p(
          "This app was developed and coded by Caihui Xiao. 
          The app was further updated by Zhiliang Zhang and Jiajun Gao 
          in June 2018 and by Daehoon Gwak in July 2020.
          Special thanks to Sitong Liu for help on some programming issues.", 
          br(), 
          br(), 
          br(), 
          div(class = "updated", "Last Update: 7/23/2020 by DG.")
        )
      ),
      ##Second tab - Prerequisites tab
      tabItem(
        withMathJax(), 
        tabName = "prerequisite", 
        h2("Prerequisites"), 
        br(), 
        box(
          title = "Correlation", 
          p("Correlation measures the strength of the linear association
            between two variables/attributes and is therefore 
            not appropriate for non-linear patterns. 
            Correlations are always values between –1 and +1. 
            A correlation of –1 or +1 indicates that we have a perfect
            linear relationship; every point lies on the regression line.
            Correlation is strongly affected by outliers."), 
          p("Correlation does not change when you change the unit 
             of measurement. For instance, if you find the correlation 
             between people's height (in inches) and their ages (years), 
             that value will be the same if you convert everyone's height 
             to centimeters and/or their ages to days. More generally, 
             if you transform every observation's values by adding, 
             subtracting, multiplying, and/or dividing by the same number,
             the correlation will remain the same."), 
          collapsible = TRUE, 
          collapsed = TRUE, 
          width = 12
        ), 
        box(
          title = "Regression Lines", 
          p("The regression line provides a straight line that describes 
             a response variable Y that changes as an explanatory 
             variable X changes. The regression line has the linear form
             telling you the predicted value Y, when you know that 
             the variable X has a specific value x: \\[\\widehat{Y}=a+b*x\\]"), 
          p("In this case, 'a' is an intercept (what you predict for Y 
             when x = 0) and 'b' is the slope (how much the average value of Y 
             goes up for each unit of x). The slope 'b' will always has 
             the same sign as the correlation."),
          p("The regression line is the straight line that makes the standard 
             deviation of the vertical distances of the data points from 
             the line as small as possible. (Remember that standard deviation
             measures variability and we want to keep the variability around
             the line as small as possible.)"), 
          p("The regression line does not appear appropriately,
             when there exist outliers."), 
          collapsible = TRUE, 
          collapsed = TRUE, 
          width = 12
        ), 
        br(), 
        div(
          style = "text-align:center", 
          bsButton(
            inputId = "go2", 
            label = "GO!", 
            icon("bolt"), 
            size = "large"
          )
        )
      ), 
      ## Third tab - Challenge tab
      tabItem(tabName = "explore", 
              h2("Explore the regression line"), 
              textOutput("question"), 
              br(), 
              fluidRow(
                column(
                  width = 1, 
                  actionButton(inputId = "newchallenge", 
                               label = "New Challenge"), 
                  bsPopover(id = "newchallenge", title = "", 
                            content = "Click to start new challenge", 
                            trigger = "hover", place = "right")
                ), 
                column(
                  width = 1, offset = 5, 
                  actionButton(inputId = "clear", label = "RESET"), 
                  bsPopover(id = "clear", title = "", 
                            content = "Click RESET to start over this challenge", 
                            trigger = "hover", place = "right")
                )
              ), 
              br(), 
              fluidRow(
                column(width = 2, checkboxInput(inputId = "yourownline", 
                                         label = "Create your own line (Red)", 
                                         value = FALSE)), 
                column(
                  width = 4, 
                  sliderInput(
                    inputId = "intercept", 
                    label = "Choose the intercept:", 
                    min = -10, 
                    max = 10, 
                    value = 5, 
                    step = 0.01
                  )
                ), 
                column(
                  width = 4, 
                  offset = 1, 
                  sliderInput(
                    inputId = "slope", 
                    label = "Choose the slope:", 
                    min = -10, 
                    max = 10, 
                    value = 2, 
                    step = 0.01
                  )
                )
              ), 
              fluidRow(column(
                width = 2, 
                checkboxInput(
                  inputId = "regressionline", 
                  label = "Show regression line (black)", 
                  value =  FALSE
                )
              ), 
              column(
                width = 2, 
                offset = 4, 
                checkboxInput(
                  inputId = "correlation", 
                  label = "Show correlation value", 
                  value = FALSE
                )
               )
              ), 
              br(),
              textOutput('feedback'), 
              # Add a row for the main content
              fluidRow(
                # Create a space for the plot output and enable click function
                column(
                  width = 9, 
                  plotOutput(outputId = "clusterPlot", width = "100%", 
                             click = "clusterClick"), 
                  # Alt text
                  tags$script(
                    HTML(
                      "$(document).ready(function()
                                     { document.getElementById('clusterPlot').
                                     setAttribute('aria-label',
                                     `User can create points
                                     to test their challenges`)
                                     })"
                    )
                  )
                ), 
                column(
                  width = 3, br(),br(), 
                       conditionalPanel("input.yourownline !=0", 
                                        textOutput('yourline')), 
                       br(), 
                       conditionalPanel("input.regressionline !=0", 
                                        textOutput('regression_equation')), 
                       br(), 
                       conditionalPanel("input.correlation !=0", 
                                        textOutput('correlation'))
                       )
              )
      ), 
      tabItem(
        tabName = "References", 
        h2("References"), 
        p(     #shinyBS
          class = "hangingindent", 
          "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
            R package. Available from
            https://CRAN.R-project.org/package=shinyBS"
        ), 
        p(     #Boast Utilities
          class = "hangingindent", 
          "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
           Available from https://github.com/EducationShinyAppTeam/boastUtils"
        ), 
        p(     #shinydashboard
          class = "hangingindent", 
          "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
        ), 
        p(     #shiny
          class = "hangingindent", 
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
        ), 
        p(     #shinyWidgets
          class = "hangingindent", 
          "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny,
            R package. Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
        ), 
        p(     #reference for ideas
          class = "hangingindent", 
          "Statistical Applets - Correlation and Regression (n.d.), 
          Available from
          http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_5_correg.html"
        )
      )
    )
  )
)
