# Load Libraries
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Regression Lines"
APP_DESCP  <<- paste(
  "This app allows a user to explore the relationship between correlation and",
  "linear regression."
)
## End App Meta Data------------------------------------------------------------

# Define global constants and functions ----

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "yellow",
    ## Header ----
    dashboardHeader(
      titleWidth = 250,
      title = "Regression Lines",
      tags$li(class = "dropdown",
              actionLink("inst", icon("info",class = "myClass"))),
      tags$li(
        class = 'dropdown',
        tags$a(href = "https://shinyapps.science.psu.edu/",
               icon('home', lib = 'font-awesome'))
      )
    ),
    ## Sidebar ----
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
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview ----
        tabItem(
          tabName = "overview",
          h1("Regression Lines"),
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
            tags$li("Click RESET to clear both points and regression lines.")
          ),
          br(),
          div(
            style = "text-align: center;",
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
            div(class = "updated", "Last Update: 9/15/2020 by NJH.")
          )
        ),
        ### Prerequisites ----
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
            style = "text-align: center;",
            bsButton(
              inputId = "go2",
              label = "GO!",
              icon("bolt"),
              size = "large"
            )
          )
        ),
        ## Challenge ----
        tabItem(
          tabName = "explore",
          h2("Explore the Regression Line and Correlation"),
          uiOutput("question", class = "largerFont"),
          br(),
          fluidRow(
            column(
              width = 1,
              bsButton(
                inputId = "newchallenge",
                label = "New Challenge",
                size = "large"
              )
            ),
            column(
              width = 1,
              offset = 5,
              bsButton(
                inputId = "clear",
                label = "Reset",
                size = "large",
                style = "danger"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 3,
              checkboxInput(
                inputId = "yourownline",
                label = "Create your own line (Red)",
                value = FALSE
              )
            ),
            column(
              width = 4,
              sliderInput(
                inputId = "intercept",
                label = "Choose the intercept",
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
                label = "Choose the slope",
                min = -10,
                max = 10,
                value = 2,
                step = 0.01
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              checkboxInput(
                inputId = "regressionline",
                label = "Show regression line (black)",
                value =  FALSE
              )
            ),
            column(
              width = 4,
              offset = 2,
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
              plotOutput(
                outputId = "clusterPlot",
                width = "100%",
                click = "clusterClick"
              ),
              # Alt text
              tags$script(
                HTML(
                  "$(document).ready(function(){
                  document.getElementById('clusterPlot').setAttribute('aria-label',
                  `User can create points to test their challenges`)})"
                )
              )
            ),
            column(
              width = 3,
              br(),
              br(),
              conditionalPanel(
                condition = "input.yourownline !=0",
                textOutput('yourline')
              ),
              br(),
              conditionalPanel(
                condition = "input.regressionline !=0",
                textOutput('regression_equation')
              ),
              br(),
              conditionalPanel(
                condition = "input.correlation !=0",
                textOutput('correlation')
              )
            )
          )
        ),
        ### References ----
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
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the Server ----
server <- function(input, output,session) {
  ## Info Button ----
  observeEvent(input$inst,{
    sendSweetAlert(
      session = session,
      title = "Instructions",
      type = "info",
      tags$ol(
        tags$li('Click New Challenge to change a challenge.'),
        tags$li('Create your own line by entering the values for
                both slope and intercept.'),
        tags$li('Create points by clicking in the plot.'),
        tags$li('Click RESET to clear both points and regression lines.')
      )
    )
  })

  ## Create Reactive Values ----
  c <- reactiveValues(right = c(sample(1:11,1)))
  val <- reactiveValues(x = NULL, y = NULL)

  ## First Go button  ----
  observeEvent(input$go1, {
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "prerequisite"
    )
  })

  ## Second Go button ----
  observeEvent(input$go2, {
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "explore"
    )
  })

  ## New Challenge button ----
  observeEvent(input$newchallenge, {
    ### Reset values
    c$right <- sample(1:11,1)
    val$x <- NULL
    val$y <- NULL

    ### Update inputs
    updateSliderInput(
      session = session,
      inputId = "slope",
      value = 0
    )
    updateSliderInput(
      session = session,
      inputId = "intercept",
      value = 0
    )
    updateCheckboxInput(
      session = session,
      inputId ="yourownline",
      value = FALSE
    )
    updateCheckboxInput(
      session = session,
      inputId = "correlation",
      value = FALSE
    )
    updateCheckboxInput(
      session = session,
      inputId = "regressionline",
      value = FALSE
    )
  })
  ## Reset Button ----
  observeEvent(input$clear, {
    ### Reset values
    val$x <- NULL
    val$y <- NULL

    #Update inputs
    updateSliderInput(
      session = session,
      inputId = "slope",
      value = 0
    )
    updateSliderInput(
      session = session,
      inputId = "intercept",
      value = 0
    )
    updateCheckboxInput(
      session = session,
      inputId = "yourownline",
      value = FALSE
    )
    updateCheckboxInput(
      session = session,
      inputId = "correlation",
      value = FALSE
    )
    updateCheckboxInput(
      session = session,
      inputId = "regressionline",
      value = FALSE
    )
  })

  ## User's Linear Equation ----
  output$yourline <- renderText({
    paste("Equation of your line: y =",
          input$intercept,"+", "(", input$slope,")", "* x")
  })

  # REVISE Generate and Display Questions ----
  output$question <- renderText({
    if (c$right == 1){
      "Challenge: Draw some points on a scatterplot that have y = 3 – 2x
      as their regression and a correlation more than - 0.8.
      (Check the “Regression line” box to see how you did.)"
    }
    else if  (c$right == 2){
      "Challenge: Draw some points on a scatterplot that have y = 2 + 3x
      as their regression and a correlation less than 0.5.
      (Check the “Regression line” box to see how you did.)"
    }
    else if  (c$right == 3){
      "Challenge: Draw some points on a scatterplot that have y = 2x + 1
      as their regression. (Check the “Show regression”
      box to see how you did.)"
    }
    else if  (c$right == 4){
      "Challenge: Draw your own line with the equation y = 3 – 2x
      and then add some points that have that as their regression.
      (Check the “Show regression” box to see how you did.)"
    }
    else if  (c$right == 5){
      "Challenge: Draw your own line with the equation y = 2 + 3x
      and then add some points that have that as their regression.
      (Check the “Show regression” box to see how you did.)"
    }
    else if  (c$right == 6){
      "Challenge: Create some points with a correlation of 0.6
      and then draw your own line that is your guess at the regression line.
      (Check the “Show regression” box to see how you did.)"
    }
    else if  (c$right == 7){
      "Challenge: Create some points with a correlation of -0.5 and
      then draw your own line that is your guess at the regression line.
      (Check the “Show regression” box to see how you did.)"
    }
    else if  (c$right == 8){
      "Challenge: create some points with a correlation of -0.2
      and then draw your own line that is your guess at the regression line.
      (Check the “Show regression” box to see how you did.)"
    }
    else if  (c$right == 9){
      "Challenge: Create some points that have a strong non-linear relationship,
      but a correlation between -0.1 and 0.1."
    }
    else if  (c$right == 10){
      "Challenge: Create some points that have a strong non-linear relationship,
      but a correlation between 0.4 and 0.6."
    }
    else if  (c$right == 11){
      "Challenge: Create some points that follow a roughly linear pattern
      and a correlation of 0.5 and then add an outlier to make
      the correlation go down to 0."
    }
  })

  # Listen for clicks
  observe({
    # Initially will be empty
    if (is.null(input$clusterClick)){
      return()
    }
    isolate({
      val$x <- c(val$x, input$clusterClick$x)
      val$y <- c(val$y, input$clusterClick$y)
    })
  })
  output$clusterPlot <- renderPlot({
    tryCatch({
      # Format the data as a matrix
      data1 <- data.frame(c(val$x, val$y), ncol = 2)
      # Try to cluster
      if (length(val$x) <= 1){
        stop("We can't cluster less than 2 points")
      }
      suppressWarnings({
        fit <- Mclust(data)
      })
      mclust2Dplot(data = data1, what = "classification",
                   classification = fit$classification, main = FALSE,
                   xlim = c(-2,2), ylim = c(-0.2,5),
                   cex = 1.5,
                   cex.lab = 1.5)
    }, error = function(warn){
      # Otherwise just plot the points and instructions
      plot(val$x, val$y, xlim = c(-5, 5), ylim = c(-0.2, 5), xlab = "X",
           ylab = "Y", cex = 1.5, cex.lab = 1.5,
           cex.axis = 1.5, pch = 16, col = "blue")
      if (input$yourownline > 0 ){
        abline(input$intercept, input$slope, col="red", lwd = "3.8")
      }
      # Feedback for each challenge
      output$feedback<-renderText({
        if (c$right == 1){ # for the first challenge
          # if point is less than 3,
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          # if the points is greater or equal than 3
          # and correlation is greater than -0.8,
          else if (length(val$x) >= 3 & round(cor(val$x,val$y),
                                              digits = 2) > -0.8){
            paste("The correlation is correct!
                  Please check the “Regression line” box to see
                  how you did for regression.")
          }
          # if the points is greater or equal than 3
          # and correlation is less than -0.8,
          else if(length(val$x) >= 3 & round(cor(val$x, val$y),
                                             digits = 2) <= -0.8){
            paste("Sorry, correlation <= -0.8.",
                  "Please add other points or try again for the correlation.",
                  "Please check the “Regression line” box to see
                  how you did for regression")
          }
        }
        else if(c$right == 2){
          if(length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round(cor(val$x, val$y),
                                              digits = 2) < 0.5){
            paste("The correlation is correct!",
                  "Please check the “Regression line” box to see
                  how you did for regression")
          }
          else if(length(val$x) >= 3 & round(cor(val$x,val$y),
                                             digits = 2) >= 0.5){
            paste("Sorry, correlation >= 0.5.",
                  "Please add other points or try again for the correlation.",
                  "Please check the “Regression line” box to see
                  how you did for regression")
          }
        }
        else if (c$right == 3){
          if(length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3){
            paste("Please check the “Show regression” box to see
                  how you did for regression")
          }
        }
        else if (c$right == 4){
          if(is.null(val$x) == TRUE){
            paste("Please draw the line first.")
          }
          else if (length(val$x) < 3){
            paste("Please add more points ")
          }
          else if (length(val$x >= 3)){
            paste("Please check the “Show regression” box to see
                  how well do the lines match")
          }
        }
        else if (c$right == 5){
          if(is.null(val$x) == TRUE){
            paste("Please draw the line first.")
          }
          else if (length(val$x) < 3){
            paste("Please add more points ")
          }
          else if (length(val$x >= 3)){
            paste("Please check the “Show regression” box to see
                  how well do the lines match")
          }
        }
        else if (c$right == 6){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round(cor(val$x, val$y),
                                              digits = 1) == 0.6){
            paste("The correlation is correct!",
                  "Please guess and draw the regression line.",
                  "Then check the “Regression line” box to see how you did.")
          }
          else if(length(val$x) >= 3 & round( cor(val$x, val$y),
                                              digits = 1) != 0.6){
            paste("Sorry, correlation is not equal to 0.6.",
                  "Please add other points or try again for the correlation.",
                  "You can check the “Show correlation value” box
                  to see the current correlation.")
          }
        }
        else if (c$right == 7){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round(cor(val$x, val$y),
                                              digits = 1) == -0.5){
            paste("The correlation is correct!",
                  "Please guess and draw the regression line.",
                  "Then check the “Regression line” box to see how you did.")
          }
          else if(length(val$x) >= 3 & round(cor(val$x, val$y),
                                             digits = 1) != -0.5){
            paste("Sorry, correlation is not equal to -0.5.",
                  "Please add other points or try again for the correlation.",
                  "You can check the “Show correlation value” box
                  to see the current correlation.")
          }
        }
        else if (c$right == 8){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round(cor(val$x, val$y),
                                              digits = 1) == -0.2){
            paste("The correlation is correct!",
                  "Please guess and draw the regression line.",
                  "Then check the “Regression line” box to see how you did.")
          }
          else if(length(val$x) >= 3 & round(cor(val$x, val$y),
                                             digits = 1) != -0.2){
            paste("Sorry,correlation is not equal to -0.2.",
                  "Please add other points or try again for the correlation.",
                  "You can check the “Show correlation value” box
                  to see the current correlation.")
          }
        }
        else if(c$right == 9){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round(cor(val$x,val$y), digits = 1)
                   >= -0.1 & round(cor(val$x,val$y), digits = 1) <= 0.1){
            paste("The correlation is correct!",
                  "Please make sure that your graph shows a strong non-linear
                  correlation.")
          }
          else if (length(val$x) >= 3 & (round(cor(val$x,val$y), digits = 1)
                                         < -0.1 | round(cor(val$x,val$y), digits = 1) > 0.1)){
            paste("Sorry, correlation is not between -0.1 and 0.1.",
                  "Please add other points or try again for the correlation.",
                  "Also, please make sure that your graph shows
                  a strong non-linear correlation.")
          }
        }
        else if (c$right == 10){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round(cor(val$x,val$y), digits = 1)
                   >= 0.4 & round(cor(val$x,val$y), digits = 1) <= 0.6){
            paste("The correlation is correct!",
                  "Please make sure that your graph shows a strong non-linear
                  correlation.")
          }
          else if (length(val$x) >= 3 & (round(cor(val$x,val$y), digits = 1)
                                         < 0.4 | round(cor(val$x,val$y), digits = 1) > 0.6)){
            paste("Sorry, correlation is not between 0.4 and 0.6.",
                  "Please add other points or try again for the correlation.",
                  "Also, please make sure that your graph shows a strong
                  non-linear correlation.")
          }
        }
        else if (c$right == 11){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3){
            paste("You can check the “Show correlation value” box to see
                  the current correlation.")
          }
        }
      })
      ## show regression equation
      if (input$regressionline  == "TRUE" & length(val$x) >= 3 ){
        abline(lm(val$y ~ val$x, data = data1), lwd = "4")
        mod_name <- lm(val$y ~ val$x, data = data1)
        mod_name$coeff[2]
        output$regression_equation <- renderText({
          paste("Regression Equation : y =",
                round(mod_name$coeff[1], digits = 2), "+", "(",
                round(mod_name$coeff[2], digits = 2), ")", "* x")
        })
      }
      else if (input$regressionline  == "TRUE" & length(val$x) < 3 ){
        output$regression_equation <- renderText({
          paste("Please click to add more points.")
        })}
      # show correlation
      if (input$correlation  == "TRUE" & length(val$x) >= 3 ){
        output$correlation <- renderText({
          paste("Correlation = ", round(cor(val$x,val$y), digits = 2))
        })
      }
      else if (input$correlation  == "TRUE" & length(val$x) < 3 ){
        output$correlation <- renderText({
          paste("Please click to add more points.")
        })
      }
    })
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)