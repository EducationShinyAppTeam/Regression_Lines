# Load Libraries
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

# Define global constants and functions ----
## none at this time

# Define the UI ----
ui <- list(
  ## app page ----
  dashboardPage(
    skin = "yellow",
    dashboardHeader(
      titleWidth = 250,
      title = "Regression Lines",
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Regression_lines")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          id = "home",
          href = 'https://shinyapps.science.psu.edu/',
          icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Challenge", tabName = "challenge", icon = icon("gears")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview ----
        tabItem(
          tabName = "overview",
          h1("Regression Lines"),
          p(
            "In this app, you will explore how correlation and
          regression lines relate to the points on a scatterplot."
          ),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li(
              "Head to the 'Prerequisites' page to review concepts related to
              correlation and regression lines."
            ),
            tags$li(
              "When ready, make your way to the 'Challenge' page to further explore
              these concepts."
            ),
            tags$li(
              "There, you can create points by clicking on the scatterplot and create
              your own line by adjusting the sliders for slope and intercept."
            ),
            tags$li("Use these interactive elements to mimic the situation that
                    is presented to complete the challenges.")
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToPrereq",
              label = "Prerequisites",
              icon = icon("book"),
              size = "large",
            )
          ),
          #Acknowledgements
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Caihui Xiao.
            The app was further updated by Zhiliang Zhang and Jiajun Gao
            in June 2018, Daehoon Gwak in July 2020, Sean Burke in June
            2023, and by Nathan Pechulis in July 2024.",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/8/2024 by NP.")
          )
        ),
        ### Prerequisites ----
        tabItem(
          withMathJax(),
          tabName = "prerequisites",
          h2("Prerequisites"),
          p("To get the most out of this app, review the following information
            about correlation and regression lines."),
          br(),
          box(
            title = tags$strong("Correlation"),
            tags$ul(
              tags$li(
                "Correlation measures the strength of the linear association
                between two variables/attributes and is therefore not appropriate
                for non-linear relationships. Correlations are always values
                between –1 and +1, inclusive. A correlation of –1 or +1 indicates
                that we have a perfect linear relationship; every point lies on
                the regression line. Correlation is strongly affected by outliers."
              ),
              tags$li(
                "Correlation does not change when you change the unit of
                measurement. For instance, if you find the correlation between
                elementary school children's height (in inches) and their ages
                (years), that value will be the same if you convert everyone's
                height to centimeters and/or their ages to days. More generally,
                if you transform every observation's values by adding,
                subtracting, multiplying, and/or dividing by the same number,
                the correlation will remain the same."
              )
            ),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          ),
          box(
            title = tags$strong("Regression Lines"),
            tags$ul(
              tags$li(
                "A regression line is the graph of a linear function thought to
                describe how a response variable (typically \\(Y\\)) changes as
                an explanatory variable (typically \\(X\\)) changes. The regression
                line's linear form focuses on the", tags$em("predicted value"),
                "of the response (\\(\\widehat{Y}\\)). Thus, when you know that
                \\(X\\) takes on a specific value, \\(x\\), you can find the
                predicted value with the formula: \\[\\widehat{Y}=a+b*x\\]"
              ),
              tags$li(
                "In the above formula, \\(a\\) is an intercept--what you would
                predict for \\(Y\\) when \\(x = 0\\). The \\(b\\) is the slope or
                rate of change of \\(Y\\) with respect to \\(X\\). The slope tells
                you how many times the change in \\(Y\\) is as large as the
                corresponding change in \\(X\\). The sign of the slope will always
                match the sign of the correlation value."
              ),
              tags$li(
                "The regression line is the graph of the linear function that
                minimizes the vertical distance between the line and each of the
                cases (data points). This results in the smallest standard deviation
                for the error (i.e., the vertical distance between the line and
                each case). Recall that  standard deviation measures variability
                and we want to keep the variability around the line as small as
                possible."
              ),
              tags$li(
                "The regression line is not appropriate for making predictions
                when",
                tags$ul(
                  tags$li("there is a non-linear relationship,"),
                  tags$li("when there are outliers driving the prediction and/or
                          relationship, or"),
                  tags$li("when you are predicting far beyond the range of the
                          data.")
                )
              )
            ),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12
          ),
          br(),
          br()
        ),
        ### Challenge ----
        tabItem(
          tabName = "challenge",
          h2("Regression Line and Correlation on a Scatterplot"),
          p(
            "Use the interactive graph below to mimic the challenge that is being
            proposed. To begin, create points by simply clicking on the graph and
            adjust the values for your line's slope/intercept (if a line is needed)
            while also checking the 'Create your own line' box. Then, you can
            utilize the given feedback from the other two checkboxes to get as
            close to the prompt as you can. Try your best to complete as many of
            the challenges as you can; good luck!"
          ),
          br(),
          h3("Challenge"),
          uiOutput(
            outputId = "question",
            class = "largerFont"
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "intercept",
                  label = "Choose the intercept",
                  min = -10,
                  max = 10,
                  value = 5,
                  step = 0.01
                ),
                sliderInput(
                  inputId = "slope",
                  label = "Choose the slope",
                  min = -10,
                  max = 10,
                  value = 2,
                  step = 0.01
                ),
                checkboxInput(
                  inputId = "userLine",
                  label = "Create your own line (red)",
                  value = FALSE
                ),
                checkboxInput(
                  inputId = "regressionLine",
                  label = "Show regression line (blue)",
                  value =  FALSE
                ),
                checkboxInput(
                  inputId = "correlation",
                  label = "Show correlation value",
                  value = FALSE
                ),
                bsButton(
                  inputId = "newChallenge",
                  label = "New Challenge",
                  icon = icon("forward"),
                  size = "large"
                ),
                bsButton(
                  inputId = "undoButton",
                  label = "Undo Point",
                  icon = icon("undo"),
                  size = "large",
                  disabled = TRUE
                ),
                bsButton(
                  inputId = "reset",
                  label = "Reset",
                  icon = icon("eraser"),
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              plotOutput(
                outputId = "scatterPlot",
                click = "clicks"
              ),
              br(),
              h4("Feedback"),
              textOutput('feedback'),
              textOutput('userFunction'),
              textOutput('regEqn'),
              textOutput('correlation')
            )
          )
        ),
        ### References ----
        tabItem(
          tabName = "References",
          h2("References"),
          p(     #shinyBS
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(     #Boast Utilities
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2024). boastUtils: BOAST utlities.
            (v 0.1.12.2). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(     #shinydashboard
            class = "hangingindent",
            "Chang, W., and Borges Ribeiro, B. (2021). shinydashboard: Create
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(     #shiny
            class = "hangingindent",
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2024). shiny:
            Web application framework for R. (v 1.8.1.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(     #shinyWidgets
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2024). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.8.6). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(     #reference for ideas
            class = "hangingindent",
            "Statistical Applets: Correlation and regression. (n.d.). [Web applet;
            original idea]. Available from
            http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_5_correg.html"
          ),
          p(     # ggplot2
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.5.1) [R package]. Available from
            https://ggplot2.tidyverse.org"
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
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Information",
        type = "info",
        text = "This app will allow you to explore the connection between the data
        points, their correlation, and the regression line through challenges
        using an interactive scatterplot."
      )
    }
  )

  ## Create Reactive Values ----
  userData <- reactiveVal(NULL)
  challengeId <- reactiveVal(sample(x = 1:11, size = 1))

  ## Prerequisites button  ----
  observeEvent(
    eventExpr = input$goToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )

  ## New Challenge button ----
  observeEvent(
    eventExpr = input$newChallenge,
    handlerExpr = {
      ### Reset values
      userData(NULL)
      challengeId(sample(x = 1:11, size = 1))

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
        inputId = "userLine",
        value = FALSE
      )
      updateCheckboxInput(
        session = session,
        inputId = "correlation",
        value = FALSE
      )
      updateCheckboxInput(
        session = session,
        inputId = "regressionLine",
        value = FALSE
      )
    }
  )

  ## Reset Button ----
  observeEvent(
    eventExpr = input$reset,
    handlerExpr = {
      ### Reset values
      userData(NULL)

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
        inputId = "userLine",
        value = FALSE
      )
      updateCheckboxInput(
        session = session,
        inputId = "correlation",
        value = FALSE
      )
      updateCheckboxInput(
        session = session,
        inputId = "regressionLine",
        value = FALSE
      )
    }
  )

  ## Generate and Display Questions ----
  observeEvent(
    eventExpr = challengeId(),
    handlerExpr = {
      output$question <- renderText(
        expr = {
          switch(
            EXPR = challengeId(), # integer selecting element
            "Draw some points on a scatterplot that have y = 3 – 2x as their
            regression and a correlation more than - 0.8. (Check the “Regression
            line” box to see how you did.)",
            "Draw some points on a scatterplot that have y = 2 + 3x as their
            regression and a correlation less than 0.5. (Check the “Regression
            line” box to see how you did.)",
            "Draw some points on a scatterplot that have y = 2x + 1 as their
            regression. (Check the “Show regression” box to see how you did.)",
            "Draw your own line with the equation y = 3 – 2x and then add some
            points that have that as their regression. (Check the “Show
            regression” box to see how you did.)",
            "Draw your own line with the equation y = 2 + 3x and then add some
            points that have that as their regression. (Check the “Show
            regression” box to see how you did.)",
            "Create some points with a correlation of 0.6 and then draw your own
            line that is your guess at the regression line. (Check the “Show
            regression” box to see how you did.)",
            "Create some points with a correlation of -0.5 and then draw your
            own line that is your guess at the regression line. (Check the “Show
            regression” box to see how you did.)",
            "Create some points with a correlation of -0.2 and then draw your own
            line that is your guess at the regression line. (Check the “Show
            regression” box to see how you did.)",
            "Create some points that have a strong non-linear relationship, but
            a correlation between -0.1 and 0.1.",
            "Create some points that have a strong non-linear relationship, but
            a correlation between 0.4 and 0.6.",
            "Create some points that follow a roughly linear pattern and a
            correlation of 0.5 and then add an outlier to make the correlation
            go down to 0."
          )
        }
      )
    }
  )

  ## Listen for clicks ----
  observeEvent(
    eventExpr = input$clicks,
    handlerExpr = {
      if (is.null(userData())) {
        userData(data.frame(x = input$clicks$x, y = input$clicks$y))
      } else if (nrow(userData()) < 1) {
        userData(data.frame(x = input$clicks$x, y = input$clicks$y))
      } else {
        userData(rbind(userData(), c(input$clicks$x, input$clicks$y)))
      }
    }
  )
  ## Enable/Disable Undo ----
  observeEvent(
    eventExpr = userData(),
    handlerExpr = {
      if (is.null(userData())) {
        updateButton(
          session = session,
          inputId = "undoButton",
          disabled = TRUE
        )
      } else if (nrow(userData()) == 0) {
        ## separate null case from zero row case to prevent app from crashing
        updateButton(
          session = session,
          inputId = "undoButton",
          disabled = TRUE
        )
      } else {
        updateButton(
          session = session,
          inputId = "undoButton",
          disabled = FALSE
        )
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  ## Undo Last Point Button ----
  observeEvent(
    eventExpr = input$undoButton,
    handlerExpr = {
      # Remove the last row from the dataframe
      userData(userData()[-nrow(userData()),])
    }
  )

  ## Render Scatter Plot ----
  observeEvent(
    eventExpr = c(userData(), input$userLine, input$intercept, input$slope,
                  input$regressionLine),
    handlerExpr = {
      ### Capture Data ----
      if (is.null(userData())) {
        plotData <- data.frame(x = numeric(), y = numeric())
      } else if (nrow(userData()) < 1) {
        plotData <- data.frame(x = numeric(), y = numeric())
      } else {
        plotData <- userData()
      }

      ### Generate base plot ----
      basePlot <- ggplot(
        data = plotData,
        mapping = aes(x = x, y = y)
      ) +
        geom_point(
          size = 4.5,
          color = boastPalette[1]
        ) +
        scale_x_continuous(
          breaks = seq(-5, 5, 2),
          limits = c(-5,5),
          expand = expansion(mult = 0, add = 0)
        ) +
        scale_y_continuous(
          breaks = seq(-5, 5, 2),
          limits = c(-5,5),
          expand = expansion(mult = 0, add = 0)
        ) +
        labs(x = "X", y = "Y") +
        theme_bw(base_size = 20)

      ### Add user's line ----
      if (input$userLine) {
        basePlot <- basePlot +
          geom_abline(
            intercept = input$intercept,
            slope = input$slope,
            col =  boastUtils::psuPalette[2],
            lwd = 1.25
          )
      }

      ### Add regression line ----
      if (input$regressionLine) {
        basePlot <- basePlot +
          geom_smooth(
            method = "lm",
            formula = y ~ x,
            se = FALSE,
            fullrange = TRUE,
            color = boastPalette[5],
            linewidth = 1.25,
            na.rm = TRUE
          )
      }

      ### Build Alt text ----

      if (is.null(userData())) {
        altText <- "An empty plot going from -5 to 5 in both dimensions."
      } else if (nrow(userData()) < 1) {
        altText <- "An empty plot going from -5 to 5 in both dimensions."
      } else if (nrow(userData()) < 3) {
        altText <- paste0("A scatter plot showing ", nrow(userData()), " points")
      } else {
        sampleCor <- round(cor(userData())["x", "y"], digits = 2)
        trendMsg <- ifelse(
          test =  sampleCor >= -0.1 && sampleCor <= 0.1,
          yes = "no meaningful trend.",
          no = ifelse(
            test = sampleCor > 0.1,
            yes = "a positive trend.",
            no = "a negative trend."
          )
        )
        altText <- paste0("A scatter plot showing ", nrow(userData()),
                          " points and ", trendMsg)
      }

      if (input$userLine) {
        altText <- paste(altText, "The user specified line also appears.")
      }

      if (input$regressionLine) {
        altText <- paste(altText, "The regression line also appears.")
      }

      output$scatterPlot <- renderPlot(
        expr = basePlot,
        alt = altText
      )
    }
  )

  ## Display feedback messages ----
  ### User's Linear Function ----
  observeEvent(
    eventExpr = c(input$userLine, input$intercept, input$slope),
    handlerExpr = {
      if (!input$userLine) {
        message <- NULL
      } else {
        message <- paste(
          "Equation of your line: y =",
          input$intercept,
          "+ (",
          input$slope,
          ") * x"
        )
      }

      output$userFunction <- renderText(expr = message)
    }
  )

  ### Regression Equation ----
  observeEvent(
    eventExpr = c(input$regressionLine, userData()),
    handlerExpr = {
      if (!input$regressionLine) {
        message <- NULL
      } else if (is.null(userData())) {
        message <- "You must plot points first."
      } else if (nrow(userData()) <= 1) {
        message <- "More points are needed for a regression line."
      } else {
        model1 <- lm(formula = y ~ x, data = userData())

        message <- paste(
          "Regression Equataion: y =",
          round(coef(model1)[1], digits = 2),
          "+ (",
          round(coef(model1)[2], digits = 2),
          ") * x"
        )
      }

      output$regEqn <- renderText(expr = message)
    }
  )

  ### Sample Correlation ----
  observeEvent(
    eventExpr = c(input$correlation, userData()),
    handlerExpr = {
      if (!input$correlation) {
        message <- NULL
      } else if (is.null(userData())) {
        message <- "You must plot points first."
      } else if (nrow(userData()) < 3) {
        message <- "Create more points to display the correlation value."
      } else {
        message <- paste(
          "Correlation =",
          round(cor(userData())["x", "y"], digits = 2)
        )
      }

      output$correlation <- renderText(expr = message)
    }
  )

  ### Challenge feedback ----
  observeEvent(
    eventExpr = c(userData(), challengeId()),
    handlerExpr = {
      if (is.null(userData())) {
        message <- "Please click within the plot to add points."
      } else if (nrow(userData()) < 3) {
        message <- "Please plot three or more points."
      } else {
        if (challengeId() == 1) {
          message <- ifelse(
            test = round(cor(userData())["x", "y"], digits = 2) > -0.8,
            yes = "You're on the correct path. Please check the 'Regression line'
            box to see how you did for regression.",
            no = "Sorry, you're sample correlation is too small. Please add/remove
            points and keep trying."
          )
        } else if (challengeId() == 2) {
          message <- ifelse(
            test = round(cor(userData())["x", "y"], digits = 2) < 0.5,
            yes = "You're on the correct path. Please check the 'Regression line'
            box to see how you did for regression.",
            no = "Sorry, you're sample correlation is too large. Please add/remove
            points and keep trying."
          )
        } else if (challengeId() == 3) {
          message <- "Please check the 'Regression line' box to see how you did
          for regression."
        } else if (challengeId() == 4) {
          message <- "Be sure to draw the line first. Please check the 'Regression
          line' box to see how you did for regression."
        } else if (challengeId() == 5) {
          message <- "Be sure to draw the line first. Please check the 'Regression
          line' box to see how you did for regression."
        } else if (challengeId() == 6) {
          message <- ifelse(
            test = round(cor(userData())["x", "y"], digits = 1) == 0.6,
            yes = "Your correlation is close enough. Use the controls to draw your
            guess for the regression line. Please check the 'Regression line' box
            to see how you did for regression.",
            no = "Your correlation isn't close enough to 0.6. Please add/remove
            points and keep trying. You can use the 'Show correlation value' button
            to see the current sample correlation value."
          )
        } else if (challengeId() == 7) {
          message <- ifelse(
            test = round(cor(userData())["x", "y"], digits = 1) == -0.5,
            yes = "Your correlation is close enough. Use the controls to draw your
            guess for the regression line. Please check the 'Regression line' box
            to see how you did for regression.",
            no = "Your correlation isn't close enough to -0.5. Please add/remove
            points and keep trying. You can use the 'Show correlation value' button
            to see the current sample correlation value."
          )
        } else if (challengeId() == 8) {
          message <- ifelse(
            test = round(cor(userData())["x", "y"], digits = 1) == -0.2,
            yes = "Your correlation is close enough. Use the controls to draw your
            guess for the regression line. Please check the 'Regression line' box
            to see how you did for regression.",
            no = "Your correlation isn't close enough to -0.5. Please add/remove
            points and keep trying. You can use the 'Show correlation value' button
            to see the current sample correlation value."
          )
        } else if (challengeId() == 9) {
          sampleCor <- round(cor(userData())["x", "y"], digits = 1)
          message <- ifelse(
            test = sampleCor >= -0.1 && sampleCor <= 0.1,
            yes = "Your correlation is within the bounds. Make sure that your graph
            shows a strong non-linear relationship.",
            no = "Your correlation isn't between -0.1 and 0.1. Add/remove points
            and keep trying. Make sure that your graph shows a non-linear relationship."
          )

        } else if (challengeId() == 10) {
          sampleCor <- round(cor(userData())["x", "y"], digits = 1)
          message <- ifelse(
            test = sampleCor >= 0.4 && sampleCor <= 0.6,
            yes = "Your correlation is within the bounds. Make sure that your graph
            shows a strong non-linear relationship.",
            no = "Your correlation isn't between -0.1 and 0.1. Add/remove points
            and keep trying. Make sure that your graph shows a non-linear relationship."
          )
        } else if (challengeId() == 11) {
          message <- "Use the check boxes to check your work."
        } else {
          print("ERROR IN CHALLENGE ID")
        }
      }

      output$feedback <- renderText(expr = message)
    }
  )

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)