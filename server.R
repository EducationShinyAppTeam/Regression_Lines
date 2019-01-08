library(shiny)
library(Cairo) 

server <- function(input, output,session) {
  
  # Create a spot where we can store additional
  # reactive values for this session
  c <- reactiveValues(right=c(sample(1:11,1)))
  
  
  observeEvent(input$newchallenge,{
    c$right=sample(1:11,1)
    
  })
  
  observeEvent(input$nextbutton, {
    updateTabItems(session, "tabs", "overview")
  })
  
  observe({
    if (input$newchallenge > 0){
      val$x <- NULL
      val$y <- NULL
    }})
  
  
  # Clear the points on button click
  observe({
    if (input$clear > 0){
      val$x <- NULL
      val$y <- NULL
      
    }})
  
  observe({
    input$clear
    updateSliderInput(session,"slope",value = 0)
    updateSliderInput(session, "intercept", value =0)
  })
  
  observe({
    if(input$clear > 0){
      updateCheckboxInput(session=session, inputId ="yourownline", label = "Create your own line",value = FALSE )
      updateCheckboxInput(session=session, inputId ="correlation", label = "Show correlation",value = FALSE )
      updateCheckboxInput(session=session, inputId ="regressionline", label = "Show regression",value = FALSE )
      
    }
  })
  
  observe({
    input$newchallenge
    updateSliderInput(session,"slope",value = 0)
    updateSliderInput(session, "intercept", value =0)
  })
  
  observe({
    if(input$newchallenge > 0){
      updateCheckboxInput(session=session, inputId ="yourownline", label = "Create your own line",value = FALSE)
      updateCheckboxInput(session=session, inputId ="correlation", label = "Show correlation",value = FALSE)
      updateCheckboxInput(session=session, inputId ="regressionline", label = "Show regression",value = FALSE)
    }})

  
  observeEvent(input$nextbutton, {
    
    updateTabItems(session, "tabs", "overview2")
    
  })
  
  observeEvent(input$start, {
    updateTabItems(session, "tabs", "regression")
  })

  observeEvent(input$inst2,{
    sendSweetAlert(
      session = session,
      title = "Instruction:",
      type = "info",
      print("1. Click New Challenge to get a challenge.\n2. Create your own line by entering the values for both slope and intercept.\n3. Create points by clicking in the plot.\n4. Show the regression line to do a competition.\n5. Click clear points to clean both points and regression lines.")
    )
  })
 
  
  output$yourline <- renderText({
    
    paste("Equation of your line:y =",input$intercept,"+", input$slope,"* x")
    
  })
  
  
  output$question1<- renderText({
    
    if (c$right == 1){
      "Challenge: Draw some points on a scatterplot that have y = 3 – 2x as their regression and a correlation more than - 0.8 (then check the “Regression line” box to see how you did)."
    }
    else if  (c$right == 2){
      "Challenge: Draw some points on a scatterplot that have y = 2 + 3x as their regression and a correlation less than 0.5 (then check the “Regression line” box to see how you did)."
    }
    else if  (c$right == 3){
      "Challenge: Draw some points on a scatterplot that have y = 2x + 1 as their regression (then check the “Show regression” box to see how you did)."   
    }
    else if  (c$right == 4){
      "Challenge: Draw your own line with the equation y = 3 – 2x and then add some points that have that as their regression (then check the “Show regression” box to see how you did)."   
    }
    else if  (c$right == 5){
      "Challenge: Draw your own line with the equation y = 2 + 3x and then add some points that have that as their regression (then check the “Show regression” box to see how you did)."
    }
    else if  (c$right == 6){
      "Challenge: Create some points with a correlation of 0.6 and then draw your own line that is your guess at the regression line (then check the “Show regression” box to see how you did)."
    }
    else if  (c$right == 7){
      "Challenge: Create some points with a correlation of -0.5 and then draw your own line that is your guess at the regression line (then check the “Show regression” box to see how you did)."   
    }
    else if  (c$right == 8){
      "Challenge: create some points with a correlation of -0.2 and then draw your own line that is your guess at the regression line (then check the “Show regression” box to see how you did)."   
    }
    else if  (c$right == 9){
      "Challenge: Create some points that have a strong non-linear relationship but a correlation between -0.1 and 0.1."
    }
    else if  (c$right == 10){
      "Challenge: Create some points that have a strong non-linear relationship but a correlation between 0.4 and 0.6."
    }
    else if  (c$right == 11){
      "Challenge: Create some points that follow a roughly linear pattern and a correlation of 0.5 and then add an outlier to make the correlation go down to 0."
    }
  })
  
  val <- reactiveValues(x=NULL, y=NULL)    
  
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

      
      data1 <- data.frame(c(val$x, val$y), ncol=2)
      
      # Try to cluster       
      if (length(val$x) <= 1){
        stop("We can't cluster less than 2 points")
      } 
      suppressWarnings({
        fit <- Mclust(data)
      })
      
      mclust2Dplot(data = data1, what = "classification", 
                   classification = fit$classification, main = FALSE,
                   xlim=c(-2,2), ylim=c(-0.2,5),cex=input$opt.cex, cex.lab=input$opt.cexaxis)
    }, error=function(warn){
      # Otherwise just plot the points and instructions
      plot(val$x, val$y, xlim=c(-5, 5), ylim=c(-0.2, 5), xlab="X", ylab="Y",
           cex=2.5, cex.axis=1.5,pch=16,col="blue")
      
      if (input$yourownline > 0 ){
        
        abline(input$intercept,input$slope,col="red", lwd = "3.8")
        
      }
      output$feedback<-renderText({
        if (c$right == 1){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round( cor(val$x,val$y),digits = 2) > -0.8){
            paste("The correlation is correct!
                  \nPlease check the “Regression line” box to see how you did for regression")
          }
          else if(length(val$x) >= 3 & round( cor(val$x,val$y),digits = 2) <= -0.8){
            paste("Sorry,correlation <= -0.8 
                  \nPlease add other points or try again for the correlation.
                  \nPlease check the “Regression line” box to see how you did for regression")
          }
        }
        
        else if(c$right == 2){
          if(length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round( cor(val$x,val$y),digits = 2) < 0.5){
            paste("The correlation is correct!
                  \nPlease check the “Regression line” box to see how you did for regression")
          }
          else if(length(val$x) >= 3 & round( cor(val$x,val$y),digits = 2) >= 0.5){
            paste("Sorry, correlation >= 0.5 
                  \nPlease add other points or try again for the correlation.
                  \nPlease check the “Regression line” box to see how you did for regression")
          }
        }
        
        else if (c$right == 3){
          if(length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3){
            paste("Please check the “Show regression” box to see how you did for regression")
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
            paste("Please check the “Show regression” box to see how well do the lines match")
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
            paste("Please check the “Show regression” box to see how well do the lines match")
          }
        }
        
        else if (c$right == 6){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) == 0.6){
            paste("The correlation is correct!
                  \nPlease guess and draw the regression line.
                  \nThen check the “Regression line” box to see how you did.")
          }
          else if(length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) != 0.6){
            paste("Sorry,correlation is not equal to 0.6 
                  \nPlease add other points or try again for the correlation.
                  \nYou can check the “Show correlation” box to see the current correlation.")
          }
        }
        
        else if (c$right == 7){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) == -0.5){
            paste("The correlation is correct!
                  \nPlease guess and draw the regression line.
                  \nThen check the “Regression line” box to see how you did.")
          }
          else if(length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) != -0.5){
            paste("Sorry,correlation is not equal to -0.5
                  \nPlease add other points or try again for the correlation.
                  \nYou can check the “Show correlation” box to see the current correlation.")
          }
        }
        
        else if (c$right == 8){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) == -0.2){
            paste("The correlation is correct!
                  \nPlease guess and draw the regression line.
                  \nThen check the “Regression line” box to see how you did.")
          }
          else if(length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) != -0.2){
            paste("Sorry,correlation is not equal to -0.2
                  \nPlease add other points or try again for the correlation.
                  \nYou can check the “Show correlation” box to see the current correlation.")
          }
        }
        
        else if(c$right == 9){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) >= -0.1 & round( cor(val$x,val$y),digits = 1) <= 0.1){
            paste("The correlation is correct!
                  \nPlease make sure that your graph shows a strong non-linear correlation.")
          }
          else if (length(val$x) >= 3 & (round( cor(val$x,val$y),digits = 1) < -0.1 | round( cor(val$x,val$y),digits = 1) > 0.1)){
            paste("Sorry, correlation is not between -0.1 and 0.1
                  \nPlease add other points or try again for the correlation.
                  \nAlso, please make sure that your graph shows a strong non-linear correlation.")
          }
        }
        
        else if (c$right == 10){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3 & round( cor(val$x,val$y),digits = 1) >= 0.4 & round( cor(val$x,val$y),digits = 1) <= 0.6){
            paste("The correlation is correct!
                  \nPlease make sure that your graph shows a strong non-linear correlation.")
          }
          else if (length(val$x) >= 3 & (round( cor(val$x,val$y),digits = 1) < 0.4 | round( cor(val$x,val$y),digits = 1) > 0.6)){
            paste("Sorry, correlation is not between 0.4 and 0.6
                  \nPlease add other points or try again for the correlation.
                  \nAlso, please make sure that your graph shows a strong non-linear correlation.")
          }
        }
        
        else if (c$right == 11){
          if (length(val$x) < 3 | is.null(val$x) == TRUE){
            paste("Please click to add more points.")
          }
          else if (length(val$x) >= 3){
            paste("You can check the “Show correlation” box to see the current correlation.")
          }
        }
        
      })
      
      if (input$regressionline  == "TRUE" & length(val$x) >= 3 ){
        
        abline(lm(val$y ~ val$x, data = data1),lwd = "4")

        mod_name <-lm(val$y ~ val$x, data = data1)
        mod_name$coeff[2]
        
        output$regression_equation<-renderText({
  
        
         # paste("Regression Equation : y =",round(mod_name$coeff[2],digits = 2)," * x +",round(mod_name$coeff[1],digits = 2 ))  
          paste("Regression Equation : y =",round(mod_name$coeff[1],digits = 2),"+",round(mod_name$coeff[2],digits = 2),"* x")  
          
          
        })
      } 
      

      else if (input$regressionline  == "TRUE" & length(val$x) < 3 ){
        output$regression_equation<-renderText({
          
          paste("Please click to add more points.")
          
        })}
        

      else if (input$regressionline  == "TRUE" & length(val$x) < 3 ){
        output$regression_equation<-renderText({
          paste("Please click to add more points.") 
        })

      }
      if (input$correlation  == "TRUE" & length(val$x) >= 3 ){
        

        output$correlation<-renderText({
          paste(  "Correlation = ",round( cor(val$x,val$y),digits = 5))  
          
        })
      }
      
      else if (input$correlation  == "TRUE" & length(val$x) < 3 ){
        output$correlation<-renderText({
          paste("Please click to add more points.") 
          
        })
      }
    })
  })
}

