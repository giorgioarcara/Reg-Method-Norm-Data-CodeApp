#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls())

source("R_functions/adjscores_A2024_v3.R")
source("R_functions/formula_transf_text.R")
source("R_functions/model_transf_text.R")
source("R_functions/adj_scores_transf_text.R")
source("R_functions/ES.R")
source("R_functions/tolLimits.adjscores.R")
source("R_functions/tolLimits.obs.R")


source("R_functions/transf_functions.R")


library(shiny)

require(effects)
library(car)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Arcara 2024 Regression method for normative data"),br(),
      fileInput("file1", "1. Upload a csv File", accept = ".csv"),
      p("Click this button to upload a .csv file.",
        " It should include four columns labeled:", code("Age, Education, Sex, Score"),
        " and should use", code('","'), "(comma)", "as delimiter"),
      br(),
      numericInput("min_val", label ="2a. set min Score value", 0 , min = 0, max = NA, step = 1),
      numericInput("max_val", label ="2b. set max Score value", NA , min = 0, max = NA, step = 1),
      p("insert here the", em("minimum"), "achievable score,
        and the",  em("maximum"), "achievable score (necessary for the method)"),
      br(),
      actionButton("action_1", strong("3. Calculate results")),
      #actionButton("action_2", strong("3. Reset results")),
      p("Click this button calculate the results"),
      br(),
      downloadButton("downloadData", strong("4. Export .csv")),
      #actionButton("action_2", strong("4. Export .csv")),
      p("Click this button to export the results",
        "A .csv file will be saved on your computer")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 htmlOutput("result2"),
                 br(),
                 plotOutput("result1"),
                 htmlOutput("result3"),
                 br(),
                 htmlOutput("result4"),
                 br(),
                 HTML("This shiny app accompanies the article  Arcara G. (2024) 'Improving Equivalent Scores: a new regression method'<br><br>"),
                 HTML("<b>Instructions</b>: The .CSV File supplied should have <code>,</code> as delimiter and should include the following columns:<br>",
                      "<li><code>Age</code>", "should be a numerical variable</li>",
                      "<li><code>Education</code> as a numerical variable</li>",
                      "<li><code>Sex</code> as either 0 or 1. E.g. 0 for Males, and 1 for Females.</li>", 
                      "<li><code>Score</code> as a numerical variable</li>"),
                 HTML("<br><b>NOTE</b>: <br> - If you use the regression method script please cite: Arcara G. (2024) Improving Equivalent Scores: A new method for regression model selection.<i>Neurological Sciences, 45(12),</i> 5685-5695<br> - If you also use the ES, please add: <i> Aiello, E. N., & Depaoli, E. G. (2022)...</i>"),
                 HTML("<br><br><b>WARNING</b>: Regression modeling should always include diagnostic inspection. Please check that the fit is appropriate checking the Model Diagnostics Tab."),
                 HTML(paste("<i>Last modification:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),"</i>"))
        ),
        
        tabPanel("Model Diagnostics",
                 h4("Diagnostic Plots for Regression Model"),
                 uiOutput("diagnostics_ui")
        ),
      )
    )
  )
)
  
  server <- function(input, output) {
    
    result <- reactiveVal(NULL)
    data_raw <- reactive({
      req(input$file1)
      read.csv(input$file1$datapath)
    })
    
    dat_res <- reactiveVal(NULL)
    model_fit <- reactiveVal(NULL)
    adj_text <- reactiveVal(NULL)
    es_adj <- reactiveVal(NULL)
    es_res <- reactiveVal(NULL)
    
    observeEvent(input$action_1, {
      
      # Validate input file
      if (is.null(input$file1)) {
        showModal(modalDialog(
          title = "Missing Input",
          "Please upload a CSV file and set both minimum and maximum score values before calculating.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
      
      # Validate min and max score values
      if (is.na(input$min_val) || is.na(input$max_val)) {
        showNotification("⚠ Please enter both minimum and maximum score values.", type = "error")
        return()
      }
      
      dat <- read.csv(input$file1$datapath)
      
      if (!all(c("Age", "Education", "Sex", "Score") %in% colnames(dat))) {
        showNotification("⚠ File must contain columns: Age, Education, Sex, Score", type = "error")
        return()
      }
      
      datres <- adjscores_A2024(df = dat, dep = "Score", age = "Age", edu = "Education", sex = "Sex", dep.range = c(input$min_val, input$max_val))
      dat_res(datres)
      model_fit(datres$lm.model)
      adj_text(datres$adj_text)
      
      esr <- ES(adjscores = datres$new.df$ADJ_SCORES)
      es_adj(as.data.frame(t(esr$Adjusted_Scores)))
      es_res(esr)
      
      result(TRUE)
    })
    
    ## OUTPUTS
    
    output$result1 <- renderPlot({
      req(result())
      
      if (is.null(dat_res())) {
        plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
        text(0, 0, "Model not calculated.\nUpload file and click 'Calculate results'", cex = 1.5)
      } else {
        dat.lm <- model_fit()
        par(mar = c(10, 2, 10, 2))
        plot(allEffects(dat.lm, partial.residuals = TRUE), residuals.cex = 0.5, residuals.pch=19)
      }
    })
    
    output$result2 <- renderText({
      req(dat_res())
      paste('<b><font size="3"> Regression function : ', as.character(dat_res()$model_text), '</b></font> <br>')
    })
    
    output$result3 <- renderText({
      req(adj_text())
      paste('<b><font size="3">', adj_text(), '</b></font> <br>')
    })
    
    output$result4 <- renderTable({
      req(es_adj())
      es_adj()
    }, rownames = TRUE)
    
    
    # Model Diagnostics tab ----
    output$diagnostics_ui <- renderUI({
      if (is.null(model_fit())) {
        return(
          div(
            style = "padding: 20px; font-size: 16px;",
            strong("⚠ Please upload data and calculate the model to view diagnostics.")
          )
        )
      }
      
      # Show diagnostic plots if model exists
      tagList(
        tagList(
          plotOutput("diag_resid_fitted"),
          p(em("A good fit shows residuals scattered randomly around zero without a pattern.")),
          br(),
          plotOutput("diag_qq"),
          p(em("Points should fall approximately along the line, indicating normally distributed residuals.")),
          br(),
          plotOutput("diag_resid_hist"),
          p(em("Residuals should resemble a bell curve; deviations suggest non-normality.")),
          br(),
          plotOutput("diag_scale_location"),
          p(em("A good fit has a horizontal line with equally spread points, showing constant variance."))
          
         # plotOutput("diag_cooks"),
          # p(em("Most values should be small; large spikes may indicate influential data points.")),
          
        )
      )
    })
    
    
    observe({
      req(model_fit())
    })
    
    output$diag_resid_fitted <- renderPlot({
      req(model_fit())
      plot(model_fit()$fitted.values, resid(model_fit()),
           xlab = "Fitted values", ylab = "Residuals", col = "orange", pch=19,
           main = "Residuals vs Fitted")
      abline(h = 0, col = "red")
    })
    
    output$diag_qq <- renderPlot({
      req(model_fit())
      qqnorm(resid(model_fit()), col = "orange", pch=19)
      qqline(resid(model_fit()), col = "red")
    })
    
    output$diag_scale_location <- renderPlot({
      req(model_fit())
      sqrt_abs_resid <- sqrt(abs(resid(model_fit())))
      plot(model_fit()$fitted.values, sqrt_abs_resid,
           xlab = "Fitted values", ylab = "Sqrt(|Residuals|)", col = "orange", pch=19,
           main = "Scale-Location")
      abline(h = 0, col = "red")
    })
    
    output$diag_cooks <- renderPlot({
      req(model_fit())
      plot(cooks.distance(model_fit()), type = "h",
           main = "Cook's Distance", ylab = "Distance")
    })
    
    output$diag_resid_hist <- renderPlot({
      req(model_fit())
      res <- resid(model_fit())
      
      hist(res,
           probability = TRUE,
           breaks = 20,
           col = "lightblue",
           border = "white",
           main = "Histogram of Residuals with Normal Curve",
           xlab = "Residuals")
      
      # Add normal curve
      xfit <- seq(min(res), max(res), length = 100)
      yfit <- dnorm(xfit, mean = mean(res), sd = sd(res))
      lines(xfit, yfit, col = "red", lwd = 2)
    })
    
    # Export button
    output$downloadData <- downloadHandler(
      filename = "Reg-Method_Exported_Results.txt",
      content = function(file) {
        if (is.null(dat_res()) || is.null(es_res())) {
          write("Model not calculated yet. Upload a valid file and calculate first.", file)
        } else {
          write(paste("Regression function :", as.character(dat_res()$model_text), sep = ""), file)
          write(adj_text(), file, append = TRUE)
          write("\n", file, append = TRUE)
          write(paste(names(es_res()$Adjusted_Scores), collapse = "\t"), file, append = TRUE)
          write(es_res()$Adjusted_Scores, file, append = TRUE)
          
          citation <- "\n\nNOTE:\n - If you use the regression method script please cite: Arcara G. (2024)..."
          warning <- "\n WARNING: Proper regression modeling should also include diagnostic inspection."
          
          write(citation, file, append = TRUE)
          write(warning, file, append = TRUE)
        }
      }
    )
  }
  
  shinyApp(ui, server)