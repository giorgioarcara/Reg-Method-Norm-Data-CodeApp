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
           "<li><code>Sex</code> as either 0 or 1. E.g. 0 for Males, and 1 for Females. The mapping of values (e.g., which number corresponds to which sex) should be defined clearly in your documentation or data processing pipeline.</li>", 
           "<li><code>Score</code> as a numerical variable</li>"),
      HTML("<br><b>NOTE</b>: <br> - If you use the regression method script please cite: Arcara G. (2024) Improving Equivalent Scores: A new method for regression model selection.<i>Neurological Sciences, 45(12),</i> 5685-5695<br> - If you also use the ES, please add: <i> Aiello, E. N., & Depaoli, E. G. (2022). Norms and standardizations in neuropsychology via equivalent scores: software solutions and practical guides. Neurological Sciences, 43(2), 961-966. </i>"),
      HTML("<br><br><b>WARNING</b>: Proper regression modeling should also include diagnostic inspection. Please check that the fit is appropriate.<br>"),
      HTML(paste("<i>Last modification:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),"</i>"))
    ),
  )
)


server <- function(input, output) {
  
  result <- reactiveVal()
  observeEvent(input$action_1, { result(1)})
  observeEvent(input$action_2, { result(2) })
  
  # MAIN PANEL 2 (PLOT)
  output$result1 <- renderPlot({
    
    
    # default result is Null. It would return an error
    if (!is.null(result())){ 
      
      
      # INPUT BUTTON
      if (result()==1){
        
        if (is.null(input$file1)){
          #plot(rnorm(10))
          plot(0, type="n", xlab="", ylab="", axes=F, xlim=c(-1, 1), ylim=c(-1,1))
          text(0, 0, labels="Before calculating the results\n you need to upload a valid .csv file", cex=1.5, font=2) # 
        }
        
        if ((is.na(input$max_val)|is.na(input$min_val)) & !is.null(input$file1)){
          #plot(rnorm(10))
          plot(0, type="n", xlab="", ylab="", axes=F, xlim=c(-1, 1), ylim=c(-1,1))
          text(0, 0, labels="Before calculating the results, please specify\n the minimumn and maximum observable Score values", cex=1.5, font=2) # 
        } 
        
        if (!(is.na(input$max_val)|is.na(input$min_val)) & !is.null(input$file1)){
          
          
          dat = read.csv(input$file1$datapath)
          if (length(setdiff(c("Age", "Education", "Sex", "Score"), names(dat))) > 0 ){
            plot(NA, axes=F, frame.plot=F, ylab="", xlab="", xlim=c(-1,1), ylim=c(-1,1))
            text(0,0, "The file is not appropriate.\n Check the instructions\n", cex=1.5, font=2)
            text(0,-0.5, "The file column names should be: Age, Education, Sex, and Score", cex=1.5, font=1)
            text(0,-1, "The .csv file must have comma ',' as field delimiter", cex=1.5, font=1)
            
            
          } else {
            plot(NA, axes=F, frame.plot=F, ylab="", xlab="", xlim=c(-1,1), ylim=c(-1,1))
            
            dat.res = adjscores_A2024(df = dat, dep="Score", age="Age", edu="Education", sex="Sex", dep.range = c(input$min_val, input$max_val))
            assign("dat.res", dat.res, envir=.GlobalEnv)
            
            ES.res = ES(adjscores = dat.res$new.df$ADJ_SCORES)
            ES.adj = as.data.frame(t(ES.res$Adjusted_Scores))
            row.names(ES.adj)="Adjusted Score"
            assign("ES.adj", ES.adj, envir=.GlobalEnv)
            assign("ES.res", ES.res, envir=.GlobalEnv)
            
            
            
            
            adj_text = adj_scores_transf_text(dat.res$lm.model, transfs=dat.res$transfs, transfs.names = c("age_funct", "edu_funct"), new.names=c("Age", "Edu"), dat=dat)
            assign("adj_text", adj_text, envir = .GlobalEnv)
            
            
            dat.lm = dat.res$lm.model        
            par(mar=c(10,2,10,2))
            plot(allEffects(dat.lm, partial.residuals=T), residuals.cex=0.2)
            
            
            # MAIN PANEL 1 (TEXT) # note, it should be here cause it use dat.res, which is created above
            output$result2 <- renderText({ 
              paste('<b><font size="3"> Regression function : ',as.character(dat.res$model_text), '</b></font> <br>')
            })
            
            # MAIN PANEL 3 (ADJUSTED SCORES FORMLA)
            output$result3 <- renderText({ 
              paste('<b><font size="3">',  adj_text, '</b></font> <br>')
            })
            
            # MAIN PANEL 4 (EQUIVALENT SCORES)
            output$result4 <- renderTable({ 
              ES.adj
            }, rownames=T)
            
            
            
          }
        }
      }
    }    
  })
  
  
  
  
  
  
  #### 
  output$downloadData <- downloadHandler(
    filename = "Reg-Method_Exported_Results.txt",
    content = function(file){# Write the dataset to the `file` that will be downloaded
      
      if(!exists("dat.res")){
        write("It seems you tried to export a .csv before uploading the data and/or computing the results. Please upload a valid .csv file and follow the steps before exporting the data.", file)
        
      } else{
        
        write(paste("Regression function :", as.character(dat.res$model_text), sep=""), file)
        write(adj_text, file, append = T)
        write("\n", file, append = T)
        write(paste(names(ES.res$Adjusted_Scores), collapse="\t"), file, append = T)
        write(ES.res$Adjusted_Scores, file, append = T)
        
        citation="\n\nNOTE:\n - If you use the regression method script please cite: Arcara G. (2024) Improving Equivalent Scores: A new method for regression model selection\n - If you use the ES, please cite: Aiello, E. N., & Depaoli, E. G. (2022). Norms and standardizations in neuropsychology via equivalent scores: software solutions and practical guides. Neurological Sciences, 43(2), 961-966. "
        warning = "\n WARNING: Proper regression modeling should also include diagnostic inspection. Please check that the fit is appropriate."
        
        write(citation, file, append=T)      
        write(warning, file, append=T)  
      }
      
    }
  )
  
}
shinyApp(ui, server)