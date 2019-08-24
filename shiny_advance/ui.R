shinyUI(fluidPage(
  
  titlePanel("我們與 R 的距離"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("model", h3("Model Select"),
                   choices = list("Linear Regression" = "Linear", 
                                  "SVR" = "SVR",
                                  "Decision Tree" = "DT",
                                  "Random Forest" = "RF",
                                  "XGBoost" = "XGBoost"),
                   selected = "Linear"),
      
      uiOutput("inputGroup"),
      
      actionButton("executeButton", "Run")
    ),
    
    #Show the RMSE values
    mainPanel(
      DT::dataTableOutput("outputTable")
    )
  )
  
))
