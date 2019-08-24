shinyUI(fluidPage(
  
  titlePanel("我們與 R 的距離"),
  
  sidebarLayout(
    sidebarPanel(
      
      h3("Decision Tree"),
      br(),
      numericInput("DT_minsplit", "minsplit", value=1, min=0, max=100, step=1) ,
      numericInput("DT_cp", "cp", value=0.3, min=0, max=1, step=0.01) ,
      numericInput("DT_maxdepth", "maxdepth", value=5, min=1, max=30, step=1) 
      
    ),
    
    #Show the RMSE values
    mainPanel(
      DT::dataTableOutput("outputTable")
    )
  )
  
))
