shinyServer(function(input, output) {
  # observe changes in "numInputs", and create corresponding number of inputs
  input_list <- list()
  output$inputGroup = renderUI({
    
    if( input$model == "Linear"){
      input_list <- list()
    }else if( input$model == "SVR"){
      input_list <- list(numericInput("SVR_cost", "cost", value=1, min=1, max=1000, step=1) ,
                         numericInput("SVR_epsilon", "epsilon", value=0.1, min=0, max=0.99, step=0.01)  )
    }else if( input$model == "DT"){
      input_list <- list(numericInput("DT_minsplit", "minsplit", value=1, min=0, max=100, step=1) ,
                         numericInput("DT_cp", "cp", value=0.3, min=0, max=1, step=0.01) ,
                         numericInput("DT_maxdepth", "maxdepth", value=5, min=1, max=30, step=1) )
    }else if( input$model == "RF"){
      input_list <- list(numericInput("RF_ntree", "ntree", value=500, min=1, max=10000, step=1) ,
                         numericInput("RF_mtry", "mtry", value=10, min=1, max=(ncol(dataset)-1), step=1) ,
                         selectInput("RF_replace", "replace", choices = list("TRUE" = TRUE, "FALSE" = FALSE), selected = TRUE) ) 
    }else if( input$model == "XGBoost"){
      input_list <- list(numericInput("XGBoost_nrounds", "nrounds", value=1, min=1, max=1000, step=1) ,
                         numericInput("XGBoost_eta", "eta", value=0.3, min=0, max=10, step=0.1) ,
                         numericInput("XGBoost_gamma", "gamma", value=0, min=0, max=100, step=0.01) ,
                         numericInput("XGBoost_max_depth", "max_depth", value=3, min=1, max=100, step=1) ,
                         numericInput("XGBoost_min_child_weight", "min_child_weight", value=1, min=1, max=100, step=1) ,
                         numericInput("XGBoost_subsample", "subsample", value=1, min=1, max=100, step=1) ,
                         numericInput("XGBoost_colsample_bytree", "colsample_bytree", value=1, min=1, max=100, step=1) )
    }
    
    do.call(tagList, input_list)
  })
  
  # Default Table
  resultFrame <- reactiveValues(
    result = data.frame(Parameter = character(),
                        RMSE = numeric(), 
                        stringsAsFactors = FALSE) )
  
  observeEvent(input$executeButton, {
    if( input$model == "Linear"){
      
      regressor <- lm(formula = SalePrice_log ~ ., data = training_set)
      
      # Predicting the Test set results
      y_pred <- predict(regressor, newdata = val_set)
      
      # performance evaluation
      resultFrame$result <- bind_rows(
        resultFrame$result,
        data.frame(Parameter = paste0(input$model),
                   RMSE = rmse(val_set$SalePrice_log, y_pred), 
                   stringsAsFactors = FALSE)
      )
      
    }else if( input$model == "SVR"){
      # Fitting SVR to the dataset library(e1071)
      regressor <- svm(formula = SalePrice_log ~ ., data = training_set, type = "eps-regression",kernel = "radial", 
                       cost = input$SVR_cost, epsilon = input$SVR_epsilon)
      
      # Predicting the Test set results
      y_pred <- predict(regressor, newdata = val_set)
      
      # performance evaluation
      resultFrame$result <- bind_rows(
        resultFrame$result,
        data.frame(Parameter = paste0(input$model," ; "," cost : ",input$SVR_cost," epsilon : ",input$SVR_epsilon),
                   RMSE = rmse(val_set$SalePrice_log, y_pred), 
                   stringsAsFactors = FALSE)
      )
    }else if( input$model == "DT"){
      # Fitting Decision Tree Regression to the dataset library(rpart)
      regressor <- rpart(formula = SalePrice_log ~ ., data = training_set, 
                         control = rpart.control(minsplit = input$DT_minsplit, cp = input$DT_cp, maxdepth = input$DT_maxdepth))
      
      # Predicting the Test set results
      y_pred <- predict(regressor, newdata = val_set)
      
      # performance evaluation
      resultFrame$result <- bind_rows(
        resultFrame$result,
        data.frame(Parameter = paste0(input$model," ; "," minsplit : ",input$DT_minsplit," cp : ",input$DT_cp," maxdepth : ",input$DT_maxdepth),
                   RMSE = rmse(val_set$SalePrice_log, y_pred), 
                   stringsAsFactors = FALSE)
      )
    }else if( input$executeButton >0 & input$model == "RF"){
      # Fitting Random Forest Regression to the dataset library(randomForest)
      set.seed(1)
      regressor <- randomForest(formula = SalePrice_log ~ ., data = training_set, 
                                ntree = input$RF_ntree, mtry = input$RF_mtry, replace = ifelse(input$RF_replace=="TRUE",TRUE,FALSE))
      
      # Predicting the Test set results
      y_pred <- predict(regressor, newdata = val_set)
      
      # performance evaluation
      resultFrame$result <- bind_rows(
        resultFrame$result,
        data.frame(Parameter = paste0(input$model," ; "," ntree : ",input$RF_ntree," mtr : ",input$RF_mtr," replace : ",input$RF_replace),
                   RMSE = rmse(val_set$SalePrice_log, y_pred), 
                   stringsAsFactors = FALSE)
      )
    }else if( input$executeButton >0 & input$model == "XGBoost"){
      dtrain <- xgb.DMatrix(data = tr_x, label = tr_y) 
      dval <- xgb.DMatrix(data = val_x, label = val_y)
      
      #default parameters
      default_params <- expand.grid(
        nrounds = input$XGBoost_nrounds,
        max_depth = input$XGBoost_max_depth,
        eta = input$XGBoost_eta,
        gamma = input$XGBoost_gamma,
        colsample_bytree = input$XGBoost_colsample_bytree,
        min_child_weight = input$XGBoost_min_child_weight,
        subsample = input$XGBoost_subsample
      )
      
      
      train_control <- caret::trainControl(
        method = "none",
        verboseIter = FALSE, # no training log
        allowParallel = TRUE # FALSE for reproducible results 
      )
      
      xgb_base <- caret::train(
        x = tr_x,
        y = tr_y,
        trControl = train_control,
        tuneGrid = default_params,
        method = "xgbTree",
        verbose = TRUE
      )
      
      # performance evaluation
      resultFrame$result <- bind_rows(
        resultFrame$result,
        data.frame(Parameter = paste0(input$model," ; ",input$XGBoost_nrounds,
                                      " eta : ",input$XGBoost_eta, " gamma : ",input$XGBoost_gamma, " max_depth : ",input$XGBoost_max_depth,
                                      " min_child_weight : ",input$XGBoost_min_child_weight, " subsample : ",input$XGBoost_subsample, " colsample_bytree : ",input$XGBoost_colsample_bytree),
                   RMSE = ModelMetrics::rmse(val_y, predict(xgb_base, newdata = val_x)), 
                   stringsAsFactors = FALSE)
      )
    }
  })
  
  output_table <- reactive({resultFrame$result})  
  #observe(output$rmseValues <- renderText(HTML(rv$output_rmse)))
  
  output$outputTable <- DT::renderDataTable({
    output_table()
  })
  
})
