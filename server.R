# This is the server code of the waste calculation shiny app
#

library(shiny)
library(shinyjs)
library(rdrop2)
library(RMySQL)
library(RSQLite)
library(mongolite)
library(DBI)
library(factoextra)
library(psych)

#models that were developed using the data are called to the global environment to make predictions using the average value from all 3 models
#model_noR<- readRDS("./www/final_model_no_roof.rds")
model_noR_all<- readRDS("./www/allNoRoofModel.RDS")
model_R<- readRDS("./www/roofModel.RDS")

#the table that saves input data is defined and saved in the global env
input_table <<- data.frame(matrix(ncol=15, nrow = 0))
colnames(input_table) <<- c("project_name","city","working_days","h_f","bathrooms","floor_area","ext_perimeter","int_wall_length","sheet_cladding","brick_or_stone","board_cladding","roof_clad","roof_area","stories", "corners")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ############selctive action buttons ############################
  #here I use action buttons as a data input method to make the app look nicer. So once the action button is clicked the selection is saved in to the relevent variable in the global env
  #also to show the selection I use enable and disable options using shinyjs
  
  #h_f variable house or granny flat 
  observeEvent(input$h,{
    shinyjs::disable("h")
    shinyjs::enable("f")
    h_f <<- 1    })
  observeEvent(input$f,{
    shinyjs::disable("f")
    shinyjs::enable("h")
    h_f <<- 0
  })
  
  #stories 
  observeEvent(input$s1,{
    shinyjs::disable("s1")
    shinyjs::enable("s2")
    stories <<- 1    })
  
  
  observeEvent(input$s2,{
    shinyjs::enable("s1")
    shinyjs::disable("s2")
    stories <<- 2    })

  #bathrooms

  observeEvent(input$b2,{
    shinyjs::disable("b2")
    shinyjs::enable("b2_5")
    shinyjs::enable("b3")
    shinyjs::enable("b3_5")
    bathrooms <<- 2    })
  observeEvent(input$b2_5,{
    shinyjs::enable("b2")
    shinyjs::disable("b2_5")
    shinyjs::enable("b3")
    shinyjs::enable("b3_5")
    bathrooms <<- 2.5    })
  
  observeEvent(input$b3,{
    shinyjs::enable("b2")
    shinyjs::enable("b2_5")
    shinyjs::disable("b3")
    shinyjs::enable("b3_5")
    bathrooms <<- 3    })
  observeEvent(input$b3_5,{
    shinyjs::enable("b2")
    shinyjs::enable("b2_5")
    shinyjs::enable("b3")
    shinyjs::disable("b3_5")
    bathrooms <<- 3.5    })
  
  #roof cladding  
  observeEvent(input$S,{
    shinyjs::disable("S")
    shinyjs::enable("PM")
    roof_clad <<- "S"    })
  observeEvent(input$PM,{
    shinyjs::disable("PM")
    shinyjs::enable("S")
    roof_clad <<- "PM"
  })
  
  #builder type as dataset  
  #observeEvent(input$dataSet,{
   # dataSet <<- input$dataSet })

  
  
  
  
  ##################### Go Button ##################
  
  #this is created so that when the Gobutton (calculate waste button) is clicked it will first clear the output table of previous inputs, then check if all the input values are given and then calculate the waste using the 3 models
  
  predictionCal <- eventReactive(input$btn_go,{

    #resetting the output table so that previous results are not shown when run again
    updateData()
    
    #validation and error messages before running the calculation
    validate(
      need(input$project_id,message = "Enter a project name"),
      need(input$city,message = "Enter the location"),
      need(input$working_days,message = "Enter working days"),
      need(input$h || input$f, message = "Select the house type"),
      need(input$s1 || input$s2, message = "Select the number of stories/levels"),
      need(input$b2 || input$b2_5|| input$b3|| input$b3_5, message = "Select the number of bathrooms"),
      need(input$corners,message = "Enter number of corners"),
      need(input$floor_area,message = "Enter floor area"),
      need(input$ext_perimeter,message = "Enter external perimeter"),
      need(input$int_wall_length,message = "Enter internal wall length"),
      need(input$sheet_cladding,message = "Enter sheet cladding area"),
      need(input$board_cladding,message = "Enter board cladding area"),
      need(input$brick_or_stone,message = "Enter brick or stone area"),
      need(input$S || input$PM, message = "Select the roof cladding type"),
      need(input$roof_area,message = "Enter roof area")
    )
    input_table <<- data.frame(matrix(ncol=16, nrow = 1))
    colnames(input_table) <<- c("project_name","city","working_days","h_f","bathrooms","floor_area","ext_perimeter","int_wall_length","sheet_cladding","brick_or_stone","board_cladding","roof_clad","roof_area","stories", "corners","dataSet")
    
    #req(whichbutton())
    #h_f<<-whichbutton()
    
    output_table <<- input_table
    
    output_table<<-matrix(ncol=ncol(output_table), rep(NA, prod(dim(output_table))))
    p=1
    for (i in 1:1) {
      
      j=1
      input_table[p,j] <<- input$project_id
      input_table[p,j+1] <<- input$city
      input_table[p,j+2] <<- input$working_days
      input_table[p,j+3] <<- h_f
      input_table[p,j+4] <<- bathrooms
      input_table[p,j+5] <<- input$floor_area
      input_table[p,j+6] <<- input$ext_perimeter
      input_table[p,j+7] <<- input$int_wall_length
      input_table[p,j+8] <<- input$sheet_cladding
      input_table[p,j+9] <<- input$brick_or_stone
      input_table[p,j+10] <<- input$board_cladding
      input_table[p,j+11] <<- roof_clad
      input_table[p,j+12] <<- input$roof_area
      input_table[p,j+13] <<- stories
      input_table[p,j+14] <<- input$corners
      input_table[p,j+15] <<- input$dataSet
      
      j=1
      p=p+1
    }
    
    input_table$bathrooms<<- as.factor(input_table$bathrooms)
    input_table$stories<<- as.factor(input_table$stories)
    input_table$h_f<<- as.factor(input_table$h_f)
    input_table$roof_clad<<- as.factor(input_table$roof_clad)
    input_table$dataSet<<- as.factor(input_table$dataSet)
    
    
    ############################# getting pcs for roof data ################################################
    
    pca.roof <- readRDS("./www/roofPCA.RDS")
    
    roof_table <- input_table[c("floor_area","ext_perimeter","int_wall_length","roof_area")]
    roof_pc<-as.data.frame(predict(pca.roof,roof_table))
    
    input_table<-cbind(input_table,roof_pc)
    
    
    
    
    

    #################### getting predictions without roof ####################################################
    
    #model with all except roof variables
    
    preds_link = predict(model_noR_all, newdata = input_table,
                         type = "link",
                         se.fit = TRUE)# use the glm to make predictions, also provides  std. error
    critval <- qt(0.005, df = df.residual(model_noR_all), lower.tail = FALSE) # critical value for approx 95% CI
    se <- preds_link$se.fit
    upper_ci_link <- preds_link$fit + (critval * preds_link$se.fit)# estimate upper CI for prediction on link scale
    lwr_ci_link <<- preds_link$fit - (critval * preds_link$se.fit)# estimate lower CI for prediction on link scale
    fit_link <- preds_link$fit# returns fited value
    upper_ci <- model_noR_all$family$linkinv(upper_ci_link)
    lwr_ci <- model_noR_all$family$linkinv(lwr_ci_link)
    fit <- model_noR_all$family$linkinv(preds_link$fit)
    
    
    
    predicted_waste_noR2 <<- fit
    Lower_limit_noR2 <<- fit-lwr_ci
    
    
    
    #ending predictions with no roof
    
    
    #getting predictions with roof
    
    if(bathrooms==4){
      
      input_table$bathrooms = "3.5"
      #input_table$bathrooms<<- as.factor(input_table$bathrooms)
      
      preds_link = predict(model_R, newdata = input_table,
                           type = "link",
                           se.fit = TRUE)# use the glm to make predictions, also provides  std. error
      critval <- qt(0.005, df = df.residual(model_R), lower.tail = FALSE) # critical value for approx 95% CI
      se <- preds_link$se.fit
      upper_ci_link <- preds_link$fit + (critval * preds_link$se.fit)# estimate upper CI for prediction on link scale
      lwr_ci_link <<- preds_link$fit - (critval * preds_link$se.fit)# estimate lower CI for prediction on link scale
      fit_link <- preds_link$fit# returns fited value
      upper_ci <- model_R$family$linkinv(upper_ci_link)
      lwr_ci <- model_R$family$linkinv(lwr_ci_link)
      fit <- model_R$family$linkinv(preds_link$fit)
      
      predicted_waste_R <<- fit
      Lower_limit_R <<- fit-lwr_ci
      
      input_table[1,4] <<- bathrooms
      input_table$bathrooms<<- as.factor(input_table$bathrooms)
      
    } else {
      
      preds_link = predict(model_R, newdata = input_table,
                           type = "link",
                           se.fit = TRUE)# use the glm to make predictions, also provides  std. error
      critval <- qt(0.005, df = df.residual(model_R), lower.tail = FALSE) # critical value for approx 95% CI
      se <- preds_link$se.fit
      upper_ci_link <- preds_link$fit + (critval * preds_link$se.fit)# estimate upper CI for prediction on link scale
      lwr_ci_link <<- preds_link$fit - (critval * preds_link$se.fit)# estimate lower CI for prediction on link scale
      fit_link <- preds_link$fit# returns fited value
      upper_ci <- model_R$family$linkinv(upper_ci_link)
      lwr_ci <- model_R$family$linkinv(lwr_ci_link)
      fit <- model_R$family$linkinv(preds_link$fit)
      
      predicted_waste_R <<- fit
      Lower_limit_R <<- fit-lwr_ci
        
    }# ending predictions with roof data
    
    bathrooms2 <<- as.factor(bathrooms)
    input_table$bathrooms <<- bathrooms
    
    
    #final value of predictions averaged by the number of models, less weight given to the model with roof
    
    predicted_waste <<- (0.5*predicted_waste_noR2) + (0.5*predicted_waste_R)
    Lower_limit <<- (0.5*Lower_limit_noR2) + (0.5*Lower_limit_R)
    
    #adding prediction details to the output table
    output_table <<- input_table[,1:15]
    output_table$bathrooms <<- bathrooms
    output_table$pred <<- predicted_waste
    output_table$llimt <<- predicted_waste - Lower_limit
    output_table$ulimt <<- predicted_waste + Lower_limit
    
    colnames(output_table) <<- c("Project Name","Location","Working Days","House or House and flat","Bathrooms","Floor Area","External Perimeter","Internal Wall Length","Sheet Cladding Area","Brick and or Stone Area","Board Cladding Area","Roof Cladding Type","Roof Area","Number of Stories", "Number of Corners", "Preidcted Waste", "Lower Prediction Limit", "Upper Prediction Limit")
    
    saveData(input_table)
    
    
    HTML(paste("<b>","<span style=\"color:green\">Predicted waste is </span>", "<span style=\"color:green\">",round(predicted_waste,3),"(±",round(Lower_limit,3),"</span>","<span style=\"color:green\">) tonnes </span>","</b>"))
    
  })
  
  
  output$calWaste <- renderText(
    predictionCal()
    
  )
  
  
  
  ################calculation ends here#############################################
  
 
  ###############################Download button ##########################################
  
  
  output$ui_dlbtn <- renderUI({
    req(input$btn_go)
    downloadButton("dl_data", "Download")
    
  })
  
  
  
  
  
  output$dl_data <- downloadHandler(
    filename = function() { 
      paste0("data-", format(Sys.time(),"%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      
      write.csv(output_table, file)
      
    }
  )
  
  
  
  ########Download button ends here ###############################
  
  
  
  
  
  ##################### writing the files to mongodb servers #######################
  
  saveData <- function(data) {
    # Connect to the database
    db <- mongo(collection = "inputtable", db = "waste_responsesdb",
                url = "mongodb+srv://wasteapp:shiny1234@wmifcluster.hjl7evs.mongodb.net/test?retryWrites=true&w=majority")
    
    # Insert the data into the mongo collection as a data.frame, file
    data <- as.data.frame(data)
    db$insert(data)
  }
  
  
  ############### monogodb ends here ############################
  
  
  
  ###### reset button not working yet add code here later#######
  observeEvent(input$Reset, {
    reset("form")
  })
  ########## reset button ends here #####################
  
  
  ##################### update output_table so it is emptied after each run############################################
  
  
  values <- reactiveValues()
  updateData <- function() {
    vars <- load(file = "./www/output_table_sample.Rdata", envir = .GlobalEnv)
    for (var in vars)
      output_table <<- get(var, .GlobalEnv)
  }
  
  
  
  ###########end###############
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #error events for working days
  
  
  observeEvent(input$working_days, {
    req(input$working_days)
    
    #validate(
    #need(input$working_days,"Error message")
    #)
    
    Xworking_days <- input$working_days  # value 2
    
    
    
    min_Xworking_days <- 75
    max_Xworking_days <- 350
    
    if(Xworking_days > max_Xworking_days && Xworking_days!="") {
      ## Conditions
      output$errorwd <- renderText("Warning!!! This calculator is accurate for working days between 75 - 350")
      
    }
    else if(Xworking_days < min_Xworking_days && Xworking_days!="") {
      ## Conditions
      output$errorwd <- renderText("Warning!!! This calculator is accurate for working days between 75 - 350")
      
    }
    
    
    #else if(Xworking_days > min_Xworking_days) {
    ## Conditions
    #  output$errorwd <- renderText("")
    
    #}
    #else if(input$working_days=='') {
    # Conditions
    # output$errorwd <- renderText("")
    
    #}
    
    else {
      ## Conditions
      output$errorwd <- renderText("")
      
    }
    
    
  })
  
  #erro events for floor area
  observeEvent(input$floor_area, {
    
    req(input$floor_area)
    
    Xfloor_area <- input$floor_area  # value 2
    min_Xfloor_area <- 89
    max_Xfloor_area <- 337
    
    if(max_Xfloor_area < Xfloor_area) {
      ## Conditions
      output$errorfa <- renderText("Warning!!! This calculator is accurate for floor area between 89m2 - 337m2")
      
      
    }
    
    else if(Xfloor_area < min_Xfloor_area) {
      ## Conditions
      output$errorfa <- renderText("Warning!!! This calculator is accurate for floor area between 89m2 - 337m2")
      
    }
    
    else if(Xfloor_area > min_Xfloor_area) {
      ## Conditions
      output$errorfa <- renderText("")
      
    }
    
  }) 
  ##end of erro event
  
  
  #error events for int_wall_length
  observeEvent(input$int_wall_length, {
    
    req(input$int_wall_length)
    
    Xint_wall_length <- input$int_wall_length  # value 2
    min_Xint_wall_length <- 13
    max_Xint_wall_length <- 120
    
    if(max_Xint_wall_length < Xint_wall_length) {
      ## Conditions
      output$errorip <- renderText("Warning!!! This calculator is accurate for Internal wall perimeter length between 13m - 120m")
      
      
    }
    
    else if(Xint_wall_length < min_Xint_wall_length) {
      ## Conditions
      output$errorip <- renderText("Warning!!! This calculator is accurate for Internal wall perimeter length between 13m - 120m")
      
    }
    
    else if(Xint_wall_length > min_Xint_wall_length) {
      ## Conditions
      output$errorip <- renderText("")
      
    }
    
  }) 
  ##end of error event
  
  
  #error events for corners
  observeEvent(input$corners, {
    
    req(input$corners)
    
    Xcorners <- input$corners  # value 2
    min_Xcorners <- 9
    max_Xcorners <- 46
    
    if(max_Xcorners < Xcorners) {
      ## Conditions
      output$errorco <- renderText("Warning!!! This calculator is accurate for number of corners between 9 - 46")
      
      
    }
    
    else if(Xcorners < min_Xcorners) {
      ## Conditions
      output$errorco <- renderText("Warning!!! This calculator is accurate for number of corners between 9 - 46")
      
    }
    
    else if(Xcorners > min_Xcorners) {
      ## Conditions
      output$errorco <- renderText("")
      
    }
    
  }) 
  ##end of error event
  
  
  
  #error events for ext_wall_length
  observeEvent(input$ext_perimeter, {
    
    req(input$ext_perimeter)
    
    Xext_perimeter <- input$ext_perimeter  # value 2
    min_Xext_perimeter <- 40
    max_Xext_perimeter <- 132
    
    if(max_Xext_perimeter < Xext_perimeter) {
      ## Conditions
      output$errorep <- renderText("Warning!!! This calculator is accurate for external wall perimeter length between 40m - 132m")
      
      
    }
    
    else if(Xext_perimeter < min_Xext_perimeter) {
      ## Conditions
      output$errorep <- renderText("Warning!!! This calculator is accurate for Internal wall perimeter length between 40m - 132m")
      
    }
    
    else if(Xext_perimeter > min_Xext_perimeter) {
      ## Conditions
      output$errorep <- renderText("")
      
    }
    
  }) 
  ##end of error event
  
  #error events for brick_or_stone
  observeEvent(input$brick_or_stone, {
    
    req(input$brick_or_stone)
    
    Xbrick_or_stone <- input$brick_or_stone  # value 2 
    
    
    max_Xbrick_or_stone <- 250
    
    if(max_Xbrick_or_stone < Xbrick_or_stone) {
      ## Conditions
      output$errorbs <- renderText("Warning!!! This calculator is accurate for brick or stone cladding area between 0m2 - 250m2")
      
      
    }
    
    else if(max_Xbrick_or_stone > Xbrick_or_stone) {
      ## Conditions
      output$errorbs <- renderText("")
      
    }
    
    
    
    
  }) 
  ##end of error event
  
  #error events for sheet_cladding
  observeEvent(input$sheet_cladding, {
    
    req(input$sheet_cladding)
    
    Xsheet_cladding <- input$sheet_cladding  # value 2 
    
    max_Xsheet_cladding <- 240
    
    if(max_Xsheet_cladding < Xsheet_cladding) {
      ## Conditions
      output$errorsc <- renderText("Warning!!! This calculator is accurate for sheet cladding area between 0m2 - 240m2")
      
    }
    
    else if(max_Xsheet_cladding > Xsheet_cladding) {
      ## Conditions
      output$errorsc <- renderText("")
      
    }
    
  }) 
  ##end of error event
  
  #error events for roof_area
  observeEvent(input$roof_area, {
    
    req(input$roof_area)
    
    Xroof_area <- input$roof_area  # value 2 
    
    max_Xroof_area <- 460
    min_Xroof_area <- 211
    
    if(max_Xroof_area < Xroof_area) {
      ## Conditions
      output$errorra <- renderText("Warning!!! This calculator is accurate for roof area between 211m2 to 460m2")
      
    }
    
    else if(Xroof_area < min_Xroof_area ) {
      ## Conditions
      output$errorra <- renderText("Warning!!! This calculator is accurate for roof area between 211m2 to 460m2")
      
    }
    
    else if(max_Xroof_area > Xroof_area) {
      ## Conditions
      output$errorra <- renderText("")
      
    }
    
    
  }) 
  ##end of error event
  
})
