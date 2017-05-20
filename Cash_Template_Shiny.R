## app.R ##
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Cash Forecasting"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Loader", tabName = "Data_Loader", icon = icon("Data Loader")),
      menuItem("Result", tabName = "Result", icon = icon("Result")),
      menuItem("SVM", tabName = "SVM", icon = icon("SVM"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # Data Loader Contents =========================================================================================
      tabItem(tabName = "Data_Loader",tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"),
              fluidPage(":)",fluidRow(
                fileInput('TextFile', 'Choose Text file to upload',
                          accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain'
                          )
                ),
                tags$hr(),
                checkboxGroupInput("SelectedModel","Select Model",choices =c("SVM","Regression","RandomForest") ),
                tags$hr(),
                numericInput('tsize','Enter training set percentage value',value=70) 
                )
              )
      ),
      tabItem(tabName = "Result",tags$style(type="text/css",
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }"),
              fluidPage("Results",fluidRow(
                infoBoxOutput("Load_Info"),
                dataTableOutput("hd")
                )
              )
      
      ),
      tabItem(tabName = "SVM",tags$style(type="text/css",
                                            ".shiny-output-error { visibility: hidden; }",
                                            ".shiny-output-error:before { visibility: hidden; }"),
              fluidPage("SVM",fluidRow(
                infoBoxOutput("rmse_Info")
                
              )
              )
      
    )
  )
)
)


options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output) {
  
  data_l<-reactive({
          inFile <- input$TextFile
          if (is.null(inFile))
            return(NULL)
          data<-read.csv(inFile$datapath,header=T)
          data<-data[,-1]
          return(data)
          })
  
  output$Load_Info <- renderInfoBox({
          infoBox("Machine is thinking please Wait :) ", icon = icon("thumbs-up", lib = "glyphicon"),color = "blue")
          })    
  
  output$hd <- renderDataTable(data_l())
  
  output$rmse_Info <- renderInfoBox({
    print("1")
    data<-data_l()
    set.seed(999)
    print("2")
    tsize=round(length(data[,1])*(input$tsize/100))
    print(tsize)
    train<-sample(1:length(data[,1]),tsize,replace = F)
    test<--train
    training_data<-ndat[train,]
    testing_data<-ndat[test,]
    print("3")
    ##======================SIMPLE SVM
    model1.svm <- svm(Dispense ~ ., data = training_data)
    print("4")
    rm<-rmse(testing_data[,7],predict(model1.svm, testing_data[,-7]))
    print("5")
    infoBox("Machine has thought rmse is : ",rm, icon = icon("thumbs-up", lib = "glyphicon"),color = "red")
    print(rm)  
})  
  
}
shinyApp(ui, server)