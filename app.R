#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages<-c("shinydashboard", "shinyjs", "forecast", "tidyr","tidyverse","data.table","shinyLP","forecast","shiny")
check.packages(packages)







library(shiny)
library(shinydashboard)
library(plotly)
library(tidyr)
library(tidyverse)
library(shinyjs)
library(data.table)
library(shinyLP)
library(forecast)


ui <- dashboardPage(skin = "black",
  
  
  dashboardHeader(title = "E-COMMERCE ANALYTICS",titleWidth = '19%'),
  
  dashboardSidebar(width = '250',
    
    fluidPage( 
      sidebarMenu( titlePanel(tags$a(tags$img(src='logo.png',height='40',width='160'))),
                   menuItem("CUSTOMER SEGMENTATION",tabName = "CS",icon = icon("tasks",lib="glyphicon")),
                   menuItem("RECOMMENDER SYSTEMS ",tabName = "Rsystem",icon = icon("thumbs-up",lib="glyphicon")),
                   menuItem("MARKET BASKET ANALYSIS",tabName = "MBA",icon = icon("zoom-in",lib = "glyphicon")),
                   menuItem("RETURNS FORECASTING",tabName = "RP",icon = icon("repeat",lib = "glyphicon"))
                   
                   
      )
      
      
    )
  ) #end of dashboard side bar 
  ,
  
  #dashboard body begins 
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    fluidPage(
      tabItems(
      tabItem(tabName = "CS",
              
              fluidRow( box( title = "DATA OVERVIEW",
                             HTML("<div class='well well-sm'>
                                  <p><h4>Click Here To View the Source</h4> </p> <br>
                                  <p><a class='btn btn-primary btn-lg' button id='tabBut' href='http://archive.ics.uci.edu/ml/datasets/online+retail' target = '_blank'>DATA SOURCE</a></p>
                                  </div>"),
                             uiOutput("infoTT"),
                             uiOutput("Tcust"),
                             uiOutput("Tguest"),
                             uiOutput("Trev"),
                             uiOutput("Tdemograph"),
                             uiOutput("Tsales")
                             ),
                        
                        box(
                          title = "",
                          plotlyOutput("gcount")
                        )
                       ),
              
              fluidRow(
                box( title = "CUSTOMER SEGMENTS",
                     plotlyOutput("CSegments")
                  
                ),
                tabBox(side = "right",
                       tabPanel(title = "MONETARY VALUES",
                                plotlyOutput("Rsegments")
                                ),
                       tabPanel(title = "TABULAR VIEW",
                                tableOutput("SMTable")
                               
                         
                       ),
                       tabPanel(title = "CUSTOMERS LIST",
                                
                                uiOutput("segment_select"),
                                fluidRow(uiOutput("seg_count"),
                                uiOutput("seg_percent")),
                                downloadButton("downloadData", "Download")

                                
                               
                                ),
                       tabPanel(title = "CHURN RATE",
                                HTML("<div class='well well-sm'>
                                  <p><h4>TOTAL NUMBER OF CUSTOMERS : 1139</h4> </p>
                                  <p><h4>TOTAL NUMBER OF LOST CUSTOMERS : 91</h4> </p>
                                   <p><h4><b>CHURN RATE : 7.9% </b></h4> </p>
                                    </div>")
                         
                       )
                       )
                
              ),
              
              fluidRow(
                
                HTML("<div class='jumbotron'>
                                  <p><h3>DISCLAIMER</h3> </p> <br>
                     <p>Data sets are from public data sources .Data sets were used for demonstration purpose only. The accuracy of the result may vary  according to the data</p>
                     </div>")
              )
  
             
                
              
        
      ),
      tabItem(tabName = "Rsystem",
              
              fluidRow( tabBox(side = "right",
                               
                               tabPanel(title = h4("DEMO"),
                                        
                                        uiOutput("Cprod"),
                                        tableOutput("MTable")
                                        
                                 
                               ),
                               tabPanel(title = h4("DATA OVERVIEW"),
                                        
                                        valueBox(value = "206K",subtitle ="USERS",color = "red",icon=icon("user",lib = "glyphicon")),
                                        valueBox(value = "49688",subtitle = "PRODUCTS",color = "green",icon=icon("stats",lib = "glyphicon")),
                                        valueBox(value = "21",subtitle = "DEPARTMENTS",color = "yellow",icon=icon("bookmark",lib = "glyphicon")),
                                        
                                        HTML("<div class='well well-sm'>
                                              <br>
                                             <p><a class='btn btn-primary btn-lg' button id='tabBut' href='https://www.instacart.com/datasets/grocery-shopping-2017' target = '_blank'>DATA SOURCE</a></p>
                                              <br> <br> <p><h3>DISCLAIMER</h3> </p> <br> 
                                             <p>Data sets are from public data sources .Data sets were used for demonstration purpose only. The accuracy of the result may vary  according to the data</p>
                                             </div>"
                                             
                                        )
                                        
                                       
                                        
                                 
                               )
                               ),
                      box( title = h4("DEVELOPMENT STAGES"),
                           HTML("<div class='well well-sm'>
                                  
                                  <ul class='list-group'>
                                  <li class='list-group-item'>1. Data Mining</li>
                                  <li class='list-group-item'>2. Product Segmentation</li>
                                  <li class='list-group-item'>3. Product Purchase Pattern Analysis</li>
                                  <li class='list-group-item'>4. Identifying Training Interval </li>
                                  <li class='list-group-item'>5. Algorithm Deployment and Testing</li>
                                  <li class='list-group-item'><b>6. Preferable Models:</b> Deep learning models or Association Models </li>
                                  <li class='list-group-item'>7. API Deployment For Request and Response of Recommendation </li>
                                  <li class='list-group-item'>8. Testing and Product Roll Out</li>
                                  </ul>
                                  </div>"
                                
                             
                           )
                          
                           
                        
                      )
                  
                
              ),
              
              fluidRow(
                
                box(title = "",width = "100%",
                    tags$head(tags$style(
                      type="text/css",
                      "#image img {max-width: 100%; width: 30%; height: auto}"
                    )),
                    jumbotron(h3("DEPLOYMENT ARCHITECTURE"),content = "",button = FALSE),
                   HTML( "<div class='container'>
                        <img class='img-responsive' src='Presentation-1.jpg' width='95%' height = '50%'>
                          </div>
                         " )
                  
                )
                
              )
              
             
              
              
        
      ),
      tabItem(tabName = "MBA",
              

              
              
              fluidRow( 
                box(title = h3("DEMO"),
                            uiOutput("CMC"),
                            uiOutput("Cqt"),
                            actionButton(inputId = "Mpred",label = "Proceed"),
                            tableOutput("ATable")
                            
                           
                            
                            ),
                box(title = "DATA OVERIVEW",
                    
                    
                    HTML("<div class='well well-sm'>
                          <p><h4>Click Here To View the Source</h4> </p>
                         <p><a class='btn btn-primary btn-lg' button id='tabBut' href='https://www.instacart.com/datasets/grocery-shopping-2017' target = '_blank'>DATA SOURCE</a></p>
                         </div>"
                         
                    ),
                    valueBox(value = "206K",subtitle ="USERS",color = "red",icon=icon("user",lib = "glyphicon")),
                    valueBox(value = "49688",subtitle = "PRODUCTS",color = "green",icon=icon("stats",lib = "glyphicon")),
                    valueBox(value = "21",subtitle = "DEPARTMENTS",color = "yellow",icon=icon("bookmark",lib = "glyphicon"))
                    
                  
                    
                    
                    
                )
                 ),
              
              fluidRow(
                
               tabBox(side = "left",width = '100%',
                      
                      tabPanel(title = "TOP SALES",
                               
                              uiOutput("Cdept"),
                              tableOutput("TopTable")
                        
                      ),
                      tabPanel( title = "DEPARTMENT SALES",
                                
                                plotlyOutput("Deptplot")
                        
                      )
                 
               )
                
              ),
              
              fluidRow(
                
                jumbotron(header = h3("Disclaimer"),content = "Data sets are from public data sources .Data sets were used for demonstration purpose only. The accuracy of the result may vary  according to the data",button = FALSE)
              )
              
              
              
        
      ),
      tabItem(tabName = "RP",
              
             
                fluidRow(  
                valueBox(value = "31707",subtitle = "PRODUCTS",icon=icon("stats",lib = "glyphicon"),color = "blue"),
                valueBox(value = "3.5 M",subtitle = "ITEMS RETURNED",color = "orange",icon= icon("send", lib = "glyphicon")),
                valueBox(value = "131.7 M", subtitle = "RETURN COST",icon= icon("euro", lib = "glyphicon"),color = "olive")
                ),
              fluidRow(
                tabBox(side = "left",
                       
                       tabPanel(title = h4("PREVIOUS RECORDS"),
                                HTML("<div class='well well-sm'>
                                      <p> <b> FIRST TRANSACTION DATE : </b> January 8 2015<br>
                                      <b> LAST TRANSACTION DATE : </b> December 30 2016<br>
                                      <b> TIME WINDOW : </b> 7 Days (Week)<br>
                                      <b> MODEL USED  : </b>Holt Winters<br>
                                     </div>"
                                    ),
                                plotlyOutput("Rhistory"))
                                ,
                      tabPanel(title = h4("ALGORITHMS USED"),
                            HTML("<div class='well well-sm'>"),  
                            tableOutput("Rtable"),
                            HTML("</div>")
                       
                       )),
                
                
                
                tabBox(side = "left",
                       tabPanel(title = h4("FORECAST"),
                                
                                sliderInput(inputId = "rpweeks",label = "CHOOSE NO. OF WEEKS",min = 4, max = 40,value = 4),
                                actionButton(inputId = "rpredict",label = "FORECAST"),
                                plotlyOutput("Rplot")
                         
                       ),
                       tabPanel(title = h4("TREND"),
                                plotlyOutput("RTplot")
                         
                       )
                  
                  
                )
              
               
      
              )
              
            )#end of Returns forecasting
        
      )
      
    )#end of fluid page
  )
  #end of dashboard body

  
  
  
  
  
  ) 


#end of UI , Dashboard Page



server <- function(input, output) {
   

###################------------------------- Customer Segmentation ----------------- ##################################  
df <- fread("Database/Online Retail.csv",sep = ",")
df <- subset(df,select = c(-1,-2,-3))
df<- subset(df,df$Quantity>0)
df <- subset(df , df$UnitPrice>0)
df$Country <- as.factor(df$Country)
df$Net_Price <- df$UnitPrice * df$Quantity
countries <- length(unique(df$Country))
Mcount <- aggregate(df$Net_Price,by= list(Country = df$Country),FUN=sum)
colnames(Mcount) <- c("Country","Monetary_Value")


Vcust <- df[!(is.na(df$CustomerID))]
Ncust <- df[is.na(df$CustomerID)]
Vcust$CustomerID <- as.factor(Vcust$CustomerID)
rm(df)
#------- Pre- processing data -------------------------# 

observe({
  
total_transc <- length(Vcust$Quantity) + length(Ncust$Quantity)
Customers <- length(unique(Vcust$CustomerID)) 
Guests <- length(unique(Ncust$InvoiceDate))
Revenue <- paste(round((sum(Vcust$Net_Price) + sum(Ncust$Net_Price))/1000) ,"k")
sales <- paste(round((sum(Vcust$Quantity)+sum(Ncust$Quantity))/1000),"k")

output$infoTT <- renderUI(valueBox(value = total_transc,subtitle = "TRANSACTIONS",icon = icon("credit-card"),color = "blue"))
output$Tcust <- renderUI(valueBox(value = Customers,subtitle = "CUSTOMERS",icon= icon("user", lib = "glyphicon"),color = "teal"))
output$Tguest <- renderUI(valueBox(value = Guests,subtitle = "GUEST USERS",icon= icon("user", lib = "glyphicon"),color = "light-blue"))
output$Trev <- renderUI(valueBox(value = Revenue,subtitle = "REVENUE",icon= icon("euro", lib = "glyphicon"),color = "olive"))
output$Tdemograph <- renderUI(valueBox(value = countries,subtitle = "COUNTRIES",icon = icon("send",lib = "glyphicon"),color = "green"))
output$Tsales <- renderUI(valueBox(value = sales,subtitle = "SALES",icon=icon("stats",lib = "glyphicon"),color = "blue"))
})

observe({
  
  
  output$gcount <- renderPlotly({
    
    Mcount <- subset(Mcount,Mcount$Country != "United Kingdom")
    plot_ly(data = Mcount, labels= ~Country,values = ~Monetary_Value,type = "pie",hole = 0.6, marker = list( line = list(color = '#FFFFFF', width = 1)),textposition = 'inside', insidetextfont = list(color = '#FFFFFF'))%>%
      layout(title = "GLOBAL REVENUE DISTRIBUTION",  showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
 
})

#---------------- END OF EDA VISUALISATION FOR UK RETAILER DATASET-------------------------#

#---------------- RFM IMPLEMENTATION --------------------------------------------------------#
observe(
  {
   # LOADING PRE_ PROCESSED DATA AND MODEL
   RFM_Tab <- read.csv2("Database/Customer Segementation.csv",sep = ",")
   RFM_Tab$Segments <- as.factor(RFM_Tab$Segments) 
  
  output$CSegments <- renderPlotly({
    d <- data.frame(table(RFM_Tab$Segments))
     colnames(d)<- c("SEGMENT","COUNT")
    plot_ly(data = d,x = ~SEGMENT,y= ~COUNT,type = "bar",color = ~SEGMENT)%>% layout(title = "",
                                                                                     xaxis = list(title = "",showticklabels = FALSE),
                                                                                     yaxis = list(title = ""),
                                                                                     paper_bgcolor = 'rgba(245, 246, 249, 1)',
                                                                                     plot_bgcolor = 'rgba(300, 246, 249, 1)',
                                                                                     showlegend = TRUE)
  })
  RFM_Tab$Monetary.Value <- as.numeric(RFM_Tab$Monetary.Value)
  rd <- aggregate(RFM_Tab$Monetary.Value,by=list(SEGMENT = RFM_Tab$Segments),FUN=sum)
  colnames(rd) <- c("SEGMENT","Monetary.Value")
  
  output$Rsegments <- renderPlotly({
    plot_ly(data = rd, labels= ~SEGMENT,values = ~Monetary.Value,type = "pie",hole = 0.6, marker = list( line = list(color = '#FFFFFF', width = 1)),textposition = 'inside', insidetextfont = list(color = '#FFFFFF'))%>%
      layout(title = "",  showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$SMTable <- renderTable(rd,spacing = c("s"),striped = TRUE,hover = TRUE)
  
  output$segment_select <- renderUI(selectInput(inputId = "segselect",label = "Choose Segment",choices = unique(RFM_Tab$Segments)))
  observeEvent(input$segselect,{
      
      data <- subset(RFM_Tab, RFM_Tab$Segments == input$segselect,select = c(2))
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(input$segselect, ".csv", sep = "")
        },
        content = function(file) {
         write.csv(x = data,file,row.names = FALSE)
        }
      )
     output$seg_count <- renderUI(valueBox(length(data$Cust_id),subtitle = input$segselect,icon = icon("user", lib = "glyphicon"),color = "blue"))
     output$seg_percent <- renderUI(valueBox(paste(round((length(data$Cust_id)/length(RFM_Tab$Cust_id))*100),"%"),subtitle =input$segselect,icon = icon("user", lib = "glyphicon"),color = "light-blue" ))
    })
  
  
  }
)


     ###################---------------- End of Customer Segmentation ----------------- ##################################

    


     #################------------------- Returns forecasting ------------------------####################################

v <- reactiveValues(doplot = FALSE)
rdf <- fread("Database/Returns.csv")

observeEvent(input$rpredict,{
  
  v$doPlot <- input$rpredict
})

observeEvent(input$RP,{
  v$doPlot <- FALSE
})

output$Rplot<-  renderPlotly(
  {
    if (v$doPlot == FALSE) return()
    
    
    isolate({
      
      tSeries <- ts(data = rdf$Return_Qty,frequency = 52,start = c(2015,1))
      
      HW <- HoltWinters(tSeries)
      f <- data.frame(forecast(object = HW,h= as.integer(input$rpweeks),level = 95))
      fval <- round(f$Point.Forecast)
      dval <- c()
      sdate <- as.Date(rdf$Date[104])
      Hvalue <- as.integer(input$rpweeks)
      for(i in 1:Hvalue)
      {
        dval <- append((sdate + (i * 7)),dval)
      }
      dval <- rev(dval)
      
      p <- plot_ly(x = dval, y = fval,type = 'scatter',mode='lines+markers',name = 'forecast',color = 'orange')%>%layout(title = paste("Forecast till the week",input$rpweeks,": 2017"),
                                                                                                                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                                                                                                                      xaxis = list(title ="DATE",
                                                                                                                                   gridcolor = 'rgb(255,255,255)',
                                                                                                                                   showgrid = TRUE,
                                                                                                                                   showline = FALSE,
                                                                                                                                   showticklabels = TRUE,
                                                                                                                                   tickcolor = 'rgb(127,127,127)',
                                                                                                                                   ticks = 'outside',
                                                                                                                                   zeroline = FALSE),
                                                                                                                      yaxis = list(title = "RETURN_QTY",
                                                                                                                                   gridcolor = 'rgb(255,255,255)',
                                                                                                                                   showgrid = TRUE,
                                                                                                                                   showline = FALSE,
                                                                                                                                   showticklabels = TRUE,
                                                                                                                                   tickcolor = 'rgb(127,127,127)',
                                                                                                                                   ticks = 'outside',
                                                                                                                                   zeroline = FALSE))
      
      
    })
    
  })

observe({
  
  MODELS <- c("HOLT WINTERS","MOVING AVERAGE (3 weeks)","WEIGHTED MOVING AVERAGE (3 weeks)","ARIMA")
  MAPE <- c("9.37%","10.98%","22.03%","25.8%")
  output$Rtable <- renderTable(data.frame(MODELS,MAPE),striped = TRUE,hover = TRUE)
  output$Rhistory <- renderPlotly({
    plot_ly(x = as.Date(rdf$Date),y=rdf$Return_Qty,type = 'scatter',mode='lines',name = 'Previous Records')%>%layout(title = paste("RETURNS PER WEEK FROM 2015:2016"),
                                                                                                                             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                                                                                                                             xaxis = list(title ="DATE",
                                                                                                                                          gridcolor = 'rgb(255,255,255)',
                                                                                                                                          showgrid = TRUE,
                                                                                                                                          showline = FALSE,
                                                                                                                                          showticklabels = TRUE,
                                                                                                                                          tickcolor = 'rgb(127,127,127)',
                                                                                                                                          ticks = 'outside',
                                                                                                                                          zeroline = FALSE),
                                                                                                                             yaxis = list(title = "RETURN_QTY",
                                                                                                                                          gridcolor = 'rgb(255,255,255)',
                                                                                                                                          showgrid = TRUE,
                                                                                                                                          showline = FALSE,
                                                                                                                                          showticklabels = TRUE,
                                                                                                                                          tickcolor = 'rgb(127,127,127)',
                                                                                                                                          ticks = 'outside',
                                                                                                                                          zeroline = FALSE))
  })
  
  output$RTplot <- renderPlotly({
  if (v$doPlot == FALSE) return()
  
  
  isolate({
    
    t2Series <- ts(data = rdf$Return_Qty,frequency = 52,start = c(2015,1))
    
    HW2 <- HoltWinters(t2Series)
    f2 <- data.frame(forecast(object = HW2,h= as.integer(input$rpweeks),level = 95))
    Hvalue2 <- as.integer(input$rpweeks)
    
    #----------------------------------------------
    app_val <- numeric()
    for(i in 1:104){
      app_val <- append(app_val,i)
    }
    app_val[app_val%%110 != 0] <- NA
    fval2 <- app_val
    #----------------------------------------------
    fval2 <- append(round(f2$Point.Forecast),fval2)
    fval2 <- rev(fval2)
    dval2 <- c()
    sdate2 <- as.Date(rdf$Date[104])
    
    #---------------------------------------------- 
    app_val <- numeric()
    for(i in 1:Hvalue2){
      app_val <- append(app_val,i)
    }
    app_val[app_val%%110 != 0] <- NA
    
    #----------------------------------------------
    
    for(i in 1:Hvalue2)
    {
      dval2 <- append((sdate2 + (i * 7)),dval2)
    }
    dval2 <- rev(dval2)
    ds <- seq.Date(from = as.Date(rdf$Date[1]),to = dval2[length(dval2)],by = "week")
    p <- plot_ly(x = ds, y = rev(append(app_val,rdf$Return_Qty)),type = 'scatter',mode='lines',name = 'Previous Records',showlegend = FALSE)%>%add_trace(y = fval2,type = 'scatter', mode = 'lines',name='forecast')%>%layout(title = paste("Forecast till the week",input$rpweeks,": 2017"),
                                                                                                                       paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                                                                                                                       xaxis = list(title ="DATE",
                                                                                                                                    gridcolor = 'rgb(255,255,255)',
                                                                                                                                    showgrid = TRUE,
                                                                                                                                    showline = FALSE,
                                                                                                                                    showticklabels = TRUE,
                                                                                                                                    tickcolor = 'rgb(127,127,127)',
                                                                                                                                    ticks = 'outside',
                                                                                                                                    zeroline = FALSE),
                                                                                                                       yaxis = list(title = "RETURN_QTY",
                                                                                                                                    gridcolor = 'rgb(255,255,255)',
                                                                                                                                    showgrid = TRUE,
                                                                                                                                    showline = FALSE,
                                                                                                                                    showticklabels = TRUE,
                                                                                                                                    tickcolor = 'rgb(127,127,127)',
                                                                                                                                    ticks = 'outside',
                                                                                                                                    zeroline = FALSE))
    
    
  })
  })
  
})

##--------------------------------------  END OF RETURNS FORECASTING ----------------------------------------- ##

##-------------------------------------- MARKET BASKET ANALYSIS ----------------------------------------------  ##


#------------------------------

dept <- fread("Database/Dept_sale.csv")
titems <- fread("Database/top_dept.csv")
rules <- fread("Database/MBA.csv")
Prepo <- fread("Database/Product.csv")

#------------------------------


output$CMC <- renderUI(selectInput(inputId = "mc",label = "Choose Product",choices = rules$pname))
output$Cqt <- renderUI(selectInput(inputId = "Mqty",label = "Choose Quantity",choices = c(1,2,3,4)))
#______________________________________
k <- reactiveValues(dplot = FALSE)
observeEvent(input$Mpred,{
  
  k$dplot <- input$Mpred
  
})

observeEvent(input$MBA,{
  
  k$dplot <- FALSE
})

output$ATable <- renderTable({
  
  if (k$dplot == FALSE) return()
  
  
  isolate({
    
    recom <- rules[rules$pname == input$mc]
    #print(head(recom,5))
    recom <- recom[order(-recom$support,-recom$confidence),]
    pid <- c()
    for(i in 1:3)
    {
      pid <- append(unlist(unique(strsplit(recom$lhs[i],split = ","))),pid)
    }
    
    pid <- unique(pid)
    pid <- as.integer(pid)
    
    #getting the products
    final <- c()
    for(i in pid)
    {
      final <- rbind(subset(Prepo,Prepo$Product_id == i),final)
    }
    
    out <- final
    
  })
  
},striped = TRUE, hover = TRUE, na = "")

#--------------------------------
output$Deptplot <- renderPlotly({
  
  colnames(dept) <- c("Department","Total_Sales")
   
  plot_ly(data = dept,x = ~Department,y= ~Total_Sales,type = "bar",color = ~Department)%>% layout(title = "",
                                                                                               xaxis = list(title = "",showticklabels = FALSE),
                                                                                               yaxis = list(title = ""),
                                                                                               paper_bgcolor = 'rgba(245, 246, 249, 1)',
                                                                                               plot_bgcolor = 'rgba(300, 246, 249, 1)',
                                                                                               showlegend = TRUE)
  
})

output$Cdept <- renderUI(selectInput("mitem",label = "Department",choices = unique(titems$Department)))
observeEvent(input$mitem,{
  
  temp <- subset(titems,titems$Department == input$mitem,select = c(1,2))
  output$TopTable <- renderTable(temp,striped = TRUE,hover = TRUE)
  
})



##-------------------------------------- END OF MARKET BASKET ANALYSIS ----------------------------------------------  ##

## ------------------------------------- RECOMMENDER SYSTEMS --------------------------------------------------------  ##
alc <- fread("Database/AlcholBasket.csv")
alc$support <- gsub("\\,",".",alc$support)
alc$confidence <- gsub("\\,",".",alc$confidence)
alc$lift <- gsub("\\,",".",alc$lift)
alc$support <- as.numeric(alc$support)
alc$confidence <- as.numeric(alc$confidence)
alc$lift<- as.numeric(alc$lift)

output$Cprod <- renderUI(selectInput(inputId = "Mchoose",label = "Choose Products",choices = unique(alc$pname)))

observeEvent(input$Mchoose,{
  
  
  output$MTable <- renderTable({
    

    recom <- alc[alc$pname== input$Mchoose]
    #print(head(recom,5))
    recom <- head(recom[order(-recom$support,-recom$confidence),],5)
    
    pid <- c()
    for(i in 1:5)
    {
      pid <- append(unlist(unique(strsplit(recom$lhs[i],split = ","))),pid)
    }
    
    pid <- unique(pid)
    pid <- as.integer(pid)
    
    #getting the products
    final <- c()
    for(i in pid)
    {
      final <- rbind(subset(Prepo,Prepo$Product_id == i),final)
    }
    
    out <- final
    
    
  },na = "",striped = TRUE)
  
  
})

## ------------------------------------- RECOMMENDER SYSTEMS --------------------------------------------------------  ##

}


shinyApp(ui = ui, server = server)

