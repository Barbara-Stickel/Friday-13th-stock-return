
install.packages("xlsx")
install.packages("MASS")
install.packages("shiny")
install.packages("stringr")
library(shiny)
library(xlsx)
library(MASS)
library(stringr)

rm(list=ls())

ui<-navbarPage(
  title = "Introduction to probability and statistics project",
  
  #First Panel with one stock symbol
  tabPanel("One stock symbol", 
           
           #First panel with the histogram and the normal probability plot
           titlePanel("One stock symbol"),
           
           sidebarLayout(
             
             sidebarPanel(
               # Choice of the data
               selectInput(inputId = "choiceData", label = "Which data do you want to work with?", choices = c("Alexion Pharmaceuticals"="ALXN", "ArQule, Inc"="ARQL", "BioSpecifics Technologuies Corp"="BSTC"), selected = "ALXN")
             ),
             
             mainPanel(
               tabsetPanel(
                 # tabPanel with the histogram of the data
                 tabPanel("Plot", 
                          h3("Histogram of the data"),
                          wellPanel(
                            fluidRow(
                              column(9, textInput(inputId = "TitleHist", label = "Choose a title", value = "Histogram of the data")),
                              column(3, actionButton(inputId = "UpdateTitleHist", label = "Update"))
                            ),
                            fluidRow(
                              column(12, sliderInput(inputId = "BreaksHist", label = "Choose the number of breaks", value = 25, min = 1, max = 30))
                            ),
                            fluidRow(column(12, checkboxInput(inputId = "curvee", label = "Do you want to display a normal probability plot?", FALSE)))
                          ),
                          fluidRow(plotOutput(outputId = "hist"))
                 ),
                 # tabPanel with the confidence intervals
                 tabPanel("Confidence Intervals",
                          h3("Confidence Interval"),
                          wellPanel(sliderInput(inputId = "AlphaValue", label = "Choose the value of alpha", value = 0.05, min = 0, max = 1)),
                          fluidRow(column(12, textOutput(outputId = "CImu"))),
                          fluidRow(column(12, textOutput(outputId = "CIst")))
                 ),
                 
                 # tabPanel with the regression
                 tabPanel("Regression",
                          h3("Regression of the log return on time"),
                          plotOutput(outputId = "RegLogRe"),
                          plotOutput(outputId = "RegResidual"),
                          verbatimTextOutput(outputId = "summary")
                 )
               )
             )
           )
           
           
           
  ),
  
  # Second Panel with two stock symbols
  tabPanel("Two stock symbols",
           titlePanel("Two stock symbols"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "choiceData1", label = "Which data do you want to work with?", choices = c("Alexion Pharmaceuticals"="ALXN", "ArQule, Inc"="ARQL", "BioSpecifics Technologuies Corp"="BSTC"), selected = "ALXN"),
               selectInput(inputId = "choiceData2", label = "", choices = c("Alexion Pharmaceuticals"="ALXN", "ArQule, Inc"="ARQL", "BioSpecifics Technologuies Corp"="BSTC"), selected = "ARQL")
             ),
             mainPanel(
               fluidRow(
                 h3("Regression between the two symbols"),
                 plotOutput("Reg2"),
                 textOutput(outputId = "equalityMean"),
                 plotOutput(outputId = "Reg2Residual"),
                 verbatimTextOutput(outputId = "Re2Summary")
               )
             )
           )
           
           
  ),
  
  # Third Panel with our work on Black Friday
  
  tabPanel("Friday Thriteen",
           titlePanel("Friday Thirteen"),
           wellPanel(
             helpText("Since Friday 13th are known to be unlucky days, we compare the stock market on a normal Friday and on a Friday 13th. Therefore, if the mean of the stock market is different on Friday 13th and on normal Friday, it means there is probably an influence from that 'unlucky' day."),
             helpText("In order to make the comparison, we have divided the data of log-returns (log(close/open)) into black Fridays (Black day and 13rd of the month) and normal Fridays, then tested if their means are equal."),
             selectInput(inputId = "choiceData3", label = "Which data do you want to work with?", choices = c("Alexion Pharmaceuticals"="1", "ArQule, Inc"="2", "BioSpecifics Technologuies Corp"="3"), selected = "1"),
             textOutput(outputId = "BBFF"),
             plotOutput(outputId = "BFhistogram")
           )
           
  )
  
  
)

server<-function(input, output){
  
  # the data set (the data set is reduced compared with the one use in the project, only in order to increase the reactivity of the app).
  Data<- read.xlsx("openclose2.xlsx",1)
  attach(Data)
  head(Data)
  
  Data$date <- as.Date(Data$date, "%m/%d/%Y")
  day <- weekdays(as.Date(Data$date))
  date<-Data$date
  
  # Choice of the data
  output$text = renderText(input$choiceData)
  
  X<-reactive({X<-Data[,input$choiceData]})
  rv<-reactiveValues()
  
  # Variables
  n<-reactive({n<-length(X())})
  N<-reactive({N<-n()-1})
  
  Mean <- reactive({Mean <- mean(X())})
  Sd <- reactive({Sd <- sd(X())})
  SE <- reactive({SE <- Sd()/sqrt(n())})
  
  # One stock symbol
  # Task 1 (Display histograms for your data by stock symbol)
  
  # Title of the histogram
  TiHist <- eventReactive(input$UpdateTitleHist, {input$TitleHist})
  
  
  # Histogram
  output$hist= renderPlot({
    
    Re<-matrix(0, N(), 1)
    for(i in 1:N()){
      Re[i]=log(X()[[i]]/X()[[i+1]])
    }
    
    rv$meanRe=mean(Re)
    rv$sdRe=sd(Re)
    
    # Task 2 (Display a normal probability plot to see if the data is approximately normal)
    
    if(input$curvee){
      hist(Re, freq=FALSE, breaks=input$BreaksHist, main = TiHist())
      curve(dnorm(x, rv$meanRe, rv$sdRe), add=TRUE, lwd=2) 
    }
    else{
      hist(Re, freq = FALSE, breaks=input$BreaksHist, main = TiHist())
    }
  })
  
  # Task 3 (Create (approximate) confidence intervals for the means and variances given a confidence level)
  
  # Confidence Interval for the mean
  output$CImu <-renderText({
    
    alpha=input$AlphaValue # change value of alpha
    rv$E = qt(1-alpha*0.5, N()*SE())
    rv$CImu = c(Mean(),Mean()) + c(-rv$E, rv$E)
    paste("The confidence interval for the mean is [", str_sub(rv$CImu[1],1,7), ";",str_sub(rv$CImu[2],1,7),"]")
  })
  
  # Confidence interval for the standard deviation
  output$CIst <- renderText({
    alpha=input$AlphaValue
    rv$lower=sqrt(N()*Sd()^2/qnorm(1-alpha/2,N()))
    rv$upper=sqrt(N()*Sd()^2/qnorm(alpha/2,N()))
    paste("The confidence interval for the standard deviation is [", str_sub(rv$lower,1,7), ";", str_sub(rv$upper,1,7),"]" )
  })
  
  # Taks 4 (Perform a regression of the log-return on time.)
  
  # Regression on time
  output$RegLogRe <- renderPlot({
    
    Re<- matrix(0, N(), 1)
    time<-1:N()
    for(i in 1:N()){
      Re[i]=log(X()[[i]]/X()[[i+1]])
    }
    
    #plot
    plot(time,Re)
    abline(lm(Re ~ time))
    
  })
  
  # Graph of residuals
  output$RegResidual <- renderPlot({
    
    Re<- matrix(0, N(), 1)
    time<-1:N()
    for(i in 1:N()){
      Re[i]=log(X()[[i]]/X()[[i+1]])
    }
    rv$ReResidual <- lm(Re ~ time)
    
    res = resid(rv$ReResidual)
    plot(time, res, ylab="Residuals", xlab="Time",  main="Residual against the observed values") 
    abline(0, 0)
  })
  
  # Summary of the regression
  output$summary <-renderPrint({
    summary(rv$ReResidual)
  })
  
  #Two stock symbols
  
  Y<-reactive({Y<-Data[,input$choiceData1]})
  Z<-reactive({Z<-Data[,input$choiceData2]})
  
  k <- reactive({ k<-length(Y())})
  l <- reactive({ l<-length(Z())})
  
  K <-reactive({ K<-k()-1})
  L <-reactive({ L<-l()-1})
  
  
  # Task 1 
  output$equalityMean <-renderText({
    Re1<- matrix(0, K(), 1)
    for(i in 1:K()){
      Re1[i]=log(Y()[[i]]/Y()[[i+1]])
    }
    
    Re2<- matrix(0, L(), 1)
    for(i in 1:L()){
      Re2[i]=log(Z()[[i]]/Z()[[i+1]])
    }
    
    rv$pvalue = t.test(Re1, Re2)[[3]][1]
    if(rv$pvalue>0.05){
      "The mean are equal."
    }
    else{
      "The mean are not equal."
    }
  })
  
  # Task 2  (Perform a regression of one log-return on the other)
  
  # Regression 
  output$Reg2 <- renderPlot({
    
    Re1<- matrix(0, K(), 1)
    for(i in 1:K()){
      Re1[i]=log(Y()[[i]]/Y()[[i+1]])
    }
    
    Re2<- matrix(0, L(), 1)
    for(i in 1:L()){
      Re2[i]=log(Z()[[i]]/Z()[[i+1]])
    }
    rv$lg2 <- lm(Re1 ~ Re2)
    
    #plot of regression
    plot(Re2,Re1)
    abline(lm(Re1 ~ Re2))
  })
  
  # Graph of residuals
  output$Reg2Residual <- renderPlot({
    Re1<- matrix(0, K(), 1)
    for(i in 1:K()){
      Re1[i]=log(Y()[[i]]/Y()[[i+1]])
    }
    
    Re2<- matrix(0, L(), 1)
    for(i in 1:L()){
      Re2[i]=log(Z()[[i]]/Z()[[i+1]])
    }
    
    res2 = resid(lm(Re1 ~ Re2))
    plot(Re2, res2, ylab="Residuals", xlab="Re2",  main="Residual against the observed values") 
    abline(0, 0)
  })
  
  # Summary of the regression
  output$Re2Summary <- renderPrint({
    summary(rv$lg2)
  })
  
  
  
  # Friday 13th 
  
  output$BBFF <- renderText({
    
    # Choice of the data
    rv$i=as.numeric(input$choiceData3)
    rv$open = Data[2*rv$i]
    rv$close = Data[2*rv$i+1]
    
    rv$re<-log(rv$close/rv$open)
    
    rv$mydata<-data.frame(date,day,rv$re)
    attach(rv$mydata)
    head(rv$mydata)
    
    bf=c(NULL,NULL,NULL,NULL,NULL,NULL,NULL)
    nf=c(NULL,NULL,NULL,NULL,NULL,NULL,NULL)
    
    # Separation of the data
    for (i in 1:nrow(rv$mydata)) {
      if((rv$mydata$day[i]=="Friday") & (as.POSIXlt(rv$mydata$date[i])$mday!=13))
      {
        nf=rbind(nf,rv$mydata[i,])
      }
      
    }
    
    for (i in 1:nrow(rv$mydata)) {
      if((rv$mydata$day[i]=="Friday") & (as.POSIXlt(rv$mydata$date[i])$mday==13))
      {
        bf=rbind(bf,rv$mydata[i,])
      }
      
    }
    names(bf)[3]<-paste("re")
    names(nf)[3]<-paste("re")
    
    
    #summary
    
    rv$BF<-bf$re
    meanbf=mean(rv$BF)
    sdbf=sd(rv$BF)
    n=nrow(bf)
    
    rv$NF<-nf$re
    meannf=mean(rv$NF)
    sdnf=sd(rv$NF)
    m=nrow(nf)
    
    # test the equality of the variances
    if(var.test(rv$BF, rv$NF)[3]>0.05){
      
      #test the equality of two means if variance is equal
      if(t.test(rv$BF, rv$NF, alternative="two.sided", var.equal=TRUE)[3]>0.05){
        "The variance and the means are equal"
      }
      else{
        "The variance are equal but not the means."
      }
    }
    
    else{
      
      #test the equality of two means if variance is not equal
      if(t.test(rv$BF,rv$NF)[3]>0.05){
        "The variance are not equal but the means are equal."
      }
      else{
        "The means and the variances are not equal"
      }
    }
  })
  
  output$BFhistogram <- renderPlot({
    
    meanbf=mean(rv$BF)
    sdbf=sd(rv$BF)
    
    meannf=mean(rv$NF)
    sdnf=sd(rv$NF)
    
    x <- seq(-6,6,length=100)*sdnf + meannf
    hx <- dnorm(x,meanbf,sdbf)
    hy <- dnorm(x,meannf,sdnf)
    plot(x,hx,type="l",col="purple",lwd="5")
    lines(x,hy,col="pink",lwd="5")
    
  })
}

shinyApp(ui=ui, server=server)