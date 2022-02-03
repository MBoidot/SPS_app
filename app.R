library(shiny)
library(xlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(zoo)
library(lubridate)
library(plotly)
library(shinyFiles)


theme_set(theme_bw(10))
theme_update(panel.grid.major=element_line(colour="#b2b2b2", size=0.5),
             panel.grid.minor=element_line(colour="#c5c5c5", size=0.5),
             legend.title=element_text(size=18),
             axis.title.x=element_text(size=20),
             axis.title.y=element_text(size=20,angle=90,vjust=1.5),
             axis.text.x=element_text(size=16),
             axis.text.y=element_text(size=16),
             legend.text=element_text(size=16),
             plot.title=element_text(size=25, face="bold",vjust=0.5),
             strip.text.x=element_text(size=14,face="bold"),
             strip.text.y=element_text(size=14,face="bold"))



ui <- fluidPage(
  
  pageWithSidebar(
    headerPanel("SPS file treatment"),
    
    
    sidebarPanel(width = 2,
                 fileInput("file_in","Data file", multiple = FALSE),
                 fileInput("blanc_in","Blanc file", multiple = FALSE),
                 actionButton("update_wt", "Update Windowtest plot"),
                 br(),
                 br(),
                 selectInput("sps_type", "Choose SPS device:", choices =  list(FCT = c(`FCT HPD 25` = 'HPD25', `FCT HPD 5` = 'HPD5'),
                                                                               Sumitomo = c(`Dr Sinter 2080` = 'DS2080')))
                 
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 fluidRow(
                   
                   column(12,
                          h3("Material inputs"),
                          column(3,
                                 numericInput("dmes", "Measured density (g/cm3)", value = 1,width = '100%'),
                                 numericInput("dth", "Theoretical density (g/cm3)", value = 1)),
                          column(3,numericInput("Dech", "Diameter (mm)", 20, min = 10, max = 80),
                                 numericInput("mpoudre", "Powder mass (g)", value = 1))
                   ),
                   
                   column(12,
                          column(8,
                                 hr(),
                                 h2("Recorded temperature"),
                                 plotlyOutput("avtemp_plot"),
                                 hr(),
                                 h2("Blanc relative displacement and model"),
                                 plotOutput("blancdisp_plot"),
                                 
                                 
                                 column(12, 
                                        hr(),
                                        h2("Densification rate"),
                                        plotOutput("window_plot"),
                                        hr(),
                                        column(2,numericInput("sample_rate", "Sampling rate", value = 10)),
                                        column(2,sliderInput("smooth", "Smoothing", min=0, max=1, value=0.2)),
                                        column(5,
                                               sliderInput("Trangeinput", "Temperature range", min = 0, max = 2500, value = c(750, 1134), width = '100%')),
                                        br(),
                                        br(),
                                        column(2,actionButton("update_wt2", "Update Windowtest plot"))),
                                 
                                 column(12,
                                        hr(),
                                        h3("Output Options"),
                                        
                                        column(3,sliderInput("Down_width", "Width (px)", min = 800, max = 4096, value = 2000)),
                                        column(3,sliderInput("Down_height", "Height (px)", min = 600, max = 4096, value = 1000)),
                                        column(2,sliderInput("point_size", "Point size", min=0, max=6, value=0.25, step = 0.05)),
                                        column(2,sliderInput("point_alpha", "Alpha", min=0.25, max=1, value=1)),
                                        br(),
                                        br(),
                                        column(2,downloadButton("dwnld_window",label = "Get plot"))),
                                 
                                 hr(),
                                 
                                 column(12,h2("Max sintering rate temperature"),dataTableOutput("max_temp_plot")),
                                 
                                 hr(),
                                 column(12,h2("Density evolution"),plotOutput("density_plot"))),
                          column(4,
                                 hr(),
                                 
                                 verbatimTextOutput("brush"))
                          
                   )
                 )
                 
        ),
        tabPanel("Data", dataTableOutput("reduced_data_table")),
        tabPanel("Data blanc", dataTableOutput("reduced_data_table_blanc")),
        tabPanel("Sampled data",dataTableOutput("window_table"),downloadButton("downloadwindowtable", "Download")),
        
        tabPanel("Batch treatment",
                 fluidRow(
                   
                   column(12,
                          h3("Densification rate plot comparison"),
                          # shinyFilesButton("dens_rate_in", "Choose a file" ,
                          # title = "Please select a file:", multiple = TRUE,
                          # buttonType = "default", class = NULL),
                          
                          #fileInput("dens_rate_in","Data file", multiple = TRUE),
                          # 
                          fileInput("dens_rate_in","Data files", multiple = TRUE,accept = ".csv"),
                          plotOutput("batch_plot"),
                          textOutput("txt_file"),
                          dataTableOutput("batch_table")
                   )
                 )
        ),
        tabPanel("Help",htmlOutput("helptxt")),
                 tabPanel("Releases",htmlOutput("version_text")
                          
                 )
        )
      )
    )
  )

  
  
  server = function(input, output,session){
    
    
    
    #create the dynamic dataset according to the chosen file
    dataset <-reactive({ 
      inFile <- input$file_in
      sps_type <- input$sps_type
      diam_ech <- input$Dech
      if (is.null(inFile)) {
        return(NULL)
      }
      
      #Data importing
      if (sps_type=="HPD5"){
        data <-  read.csv2(inFile$datapath,header=TRUE, sep=";",fileEncoding="latin1")
        
        #Remove first line (units)
        data <- data[-1,]
        
        #Keep useful columns
        data <- data[,c(1,5,6,12,14,16,17)]
        data[] <- sapply(data, gsub, pattern = ",", replacement= ".")
        data[] <- sapply(data, as.numeric)
        
        data$pression <- data$AV.Force/(pi*(diam_ech/(10*2))^2)
        data$reldisp <- data$AV.Abs..Piston.T-data$AV.Abs..Piston.T[1]
        
        
      } else if (sps_type=="HPD25"){
        data <-  read.csv2(inFile$datapath,header=TRUE, sep=",",fileEncoding="latin1")
        
        #Remove first line (units)
        data <- data[-1,]
        
        #Keep useful columns
        data <- data[,c(1,5,6,7,10,12,17,18)]
        data[] <- sapply(data, gsub, pattern = ",", replacement= ".")
        data[] <- sapply(data, as.numeric)
        
        data$pression <- data$AV.Force/(pi*(diam_ech/(10*2))^2)
        data$reldisp <- data$AV.Abs..Piston.T-data$AV.Abs..Piston.T[1]
        
      } else {
        #Last machine type up to 2022 02 03 : Dr sinter 2080
        
        data <- read_excel(inFile$datapath, 1)
        
        if (data[1,1] == data[2,1]){
          time_interval <- 0.5
        } else{
          time_interval <- 1
        }
        
        data$No. <- seq(0,nrow(data)-1)*time_interval
        
        data[,c(1,2,3,4,8,9)] <- NULL
        
        names(data) <- c("AV.Pyrometer", "pression", "AV.Abs..Piston.T","No.")
        
      }
      return(data)
    })  
    
    #same process on the blank file
    datablc<-reactive({
      
      blcFile <- input$blanc_in
      sps_type <- input$sps_type
      
      if (is.null(blcFile)) {
        return(NULL)
      }
      
      if (sps_type=="HPD5"){
        data2 <-  read.csv2(blcFile$datapath,header=TRUE, sep=";",fileEncoding="latin1")
        
        #Remove first line (units)
        data2 <- data2[-1,]
        
        #Keep useful columns
        data2 <- data2[,c(1,5,6,12,14,16,17)]
        data2[] <- sapply(data2, gsub, pattern = ",", replacement= ".")
        data2[] <- sapply(data2, as.numeric)
        
        data2$reldisp <- as.numeric(data2$AV.Abs..Piston.T-data2$AV.Abs..Piston.T[1])
        
      } else if (sps_type=="HPD25"){
        data2 <-  read.csv2(blcFile$datapath,header=TRUE, sep=",",fileEncoding="latin1")
        
        #Remove first line (units)
        data2 <- data2[-1,]
        
        #Keep useful columns
        data2 <- data2[,c(1,5,6,7,10,12,17,18)]
        data2[] <- sapply(data2, gsub, pattern = ",", replacement= ".")
        data2[] <- sapply(data2, as.numeric)
        
        data2$reldisp <- as.numeric(data2$AV.Abs..Piston.T-data2$AV.Abs..Piston.T[1])
        
      } else {
        #Dr Sinter 2080
        
        data2 <- read_excel(blcFile$datapath, 1)
        
        if (data2[1,1] == data2[2,1]){
          time_interval <- 0.5
        } else{
          time_interval <- 1
        }
        
        data2$No. <- seq(0,nrow(data2)-1)*time_interval
        
        data2[,c(1,2,3,4,8,9)] <- NULL
        
        names(data2) <- c("AV.Pyrometer", "pression", "AV.Abs..Piston.T","No.")
        data2$reldisp <- as.numeric(data2$AV.Abs..Piston.T-data2$AV.Abs..Piston.T[1])
      }
      
      return(data2)
      
    })
    
    
    window_data <- eventReactive(input$update_wt2 | input$update_wt, {
      
      #import all inputs
      sps_type <- input$sps_type
      dmes <- input$dmes
      dth <- input$dth
      Dech <- input$Dech
      mpoudre <- input$mpoudre
      tmin <- input$Trangeinput[1]
      tmax <- input$Trangeinput[2]
      
      
      data <- dataset()
      data2 <- datablc()
      
      #__________________________________________________________
      #Subset data and data2 in the chosen Temperature range
      #valeurs inf at Tmin
      data <- data[data$AV.Pyrometer>tmin,]
      
      #Other values
      #conditional for identifying if the recording was stopped before cooling or not
      #fisrt case end of file corresponds to cooling
      if (data$AV.Pyrometer[length(data$AV.Pyrometer)-1] > data$AV.Pyrometer[length(data$AV.Pyrometer)])  {
        
        #Identify starting time of cooling
        timemaxt <- min(data[data$AV.Pyrometer == max(data$AV.Pyrometer),"No."])
        #Subset from this time
        data <- data[data$No.<timemaxt,]
        #final subset with respect to Tmax
        data <- data[data$AV.Pyrometer<tmax,]
        
      } else {
        data <- data[data$AV.Pyrometer<tmax,]
      }
      
      
      #Remove everything before tmin
      data2 <- data2[data2$AV.Pyrometer>tmin,]
      

      #Other values
      #conditional for identifying if the recording was stopped before cooling or not
      #first case : end of file corresponds to cooling
      
      if (data2$AV.Pyrometer[length(data2$AV.Pyrometer)-1] > data2$AV.Pyrometer[length(data2$AV.Pyrometer)])  {
        
        #Identify starting time of cooling
        timemaxt <- min(data2[data2$AV.Pyrometer == max(data2$AV.Pyrometer),"No."])
        #Subset from this time
        data2 <- data2[data2$No.<timemaxt,]
        #final subset with respect to Tmax
        data2 <- data2[data2$AV.Pyrometer<tmax,]
        
      } else {
        data2 <- data2[data2$AV.Pyrometer<tmax,]
      }
      
      
      data$reldisp <- as.numeric(data$AV.Abs..Piston.T-data$AV.Abs..Piston.T[1])
      
      #Blank displacement fitted with 2nd order polynomial function
      pred <- data.frame(AV.Pyrometer = data$AV.Pyrometer)
      model_blanc_displacement <- lm(reldisp ~ poly(AV.Pyrometer,2), data=data2)
      
      data$dplblanc <- predict(model_blanc_displacement, pred)
      
      #Modified displacement
      data$dplcorr <- data$reldisp - data$dplblanc
      
      #Final height of the sample
      hfin <- as.numeric(mpoudre/(((pi*(Dech/20)^2))*dmes))
      
      #Height of the powder bed
      data$hlitpoudre <- hfin + as.numeric(data[length(data$No.),"reldisp"]) - data$reldisp
      
      #Density
      data$density <- mpoudre/(((pi*(Dech/20)^2))*data$hlitpoudre)
      
      #Relative density
      data$reldensity <- data$density/dth
      
      #Sampling
      data_ech <- data[data$No. %% input$sample_rate ==0,]
      
      #Derivative
      data_ech$DDDTsurD <- NA
      for(i in 2:(length(data_ech$No.)-1)) data_ech$DDDTsurD[i] <- (1/data_ech$density[i])*((data_ech$density[i+1]-data_ech$density[i-1]))/((data_ech$No.[i+1]-data_ech$No.[i-1]))
      
      return(data_ech)
    })
    
    output$reduced_data_table <- renderDataTable({
      head(dataset(),20)
    })
    
    output$downloadwindowtable <- downloadHandler(
      filename = function() {
        paste('window_table', '.csv', sep=';')
      },
      content = function(file) {
        df1 <- window_data()
        write.csv(df1, file)
      }
    )
    
    output$reduced_data_table_blanc <- renderDataTable({
      head(datablc(),20)
    })
    
    output$window_table <- renderDataTable({
      req(input$blanc_in)
      window_data()
    })
    
    output$avtemp_plot <- renderPlotly({
      data <- dataset()
      req(input$file_in)
      
      p <- ggplot(data, aes(No., AV.Pyrometer,key=No.))
      p <- p + geom_line()
      p <- p + geom_point(size=0.2)
      p <- p + xlab("Time (s)") + ylab("Temperature (degC)")
      p <- p + scale_y_continuous(breaks = seq(0, 2500, 100))
      g <- ggplotly(p) 
      g <-layout(g,dragmode = "select")
      
      print(g)
    })
    
    
    output$brush <- renderPrint({
      d <- event_data("plotly_selected")
      if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
    })
    
    
    output$blancdisp_plot <- renderPlot({
      req(input$blanc_in)
      data <- dataset()
      data2 <- datablc()
      win_data <- window_data()
      
      g <- ggplot(data2, aes(AV.Pyrometer, reldisp))
      g <- g + geom_line()
      g <- g + geom_line(data=win_data,aes(AV.Pyrometer, dplblanc),size=2,color="blue")
      g <- g + xlab("Temperature (degC)") + ylab("Relative blanc displacement (mm)")
      
      print(g)
    })
    
    output$window_plot <- renderPlot({
      req(input$blanc_in)
      win_data <- window_data() 
      g <- ggplot(win_data, aes(AV.Pyrometer, DDDTsurD))
      g <- g + geom_line()
      g <- g + geom_point(size=3,alpha=0.5)
      g <- g + geom_smooth(se = FALSE,span = input$smooth)
      g <- g + xlab("Temperature (degC)") + ylab(expression(1/D. ~ partialdiff ~ D / partialdiff ~t))
      
      print(g)
      
    })
    
    output$density_plot <- renderPlot({
      req(input$blanc_in)
      densityplot_data <- window_data()
      g <- ggplot(densityplot_data, aes(AV.Pyrometer, reldensity))
      g <- g +geom_line()
      g <- g + xlab("Temperature (degC)") + ylab("Relative density (%)")
      
      print(g)
      
    })
    
    output$dwnld_window <- downloadHandler(
      filename = function() { "densification rate.png" },
      content = function(file) {
        
        win_data <- window_data()
        hgt <-  input$Down_height/300
        wdth <- input$Down_width/300
        
        sze <- input$point_size
        apha <- input$point_alpha
        g <- ggplot(win_data, aes(AV.Pyrometer, DDDTsurD))
        g <- g + geom_line()
        g <- g + geom_point(size=sze,alpha=apha)
        g <- g + geom_smooth(se = FALSE,span = input$smooth)
        g <- g + xlab("Temperature (degC)") + ylab(expression(1/D. ~ partialdiff ~ D / partialdiff ~t))
        
        
        ggsave(file, plot = g, device = "png",width=wdth,height = hgt)
      }
    )
    
    batch_file_name <- reactive({
      batchFile <- input$dens_rate_in
      
      if (is.null(batchFile))
        return(NULL)
      
      return (stringi::stri_extract_first(str = batchFile$name, regex = ".*(?=\\.)"))
    })
    
    
    data_batch <- reactive({
      
      batchFile <- input$dens_rate_in
      
      if (is.null(batchFile)) {
        return(NULL)
      } else {
        
        
        numfiles = nrow(batchFile) 
        out.file<-""
        
        batch_file_names <- batch_file_name()
        
        for(i in 1:nrow(batchFile)){
          
          name <- basename(batchFile$datapath[i])
          data_b<-read.csv(batchFile$datapath[i],sep=",",header=TRUE)
          data_b <- data_b[3:nrow(data_b)-1,c(3,length(data_b))]
          data_b$sample <- batch_file_names[i]
          out.file <- rbind(out.file, data_b)
        }
        
        data_b <- out.file
        data_b <- data_b[2:nrow(data_b),]
        
        data_b$DDDTsurD <- as.numeric(data_b$DDDTsurD)
        data_b$AV.Pyrometer <- as.numeric(data_b$AV.Pyrometer)
        
        return (data_b)
      }
    })
    
    output$batch_file_names <- renderText({
      req(input$dens_rate_in)
      data_batch()
    })
    
    
    output$batch_table <- renderDataTable({
      req(input$dens_rate_in)
      data_batch()
    })
    
    output$batch_plot <- renderPlot({
      req(input$dens_rate_in)
      batchplot_data <- data_batch()
      
      g <- ggplot(batchplot_data, aes(AV.Pyrometer, DDDTsurD, group=sample, color=sample))
      g <- g + geom_line(size=1)
      g <- g + geom_point(size=3)
      g <- g + xlab("Temperature (degC)") + ylab("1/D.dD/dt")
      print(g)
      
    })
    
    
    htext <- div(
      h1("Help text to be inserted here"),
      br(),
      "boutons de telechargement des graphes individuels et des tables de donnees -> OK pour le densification rate",
      br(),
      "integration d'un graphe de resume complet des donnees avec facet grid et gestion couleurs alpha etc.",
      br(),
      "Implementation de deux sliders pour les intervalles de temperatures. Un pour le modele de deplacement sur le blanc, un pour le graphe de deplacement"
    )
    
    output$helptxt <- renderUI(htext)
    
    vtext <- div(
      h3("Help text to be inserted here"),
      br(),
      "V1.0.0 - First release of the application",
    )
    output$version_text <- renderUI(vtext)
    
    
  }
  
  shinyApp(ui = ui, server = server)
