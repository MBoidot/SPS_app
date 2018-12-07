library(shiny)
library(xlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(zoo)
library(lubridate)


ui <- fluidPage(
    
    pageWithSidebar(
        titlePanel("HPD5 and HPD25 file treatment"),

        sidebarPanel(width = 2,
            fileInput("file_in","Data file", multiple = FALSE),
            fileInput("blanc_in","Blanc file", multiple = FALSE),
            numericInput("sample_rate", h5("Sampling rate"), value = 10),
            actionButton("update_wt", "Update Windowtest plot"),
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
                                          numericInput("dmes", h5("Measured density (g/cm3)"), value = 1,width = '100%')),
                                      column(3,
                                            numericInput("dth", h5("Theoretical density (g/cm3)"), value = 1)),
                                      column(3,
                                             numericInput("Dech", h5("Diameter (mm)"), 20, min = 10, max = 80)),
                                      
                                      column(3,numericInput("mpoudre", h5("Powder mass (g)"), value = 1))),

                             column(12,
                                      sliderInput("Trangeinput", "Temperature range", min = 0, max = 2500, value = c(450, 1500), width = '100%')),
                                      
                               
                                 column(12, h3("Graph settings"),
                                        column(6,
                                               sliderInput("point_size", "Point size", min=0, max=6, value=0.25, step = 0.05),
                                               sliderInput("point_alpha", "Alpha", min=0.25, max=1, value=1)
                                               ),
                                        
                                        column(6,
                                               selectInput("col_theme", "Choose a color palette:", choices =  list(Brewer = c(`Set1` = 'Br_S1', `Set2` = 'Br_S2', `Set3` = 'Br_S3', `Spectral` = 'Br_Spectral'),
                                                                                                                   Viridis = c(`Viridis` = 'Vir_vir',`Plasma` = 'Vir_plas',`Magma` = 'Vir_mag'))))),
                             
                             "I am above graph",
                             
                             plotOutput("reduced_plot"),
                             
                             
                             "I am below graph",
                             fluidRow(
                                 downloadButton("dwnld",label = "Get plot"),
                                 sliderInput("Down_width", "Width (px)", min = 800, max = 4096, value = 1280),
                                 sliderInput("Down_height", "Height (px)", min = 600, max = 4096, value = 800)
                             )
                )),
                tabPanel("Data", dataTableOutput("reduced_data_table")),
                tabPanel("Data blanc", dataTableOutput("reduced_data_table_blanc")),
                tabPanel("Sampled data",dataTableOutput("window_table")),
                tabPanel("Help",htmlOutput("helptxt"))
            )
        )
    )
)


server = function(input, output){

#create the dynamic dataset according to the chosen file
    
dataset <-reactive({ 
    inFile <- input$file_in
    sps_type <- input$sps_type
    diam_ech <- input$Dech
      if (is.null(inFile)) {
        return(NULL)
      }
      
    
      #conditionnement de la lecture des données
      if (sps_type=="HPD5"){
        data <-  read.csv2(inFile$datapath,header=TRUE, sep=";")
        
        #retrait de la premiere ligne d'unites
        data <- data[-1,]
        
        #on garde les colonnes utiles
        data <- data[,c(1,5,6,12,14,16,17)]
        
        
      } else {
        data <-  read.csv2(inFile$datapath,header=TRUE, sep=",")
        
        #retrait de la premiere ligne d'unites
        data <- data[-1,]
        
        #on garde les colonnes utiles
        data <- data[,c(1,5,6,7,10,12,17,18)]
        
      }
      

      data[] <- sapply(data, gsub, pattern = ",", replacement= ".")
      data[] <- sapply(data, as.numeric)
      
      data$pression <- data$AV.Force/(pi*(diam_ech/(10*2))^2)
      return(data)
    })  


    
datablc<-reactive({

  blcFile <- input$blanc_in
  sps_type <- input$sps_type
  
  if (is.null(blcFile)) {
    return(NULL)
  }
  
  if (sps_type=="HPD5"){
    data2 <-  read.csv2(blcFile$datapath,header=TRUE, sep=";")
    
    #retrait de la premiere ligne d'unites
    data2 <- data2[-1,]
    
    #on garde les colonnes utiles
    data2 <- data2[,c(1,5,6,12,14,16,17)]
    
    
  } else {
    data2 <-  read.csv2(blcFile$datapath,header=TRUE, sep=",")
    
    #retrait de la premiere ligne d'unites
    data2 <- data2[-1,]
    
    #on garde les colonnes utiles
    data2 <- data2[,c(1,5,6,7,10,12,17,18)]
  }
  
  data2[] <- sapply(data2, gsub, pattern = ",", replacement= ".")
  data2[] <- sapply(data2, as.numeric)
  
  return(data2)
  
})

#need to create the appropriate dataset to plot window test graph


window_data <- eventReactive(input$update_wt, {

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

  
  data$pression <- data$AV.Force/(pi*(Dech/(10*2))^2)
  
  #__________________________________________________________
  #subset sur la plage de donn?es choisie pour data et data2
  #valeurs inf ? Tmin
  data <- data[data$AV.Pyrometer>tmin,]
  
  #autres valeurs
  if (data$AV.Pyrometer[length(data$AV.Pyrometer)-1] > data$AV.Pyrometer[length(data$AV.Pyrometer)])  {
    
    #identifier le temps ? partir duquel on refroidit
    timemaxt <- min(data[data$AV.Pyrometer == max(data$AV.Pyrometer),"No."])
    #on subset ? partir de ce temps
    data <- data[data$No.<timemaxt,]
    #final subset par rapport ? Tmax
    data <- data[data$AV.Pyrometer<tmax,]
    
  } else {
    data <- data[data$AV.Pyrometer<tmax,]
  }
  
  
  #valeurs inf ? Tmin
  data2 <- data2[data2$AV.Pyrometer>tmin,]
  
  #autres valeurs
  if (data2$AV.Pyrometer[length(data2$AV.Pyrometer)-1] > data2$AV.Pyrometer[length(data2$AV.Pyrometer)])  {
    
    #identifier le temps ? partir duquel on refroidit
    timemaxt <- min(data2[data2$AV.Pyrometer == max(data2$AV.Pyrometer),"No."])
    #on subset ? partir de ce temps
    data2 <- data2[data2$No.<timemaxt,]
    #final subset par rapport ? Tmax
    data2 <- data2[data2$AV.Pyrometer<tmax,]
    
  } else {
    data2 <- data2[data2$AV.Pyrometer<tmax,]
  }
  
  #d?placement relatifs (blanc et ech)
  data$reldisp <- data$AV.Abs..Piston.T-data$AV.Abs..Piston.T[1]
  data2$reldisp <- data2$AV.Abs..Piston.T-data2$AV.Abs..Piston.T[1]
  
  #fit avec polynome deg 2 deplacement du blanc
  pred <- data.frame(AV.Pyrometer = data$AV.Pyrometer)
  model_blanc_displacement <- lm(reldisp ~ poly(AV.Pyrometer,2), data=data2)
  
  data$dplblanc <- predict(model_blanc_displacement, pred)
  
  #d?placement corrig?
  data$dplcorr <- data$reldisp - data$dplblanc
  
  
  #hauteur finale
  hfin <- mpoudre/(((pi*(Dech/20)^2))*dmes)
  
  #hauteur lit de poudre
  data$hlitpoudre <- hfin + data[length(data$No.),"reldisp"] - data$reldisp
  
  #densit?
  data$density <- mpoudre/(((pi*(Dech/20)^2))*data$hlitpoudre)
  
  #density relative
  data$reldensity <- data$density/dth
  
  
  #subset
  
  data_ech <- data[data$No. %% input$sample_rate ==0,]
  
  #dérivée
  data_ech$DDDTsurD <- NA
  for(i in 2:(length(data_ech$No.)-1)) data_ech$DDDTsurD[i] <- (1/data_ech$density[i])*((data_ech$density[i+1]-data_ech$density[i-1]))/((data_ech$No.[i+1]-data_ech$No.[i-1]))
  

  return(data_ech)
})

     output$reduced_data_table <- renderDataTable({
         head(dataset(),20)
     })
     
     output$reduced_data_table_blanc <- renderDataTable({
       head(datablc(),20)
     })
     
     output$window_table <- renderDataTable({
       window_data()
     })
     
     #color palette picker
     #the idea is to create a string that will be used in the ggplot call
     
     color_palette <- reactive({
       plottype <- input$pl_type
       if (input$col_theme == "Vir_vir"){
         if (plottype=="DV") {
           str_col_palette <- scale_color_viridis(discrete = TRUE,option="viridis")
         } else {
           str_col_palette <- scale_fill_viridis(discrete = TRUE,option="viridis")
         }
       } else if (input$col_theme == "Vir_mag") {
         if (plottype=="DV") {
           str_col_palette <- scale_color_viridis(discrete = TRUE,option="magma")
         } else {
           str_col_palette <- scale_fill_viridis(discrete = TRUE,option="magma")
         }
       } else if (input$col_theme == "Vir_plas") {
         if (plottype=="DV") {
           str_col_palette <- scale_color_viridis(discrete = TRUE,option="plasma")
         } else {
           str_col_palette <- scale_fill_viridis(discrete = TRUE,option="plasma")
         }
       } else if (input$col_theme == "Br_S1") {
         if (plottype=="DV") {
           str_col_palette <- scale_color_brewer(palette="Set1")
         } else {
           str_col_palette <- scale_fill_brewer(palette="Set1")
         }
       } else if (input$col_theme == "Br_S2") {
         if (plottype=="DV") {
           str_col_palette <- scale_color_brewer(palette="Set2")
         } else {
           str_col_palette <- scale_fill_brewer(palette="Set2")
         }
       } else if (input$col_theme == "Br_S3") {
         if (plottype=="DV") {
           str_col_palette <- scale_color_brewer(palette="Set3")
         } else {
           str_col_palette <- scale_fill_brewer(palette="Set3")
         }
       } else if (input$col_theme == "Br_Spectral") {
         if (plottype=="DV") {
           str_col_palette <- scale_color_brewer(palette="Spectral")
         } else {
           str_col_palette <- scale_fill_brewer(palette="Spectral")
         }
       }
       
       return(str_col_palette)  
     })
     
     
     output$reduced_plot <- renderPlot({
       
         win_data <- window_data() 
         g <- ggplot(win_data, aes(AV.Pyrometer, DDDTsurD)) + geom_line()
         print(g)
         
         })
     
     # output$plot.ui <- renderUI({
     #     plotOutput("reduced_plot", width="100%")
     # })
     
     output$dwnld <- downloadHandler(
         filename = function() {
             "Plot.pdf" 
         },
         content=function(file){
             file.copy("plot.pdf", file, overwrite=TRUE)
         }
     )

     htext <- rep(as.list("Lorem ipsum dolor sit amet","je suis le second paragraphe","et ainsi de suite"),5)
     output$helptxt <- renderUI(lapply(htext,tags$p))
}

shinyApp(ui = ui, server = server)
