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
        titlePanel("HPD5 and HPD25 file treatment"),

        sidebarPanel(width = 2,
            fileInput("file_in","Data file", multiple = FALSE),
            fileInput("blanc_in","Blanc file", multiple = FALSE),

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
                                          numericInput("dmes", "Measured density (g/cm3)", value = 1,width = '100%'),
                                          numericInput("dth", "Theoretical density (g/cm3)", value = 1)),
                                      column(3,numericInput("Dech", "Diameter (mm)", 20, min = 10, max = 80),
                                          numericInput("mpoudre", "Powder mass (g)", value = 1))
                                      ),

                             
                                 # column(12, h3("Graph settings"),
                                 #        column(6,
                                 #               sliderInput("point_size", "Point size", min=0, max=6, value=0.25, step = 0.05),
                                 #               sliderInput("point_alpha", "Alpha", min=0.25, max=1, value=1)
                                 #               ),
                                 #        
                                 #        column(6,
                                 #               selectInput("col_theme", "Choose a color palette:", choices =  list(Brewer = c(`Set1` = 'Br_S1', `Set2` = 'Br_S2', `Set3` = 'Br_S3', `Spectral` = 'Br_Spectral'),
                                 #                                                                                   Viridis = c(`Viridis` = 'Vir_vir',`Plasma` = 'Vir_plas',`Magma` = 'Vir_mag')))))
                             

                             column(12,
                                    column(8,
                                           h2("Recorded temperature"),
                                           plotlyOutput("avtemp_plot"),
                                           h2("Blanc relative displacement and model"),
                                           plotOutput("blancdisp_plot"),
                                           h2("Densification rate"),
                                           plotOutput("window_plot"),
                                           column(2,numericInput("sample_rate", "Sampling rate", value = 10)),
                                           column(3,sliderInput("smooth", "Smoothing", min=0, max=1, value=0.25)),
                                           column(4,
                                                  sliderInput("Trangeinput", "Temperature range", min = 0, max = 2500, value = c(750, 1134), width = '100%')),
                                           br(),
                                           br(),
                                           column(2,actionButton("update_wt2", "Update Windowtest plot"))),
                                    column(4,
                                           verbatimTextOutput("brush"))
                             ),
                                    column(8,h2("Density evolution"),
                                           plotOutput("density_plot"))),
                                    
                                    

                             fluidRow(
                                 downloadButton("dwnld",label = "Get plot"),
                                 sliderInput("Down_width", "Width (px)", min = 800, max = 4096, value = 1280),
                                 sliderInput("Down_height", "Height (px)", min = 600, max = 4096, value = 800)
                             )
                ),
                tabPanel("Data", dataTableOutput("reduced_data_table")),
                tabPanel("Data blanc", dataTableOutput("reduced_data_table_blanc")),
                tabPanel("Sampled data",dataTableOutput("window_table"),downloadButton("downloadwindowtable", "Download")),
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
      data$reldisp <- data$AV.Abs..Piston.T-data$AV.Abs..Piston.T[1]
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
  
  data2$reldisp <- data2$AV.Abs..Piston.T-data2$AV.Abs..Piston.T[1]
  
  return(data2)
  
})

#need to create the appropriate dataset to plot window test graph


window_data <- eventReactive(input$update_wt | input$update_wt2, {

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

     output$avtemp_plot <- renderPlotly({
       data <- dataset()
       p <- ggplot(data, aes(No., AV.Pyrometer,key=No.))
       p <- p + geom_line()
       p <- p + geom_point(size=0.2)
       p <- p + xlab("Time (s)") + ylab("Temperature (°C)")
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
       data <- dataset()
       data2 <- datablc()
       win_data <- window_data() 
       g <- ggplot(data2, aes(AV.Pyrometer, reldisp))
       g <- g + geom_line()
       g <- g + geom_line(data=win_data,aes(AV.Pyrometer, dplblanc))
       g <- g + xlab("Temperature (°C)") + ylab("Relative blanc displacement (mm)")


       print(g)
       
     })
     
     output$window_plot <- renderPlot({

         win_data <- window_data() 
         g <- ggplot(win_data, aes(AV.Pyrometer, DDDTsurD))
         g <- g + geom_line()
         g <- g + geom_point(size=3,alpha=0.5)
         g <- g + geom_smooth(se = FALSE,span = input$smooth)
         g <- g + xlab("Temperature (°C)") + ylab(expression(1/D. ~ partialdiff ~ D / partialdiff ~t))

         print(g)
         
         })
     
     output$density_plot <- renderPlot({
       densityplot_data <- window_data()
       g <- ggplot(densityplot_data, aes(AV.Pyrometer, reldensity))
       g <- g +geom_line()
       g <- g + xlab("Temperature (°C)") + ylab("Relative density (%)")

       print(g)
       
     })

     output$dwnld <- downloadHandler(
         filename = function() {
             "Plot.pdf" 
         },
         content=function(file){
             file.copy("plot.pdf", file, overwrite=TRUE)
         }
     )

     htext <- div(
       h1("reste à coder :"),
       br(),
       "Interface : réarrangement des boutons dans l'UI au début",
       br(),
       "Suppression des titres des graphes et intégration en tant qu'éléments HTML",
       br(),
       "boutons de téléchargement des graphes individuels et des tables de données",
       br(),
       "intégration d'un graphe de résumé complet des données avec facet grid et gestion couleurs alpha etc."
       )
     
     output$helptxt <- renderUI(htext)
}

shinyApp(ui = ui, server = server)
