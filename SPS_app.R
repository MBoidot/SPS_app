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
                             
                             uiOutput("plot.ui"),
                             
                             
                             "I am below graph",
                             fluidRow(
                                 downloadButton("dwnld",label = "Get plot"),
                                 sliderInput("Down_width", "Width (px)", min = 800, max = 4096, value = 1280),
                                 sliderInput("Down_height", "Height (px)", min = 600, max = 4096, value = 800)
                             )
                )),
                tabPanel("Data", dataTableOutput("reduced_data_table")),
                tabPanel("Reference", dataTableOutput("reduced_data_table_blanc")),
                tabPanel("Reference", dataTableOutput("window_table")),
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
      
      data <-  read.csv2(inFile$datapath)
      
      #retrait de la premiere ligne d'unites
      data <- data[-1,]
      # data2 <- data2[-1,]
      
      #on garde les colonnes utiles
      data <- data[,c(1,5,6,12,14,16,17)]
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
  
  data2 <-  read.csv2(blcFile$datapath)
  
  #retrait de la premiere ligne d'unites
  data2 <- data2[-1,]
  # data2 <- data2[-1,]
  
  #on garde les colonnes utiles
  data2 <- data2[,c(1,5,6,12,14,16,17)]
  data2[] <- sapply(data2, gsub, pattern = ",", replacement= ".")
  data2[] <- sapply(data2, as.numeric)

  return(data2)
  
})

#need to create the appropriate dataset to plot window test graph

check <- eventReactive(input$update_wt, {
  
  #import all inputs
  sps_type <- input$sps_type
  dmes <- input$dmes
  dth <- input$dth
  Dech <- input$Dech
  mpoudre <- input$mpoudre
  Tmin <- input$Trangeinput[1]
  Tmax <- input$Trangeinput[2]

  return(c("sps_type",sps_type,"dmes",dmes,"dth",dth,"Dech",class(Dech),"mpoudre",mpoudre,"Tmin",Tmin,"Tmax",Tmax))
})


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
  data$hlitpoudre <- hfin + data[data$No.==length(data$No.),"reldisp"] - data$reldisp
  
  #densit?
  data$density <- mpoudre/(((pi*(Dech/20)^2))*data$hlitpoudre)
  
  #densit? relative
  data$reldensity <- data$density/dth
  
  #d?riv?e
  data$DDDTsurD <- NA
  for(i in 2:(length(data$No.)-1)) data$DDDTsurD[i] <- (1/data$density[i])*((data$density[i+1]-data$density[i-1]))/((data$AV.Pyrometer[i+1]-data$AV.Pyrometer[i-1]))
  
  
  #subset
  taux_ech <- 11
  data_ech <- data[data$No. %% taux_ech ==0,]

    
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
         plottype <- input$pl_type
         tmin <- input$Trangeinput[1]
         tmax <- input$Trangeinput[2]
         win_data <- window_data() 
         col_pal <- color_palette()
         
         g <- ggplot(win_data, aes(AV.Pyrometer, DDDTsurD)) + geom_line() + xlim(0.9 * tmin ,1.1 * tmax)

         #shaping
         g <-  g + theme_bw()
         g <- g + col_pal
         g <- g + theme(axis.text.x=element_text(size=12,angle=90,hjust=0,vjust=0.5),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           plot.title=element_text(size=25, face="bold",vjust=1),
                           strip.text.x=element_text(size=14,face="bold",color="white"),
                           strip.text.y=element_text(size=14, face="bold",color="white"),
                           strip.background =element_rect(fill="#2d2d2d"),
                           legend.position="none")
            
            ggsave("plot.pdf", g,width =input$Down_width/300,height = input$Down_height/300)
            
         print(g)
         })
     
     
     output$plot.ui <- renderUI({
         plotOutput("reduced_plot", height = input$height)
     })
     
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
