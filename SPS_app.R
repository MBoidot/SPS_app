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
            #uiOutput("choose_columns"),

            selectInput("sps_type", "Choose SPS device:", choices =  list(FCT = c(`FCT HPD 25` = 'HPD25', `FCT HPD 5` = 'HPD5'),
                Sumitomo = c(`Dr Sinter 2080` = 'DS2080')))
         ),
        
        mainPanel(
            tabsetPanel(
                    tabPanel("Plot",
                             fluidRow(
                               column(10,
                                      h3("Material inputs"),
                                      
                                      column(3,
                                          numericInput("dmes", h5("Measured density (g/cm3)"), value = 1,width = '100%')),
                                      column(3,
                                            numericInput("dth", h5("Theoretical density (g/cm3)"), value = 1)),
                                      column(3,
                                      selectInput("Dech", h5("Diameter (mm)"), choices = c(`20` = '20', `40` = '40', `60` = '60', `80` = '80'))),
                                      
                                      column(3,numericInput("mpoudre", h5("Powder mass (g)"), value = 1))),

                             column(10,
                                      sliderInput("Trangeinput", "Temperature range", min = 0, max = 2500, value = c(450, 1500), width = '100%')),
                                      
                               
                                 column(10, h3("Graph settings"),
                                        column(5,
                                               sliderInput("point_size", "Point size", min=0, max=6, value=0.25, step = 0.05),
                                               sliderInput("point_alpha", "Alpha", min=0.25, max=1, value=1)
                                               ),
                                        
                                        column(5,
                                               selectInput("col_theme", "Choose a color palette:", choices =  list(Brewer = c(`Set1` = 'Br_S1', `Set2` = 'Br_S2', `Set3` = 'Br_S3', `Spectral` = 'Br_Spectral'),
                                                                                                                   Viridis = c(`Viridis` = 'Vir_vir',`Plasma` = 'Vir_plas',`Magma` = 'Vir_mag'))))),
                             
                             "I am above graph",
                             textOutput(paste("You have selected diameter ",input$Dech)),
                             uiOutput("plot.ui"),
                             "I am below graph",
                             fluidRow(
                                 downloadButton("dwnld",label = "Get plot"),
                                 sliderInput("Down_width", "Width (px)", min = 800, max = 4096, value = 1280),
                                 sliderInput("Down_height", "Height (px)", min = 600, max = 4096, value = 800)
                             )
                )),
                tabPanel("Table", dataTableOutput("reduced_data_table")),
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
      
      # data2 <- data2[,c(1,5,6,12,14,16,17)]
      # data2[] <- sapply(data2, gsub, pattern = ",", replacement= ".")
      # data2[] <- sapply(data2, as.numeric)
      
      # data$pression <- data$AV.Force/(pi*(input$Dech/(10*2))^2)
      
      
    
      
      return(data)
    })  


    
datablc<-reactive({ 
  inFile <- input$file_in
  blcFile <- input$blanc_in
  sps_type <- input$sps_type
  
  
  if (is.null(inFile)) {
    # User has not uploaded a file yet
    return(NULL)
  } else {
    if (is.null(blcFile)) {
      # User has not uploaded a blanc file yet
      return(NULL)
    } else {
      
      #loading the data from the xls workbook.
      
      data2 <- assign("data",read.csv2(blcFile,header=TRUE, sep=";"))
      data2 <- data2[-1,]
      
      # Sys.setlocale("LC_TIME", "C")
      Sys.setlocale(locale="English")
      
      return(data2)
    }
  }
  
})
    
    
data_colnames<-reactive({

   colunames <- names(dataset())
   return(colunames)
})


# Check boxes for elements
output$choose_columns <- renderUI({

   # Create the checkboxes and select only one by default them all by default (don't show ville and date)

   checkboxGroupInput("columns", "Choose columns",
                      choices  = data_colnames()[5:length(names(dataset()))-1],
                      selected = data_colnames()[4:5])
})

     
     reduced_data <- reactive({

        # Get the data set
         # If missing input, return to avoid error later in function
         if(is.null(input$file_in))
           return()
         
        dat <- dataset()
        sps_type <- input$sps_type

           dat <- dat[,c(1,5,6,12,14,16,17)]
           dat[] <- sapply(dat, gsub, pattern = ",", replacement= ".")
           dat[] <- sapply(dat, as.numeric)

           return(dat)
     })

     
      # output$datemin2 <- renderText({as.character(as.Date(paste("01",as.character.Date(min(dataset()$datem)),sep="-"),"%d-%y-%m"))})
      # output$datemax2 <- renderText({as.character(as.Date(paste("01",as.character.Date(max(dataset()$datem)),sep="-"),"%d-%y-%m"))})
     # output$daterangechoose <- renderText({paste("Choose between ", as.character(as.Date(paste("01",as.character.Date(min(dataset()$datem)),sep="-"),"%d-%y-%m"))," and ",as.character(as.Date(paste("01",as.character.Date(max(dataset()$datem)),sep="-"),"%d-%y-%m")))})
      
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
     
     
     
     output$reduced_data_table <- renderDataTable({
         head(dataset(),20)
     })
     
     output$reduced_plot <- renderPlot({
         plottype <- input$pl_type
         ddm <- reduced_data() 
         col_pal <- color_palette()
         
         if (plottype=="MMC"){
             
         g <- ggplot(ddm,aes(datem,value,fill=variable))+geom_boxplot(size=input$point_size,alpha=input$point_alpha)+facet_grid(variable~city,scales = "free_y")

         
         #shaping
         g <-  g + theme_bw()
         # g <- g + scale_fill_viridis(discrete = TRUE,option="plasma") 
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
            
         } else if (plottype=="SC"){


                 g <- ggplot(ddm, aes(Season, value, group=interaction(city,Season),fill=Season));
                 
                 
                 #shaping
                 g <-  g + theme_bw()
                 g <- g + theme(axis.text.x=element_text(size=12,angle=0,hjust=0.5,vjust=0.5),
                                axis.title.x=element_blank(),
                                axis.title.y=element_blank(),
                                plot.title=element_text(size=25, face="bold",vjust=1),
                                strip.text.x=element_text(size=14,face="bold",color="white"),
                                strip.text.y=element_text(size=14, face="bold",color="white"),
                                strip.background =element_rect(fill="#2d2d2d"),
                                legend.position="none")
                 
                 g <- g + ggtitle("title") + ylab("label_y")
                 g <- g + geom_boxplot(width=1)
                 g <- g + stat_qq(aes(Season,value,group=interaction(city,Season),colour=city))
                 # g <- g + scale_fill_viridis(discrete = TRUE,option="plasma") 
                  g <- g + col_pal
                 g <- g + facet_grid(variable~city,scales="free")
                 
                 ggsave("plot.pdf", g,width =input$Down_width/300,height = input$Down_height/300)
               
         } else {
             
             
             g <- ggplot(ddm,aes(sampling_date,value,color=variable))+geom_jitter(size=input$point_size,alpha=input$point_alpha) +geom_line() +facet_grid(.~city,scales = "free_y")
             
             
             #shaping
             g <- g + theme_bw()
             # g <- g + scale_fill_viridis(discrete = TRUE,option="plasma") 
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
         }
         print(g)
         })
     
     
     output$plot.ui <- renderUI({
         plotOutput("reduced_plot", width = paste0(input$width, "%"), height = input$height)
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
