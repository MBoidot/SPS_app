#Chargement des biblioth?ques
library(ggplot2)
library(grid)
library(reshape2)
library(plyr)
library(viridis)
library(scales)
library(xlsx)
library(readxl)

#Theme update
theme_set(theme_bw(10))
theme_update(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             legend.title=element_text(size=18),
             axis.title.x=element_text(size=20),
             axis.title.y=element_text(size=20,angle=90,vjust=1.5),
             axis.text.x=element_text(size=16),
             axis.text.y=element_text(size=16),
             legend.text=element_text(size=16),
             plot.title=element_text(size=25, face="bold",vjust=0.5),
             strip.text.x=element_text(size=14,face="bold"),
             strip.text.y=element_text(size=14,face="bold"))

#wd <- "S:\\380-Mat?riaux\\380.225-CERAMIQUES\\380.225.05-CERAPRINT\\07_TECHNIQUE\\Fichiers SPS HPD5"
wd <- "D:\\Travail\\Git_projects\\SPS_app\\Files\\DrSinter 2080"
setwd(wd)

#definition des parametres de l'échantillon, régulation et outillages

regul <- "Pyro" #ou "TC"
#diam?tre ?chantillon en mm
Dech <- 20
#masse de poudre (g)
mpoudre <- 10
#densit? th?orique (g/cm3)
dth <- 4.5
#densit? mesur?e
dmes <- 4
#intervalle de temperature
tmin <- 765
tmax <- 1735

#chargement des données du fichier et du blanc

data_file <- file.choose() #chem_file is the file path
data_wb <- loadWorkbook(data_file) #chem_wb has class "jobjref
datasheets <- getSheets(data_wb) #chemsheets is a list
datasheets_names <- as.factor(names(getSheets(data_wb)))

data <- read.xlsx(data_file, sheetName = datasheets_names[1])

if (data[1,1] == data[2,1]){
  time_interval <- 0.5
} else{
  time_interval <- 1
}

data$No. <- seq(0,nrow(data)-1)*time_interval

data[,c(1,2,3,4,8,9)] <- NULL

names(data) <- c("AV.Pyrometer", "pression", "AV.Abs..Piston.T","No.")

#chargement des données du blanc

data2_file <- file.choose() #chem_file is the file path
data2_wb <- loadWorkbook(data2_file) #chem_wb has class "jobjref
data2sheets <- getSheets(data2_wb) #chemsheets is a list
data2sheets_names <- as.factor(names(getSheets(data2_wb)))

data2 <- read.xlsx(data2_file, sheetName = data2sheets_names[1])

if (data2[1,1] == data2[2,1]){
  time_interval <- 0.5
} else{
  time_interval <- 1
}

data2$No. <- seq(0,nrow(data2)-1)*time_interval

data2[,c(1,2,3,4,8,9)] <- NULL

names(data2) <- c("AV.Pyrometer", "pression", "AV.Abs..Piston.T","No.")




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


#valeurs inf à Tmin
data2 <- data2[data2$AV.Pyrometer>tmin,]

#autres valeurs
if (data2$AV.Pyrometer[length(data2$AV.Pyrometer)-1] > data2$AV.Pyrometer[length(data2$AV.Pyrometer)])  {
  
  #identifier le temps à partir duquel on refroidit
  timemaxt <- min(data2[data2$AV.Pyrometer == max(data2$AV.Pyrometer),"No."])
  #on subset ? partir de ce temps
  data2 <- data2[data2$No.<timemaxt,]
  #final subset par rapport à Tmax
  data2 <- data2[data2$AV.Pyrometer<tmax,]
  
} else {
  data2 <- data2[data2$AV.Pyrometer<tmax,]
}

#déplacement relatifs (blanc et ech)
data$reldisp <- data$AV.Abs..Piston.T-data$AV.Abs..Piston.T[1]
data2$reldisp <- data2$AV.Abs..Piston.T-data2$AV.Abs..Piston.T[1]

#fit avec polynome deg 2 deplacement du blanc
pred <- data.frame(AV.Pyrometer = data$AV.Pyrometer)
model_blanc_displacement <- lm(reldisp ~ poly(AV.Pyrometer,2), data=data2)

data$dplblanc <- predict(model_blanc_displacement, pred)

#déplacement corrigé
data$dplcorr <- data$reldisp - data$dplblanc

#hauteur finale
hfin <- mpoudre/(((pi*(Dech/20)^2))*dmes)

#hauteur lit de poudre
data$hlitpoudre <- hfin + data[length(data$No.),"reldisp"] - data$reldisp

#densité
data$density <- mpoudre/(((pi*(Dech/20)^2))*data$hlitpoudre)

#densité relative
data$reldensity <- data$density/dth

#subset
taux_ech <- 12
data_ech <- data[data$No. %% taux_ech ==0,]

#dérivée
data_ech$DDDTsurD <- NA
for(i in 2:(length(data_ech$No.)-1)) data_ech$DDDTsurD[i] <- (1/data_ech$density[i])*((data_ech$density[i+1]-data_ech$density[i-1]))/((data_ech$No.[i+1]-data_ech$No.[i-1]))

#plot de la température enregistrée sur l'échantillon sur toute 
#la gamme de température permettant le choix de la gamme de température restreinte
p <- ggplot(data, aes(No., AV.Pyrometer))
p <- p+ geom_line()
p <- p+ ggtitle("Choose temperature range")
p <- p+ xlab("Time (s)") + ylab("Temperature (°C)")
p <- p + scale_y_continuous(breaks=c(seq(min(data$AV.Pyrometer),max(data$AV.Pyrometer),100),max(data$AV.Pyrometer)))
p

#plot du modèle de régression du déplacement du blanc 
p1 <- ggplot(data2, aes(AV.Pyrometer, reldisp))
p1 <- p1+geom_line()
p1 <- p1+geom_line(data=data,aes(AV.Pyrometer, dplblanc))
p1
#plot fenetre de vitesse de densification
p2 <- ggplot(data_ech, aes(AV.Pyrometer, DDDTsurD))
p2 <- p2+geom_line()
p2
#plot évolution de la densité relative
p3 <- ggplot(data, aes(AV.Pyrometer, reldensity))
p3 <- p3 +geom_line()
p3 <- p3 + scale_y_continuous(labels = percent)
p3 <- p3 + ggtitle("Evolution of relative density")
p3 <- p3 + xlab("Temperature (°C)") + ylab("Relative density (%)")
p3
