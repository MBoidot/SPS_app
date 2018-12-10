#Chargement des biblioth?ques
library(ggplot2)
library(grid)
library(reshape2)
library(plyr)
library(viridis)

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
             plot.title=element_text(size=25, face="bold",vjust=1),
             strip.text.x=element_text(size=14,face="bold"),
             strip.text.y=element_text(size=14,face="bold"))

#wd <- "S:\\380-Mat?riaux\\380.225-CERAMIQUES\\380.225.05-CERAPRINT\\07_TECHNIQUE\\Fichiers SPS HPD5"
#wd <- "C:\\Users\\mathi\\Desktop\\CERAPRINT"
#setwd(wd)

#d?finition des param?tres de l'?chantillon, r?gulation et outillages

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
tmin <- 600
tmax <- 1600

#chargement des donn?es du fichier et du blanc
data <- read.csv2(file.choose(),header=TRUE, sep=";")
#blanc
data2 <- read.csv2(file.choose(),header=TRUE, sep=";")

#retrait de la premi?re ligne d'unit?s
data <- data[-1,]
data2 <- data2[-1,]

#on garde les colonnes utiles
data <- data[,c(1,5,6,12,14,16,17)]
data[] <- sapply(data, gsub, pattern = ",", replacement= ".")
data[] <- sapply(data, as.numeric)

data2 <- data2[,c(1,5,6,12,14,16,17)]
data2[] <- sapply(data2, gsub, pattern = ",", replacement= ".")
data2[] <- sapply(data2, as.numeric)

data$pression <- data$AV.Force/(pi*(Dech/(10*2))^2)

p3 <- ggplot(data, aes(No., AV.Pyrometer))
p3 <- p3+geom_line()
p3
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

#subset
taux_ech <- 12
data_ech <- data[data$No. %% taux_ech ==0,]

#d?riv?e
data_ech$DDDTsurD <- NA
for(i in 2:(length(data_ech$No.)-1)) data_ech$DDDTsurD[i] <- (1/data_ech$density[i])*((data_ech$density[i+1]-data_ech$density[i-1]))/((data_ech$AV.Pyrometer[i+1]-data_ech$AV.Pyrometer[i-1]))

#plot du modèle de régression du déplacement du blanc 
p1 <- ggplot(data2, aes(AV.Pyrometer, reldisp))
p1 <- p1+geom_line()
p1 <- p1+geom_line(data=data,aes(AV.Pyrometer, dplblanc))
p1

p2 <- ggplot(data_ech, aes(AV.Pyrometer, DDDTsurD))
p2 <- p2+geom_line()
p2


