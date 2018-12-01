#Chargement des bibliothèques
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

#wd <- "S:\\380-Matériaux\\380.225-CERAMIQUES\\380.225.05-CERAPRINT\\07_TECHNIQUE\\Fichiers SPS HPD5"
wd <- "C:\\Users\\mathi\\Desktop\\CERAPRINT"
setwd(wd)

#définition des paramètres de l'échantillon, régulation et outillages

regul <- "Pyro" #ou "TC"
#diamètre échantillon en mm
Dech <- 20
#masse de poudre (g)
mpoudre <- 10
#densité théorique (g/cm3)
dth <- 3.016
#densité mesurée
dmes <- 3
#intervalle de temperature
tmin <- 650
tmax <- 1650

#chargement des données du fichier et du blanc
data <- read.csv2(file.choose(),header=TRUE, sep=";")
#blanc
data2 <- read.csv2(file.choose(),header=TRUE, sep=";")

#retrait de la première ligne d'unités
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

#__________________________________________________________
#subset sur la plage de données choisie pour data et data2
#valeurs inf à Tmin
data <- data[data$AV.Pyrometer>tmin,]

#autres valeurs
if (data$AV.Pyrometer[length(data$AV.Pyrometer)-1] > data$AV.Pyrometer[length(data$AV.Pyrometer)])  {

  #identifier le temps à partir duquel on refroidit
  timemaxt <- min(data[data$AV.Pyrometer == max(data$AV.Pyrometer),"No."])
  #on subset à partir de ce temps
  data <- data[data$No.<timemaxt,]
  #final subset par rapport à Tmax
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
  #on subset à partir de ce temps
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
data$hlitpoudre <- hfin + data[data$No.==length(data$No.),"reldisp"] - data$reldisp

#densité
data$density <- mpoudre/(((pi*(Dech/20)^2))*data$hlitpoudre)

#densité relative
data$reldensity <- data$density/dth

#dérivée
data$DDDTsurD <- NA
for(i in 2:(length(data$No.)-1)) data$DDDTsurD[i] <- (1/data$density[i])*((data$density[i+1]-data$density[i-1]))/((data$AV.Pyrometer[i+1]-data$AV.Pyrometer[i-1]))


#subset
taux_ech <- 11
data_ech <- data[data$No. %% taux_ech ==0,]


p1 <- ggplot(data_ech, aes(AV.Pyrometer, DDDTsurD))
p1 <- p1+geom_line()
p1 <- p1+ xlim(1000,1800)
p1