# Scatterplot, ggplot2: qplot, boxplot, barplot, histogram,   
# Lattice plots - splom, plotmatrix(iris[1:4])
# Bean plots, scatter3d, plot3d
# Surface plots

install.packages("grDevices",dependencies=T)

library(graphics)
library(rgl)
library(beanplot)
library(rsm)
library(lattice)
library(ggplot2)
library(GGally)
library(scatterplot3d)
library(rsm)
library(grDevices)

getwd() 

data_m <- read.csv(file.choose(),header=T)

data_ref <- data_m[,-c(1:2)]

cor_mat <- cor(data_ref)

attach(data_ref)

data_input <- data.frame(Com_Noise, Com_light, Com_temp,Light,CO2,CO,Temp,TVOC,pm,Sound,Humidity)
data_output <- data.frame(Tense, Content, Sad, Alert, Tired, Happy, Upset, Calm, Focused, Productive) 

#Nature of the inputs:
boxplot(cbind(data_input[,"Light"],data_input[,"CO2"],data_input[,"pm"]), names=c("Light","CO2","pm"))
boxplot(cbind(data_input[,"Com_Noise"], data_input[,"Com_light"],data_input[,"Com_temp"],
data_input[,"Temp"],data_input[,"Sound"]),names=c("Perceived_noise","Perceived_light","Perceived_temp","Temperature","Sound"))

boxplot(cbind(data_input[,"CO"], data_input[,"TVOC"]),names=c("CO","TVOC"))
  
boxplot(data_output, main = "Response variables")

beanplot(data_input[,4],data_input[,5], names=c("Light", "CO2"),main="Bean Plots")

detach(data_ref)

#splom(cbind(data_input[,1:3],data_output[,1:3]), cex=0.2, pch=19)
ggpairs(cbind(data_input[,1:3],data_output[,1:3]))

scatterplot3d(Sound, Tense, Content, pch=19,color="red")

open3d() 
plot3d(Sound, Tense, Content, col="blue", size=10) 
writeWebGL("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Sample_model")


#ggplot(data_ref, aes(x=Com_light, y=Tense)) + geom_line() + geom_point()

d_model<-lm(cbind(Tired, Happy, Upset) ~ cbind(Com_light,Light,CO2,pm,TVOC))

summary(d_model)

#summary(manova(d_model), test="Hotelling-Lawley")
#step <- stepAIC(model, direction="both",AICc=TRUE,trace=FALSE)

# par(mfrow=c(1,3))
# image(cbind(Tired, Happy, Upset) ~ cbind(Com_light,Light,CO2,pm,TVOC) )

data_rsm <- rsm(data_output[,2] ~ FO(data_input[,1] + data_input[,3]) )

data_lm <- lm(Alert ~ poly(Sound , Light, degree=2))

contour(data_lm, ~ Sound + Light, col="blue")

persp(data_lm, Light ~ Sound, col = "blue",
 zlab = "Predicted Alertness", contours = list(z="top", col="orange"))

## "Read the Stress data"


data_stress <- read.csv(file.choose(),header=T)

## Ordinal regression



