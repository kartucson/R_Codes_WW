#############
#Multi-level regression --> Fast track experiment with results!
# - Non-linearities
# - Time-lags
# - Mutiple groups to filter/in group level model
# - lmer ... (
######



library(metricsgraphics)
install_github('rCharts', 'ramnathv')
library(devtools)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') 
library(rJava)
library(xlsx)
require(XML)
require(rCharts)
library(reshape)
install_github('slidify', 'ramnathv', ref = 'dev')
library(bayesm)
library(multilevel)
library(lme4)
install.packages("lme4",dep=T)

data_xls <- read.xlsx(file.choose(), sheetName = "Sheet1")

######################################
data_phy <- melt(data_xls,id=c("Time"))
attach(data_xls) 
attach(data_phy)

str(data_xls)
colnames(data_xls)[14] = "Participant_ID"

data_in <- data_xls[,-c(12,13)]
colnames(data_in)

Null.Model<-lme(HRV~1,random=~1|Space,data=data_in, control=list(opt="optim"))

## Now the intra-class correlation coefficient

VarCorr(Null.Model)

ICC_val = as.numeric(VarCorr(Null.Model)[1,1])/
  (as.numeric(VarCorr(Null.Model)[1,1])+as.numeric(VarCorr(Null.Model)[2,1]))

#OR one way ANOVA:

tmod<-aov(HRV~as.factor(Space),data=data_in)
ICC1(tmod)

# Estimating Group mean reliability using GMeanRel

GREL.DAT<-GmeanRel(Null.Model)
names(GREL.DAT)

GREL.DAT$ICC
GREL.DAT$MeanRel

ICC2(tmod)

# To check if intercept is significant for group means model

Null.Model.2<-gls(HRV~1,data=data_in, control=list(opt="optim"))

anova(Null.Model,Null.Model.2)

## Level 2 model:


TEMP<-aggregate(data_in$Temp,list(data_in$Space),mean,na.rm=T) 
names(TEMP)<-c("Space","G.Temp")

Tdata_in<-merge(data_in,TEMP,by="Space")

Tdata_in[100:125,]

Model.1<-lme(HRV~ Temp + G.Temp,random=~1|Space,data=Tdata_in, 
             control=list(opt="optim"))

summary(Model.1)

# Using the more advanced lmer package:
# Check quadratic, interactions, later, based on visualizations, group by space, then participant

Model.3 <- lmer(HRV ~ Temp + Humidity + (Temp+Humidity|Space),data=data_in)

### Format is : resp ~ FEexpr + (REexpr1|factor1) + (REexpr2|factor2)
