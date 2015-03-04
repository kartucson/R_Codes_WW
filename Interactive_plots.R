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

#data_csv <- read.csv(file.choose(),header=T,colClasses=c("Time"="Date"))
data_csv <- read.csv(file.choose(),header=T)
data_xls <- read.xlsx(file.choose(), sheetName = "Sheet1")

colnames(data_csv)

data_phy <-data_csv[,-1]

m<-strptime(data_csv[,1],format = '%m/%d/%Y %H:%M')
data_phy <- data.frame(m,data_phy)


colnames(data_phy) <- colnames(data_csv)

strptime(m, "%b%d%H%M%OS")

as.POSIXct(Time,"%m/%d %h:%m") 
T <- as.POSIXct(as.numeric(format(Time, "%b-%d %H:%M")))

#######################################
data_phy <- melt(data_xls,id=c("Time"))
attach(data_xls) 
attach(data_phy)

plot(as.POSIXct(Time), Light)

d3chart <-nPlot( Sound ~ Time, data=data_phy, type="lineChart")

d3chart$chart(tooltipContent = "#! function(key, x, y, e){ 
  return 'Sound Value: ' + e.point.Sound
} !#")


d3chart$xAxis(
  tickFormat = "#!function(d) {return d3.time.format('%b %d %H:%M')(new Date( d * 86400000 ));}!#"
)

d3chart$xAxis(
  tickFormat = "#!function(d) {return d3.time.format('%b %d %H:%M');}!#"
)
## Doesnt come right###

d3chart$save('d3chart.html', cdn=TRUE)

### RIckshaw:

options(RCHART_TEMPLATE = 'Rickshaw.html')

data_phy$Stress <- as.factor(data_phy$Stress)

r3 <- Rickshaw$new()
r3$layer(
  Sound ~ Time,  
  data = data_phy,
  colors= "steelblue", 
  type = "line",
  height = 140,
  width = 800
)

r4 <- Rickshaw$new()
r4$layer(x=Time, y=Light,
  data = data_phy,
  colors= "green", 
  type = "line",
  height = 100,
  width = 1800
)

r3$layer(
  Stress ~ Time,
  data = data_phy,
  colors= "yellow", 
  type = "line",
  height = 140,
  width = 800
)

r3$layer(
  CO2 ~ Time,
  data = data_phy,
  colors= "brown", 
  type = "line",
  height = 140,
  width = 800
)

r3$layer(
  CO ~ Time,
  data = data_phy,
  colors= "grey", 
  type = "line",
  height = 140,
  width = 800
)


m1 <- mPlot(x = "Time", y = "Sound", type = "Line", data = data_phy)
m1$set(pointSize = 0, lineWidth = 1)
m1$print("chart2")

plot(Time,Light)

,xlim=c(2015-01-08 05:00:00 MST,2015-01-09 05:00:00 MST))


p4 <- Rickshaw$new()
p4$layer(value ~ Time, group = "variable", data = data_phy, type = "line", width = 1000,height=800)
# add a helpful slider this easily; other features TRUE as a default
p4$set(slider = TRUE)

data_xls$Stress <- as.factor(data_xls$Stress)

# add a helpful slider this easily; other features TRUE as a default
p6 <- Rickshaw$new()
p6$layer(Sound ~ Time, group = "Stress", data = data_xls, type = "line", width = 1000,main="Sound Levels")
# add a helpful slider this easily; other features TRUE as a default

p7 <- Rickshaw$new()
p7$layer(Humidity ~ Time, group = "Stress", data = data_xls, type = "line", width = 1000)
p7$yAxis( axisLabel = "Humidity" )
p7$set(slider = TRUE)


p4$set(slider = TRUE)
p4$print("Stacked area plot")



h1 <- hPlot(x = "Time" , y = "Sound", data = data_xls, type = c("bubble", 
    "bubble", "bubble"), group = "Stress", size = "TVOC",width=1000)
h1$set(slider = TRUE)
h1$print("HighCharts")

data_phy$Time <- format(data_phy$Time,"%b-%d %H:%M")
x1 <- xPlot(value ~ Time, group = "variable", data = data_phy, type = "line-dotted", stacked=T)

x1$print("chart4")

