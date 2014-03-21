setwd('C:\\Users\\Ameya Patil\\Desktop\\DIC dataset')
list.files()
comp <- read.csv("2008.csv")
ap <- read.csv("airports.csv")
comp<- na.omit(comp)  # Removing NA values from the dataset
carriers <- read.csv("carriers.csv")

install.packages("sqldf")
install.packages("maps")
install.packages("mapdata")
install.packages("maptools")
install.packages("geosphere")
install.packages("ggplot2")
install.packages("mapproj")
library(maps)
library(mapdata)
library(sqldf)
library(maptools)
library(geosphere)
library(ggplot2)
library(mapproj)
comp$tot_delay <- comp$ArrDelay+comp$DepDelay+comp$CarrierDelay
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
require(mapproj)

map('state',boundary=TRUE,col=colors,fill=TRUE,resolution=1)

abc1 <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.Origin='LAX' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin AND comp.tot_delay > 150 AND comp.tot_delay < 500 AND comp.DepTime >1600 AND comp.DepTime<1800")
points(abc1$dlong,abc1$dlat , col="yellow" )
segments(abc1$dlong, abc1$dlat, abc1$olong, abc1$olat, col=c("yellow"))

#Plotting Delay paths from LAX
abc1 <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.Origin='LAX' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin AND comp.tot_delay >-50 AND comp.tot_delay <50 AND comp.DepTime >1600 AND comp.DepTime<1800")
points(abc1$dlong,abc1$dlat , col="green" )
segments(abc1$dlong, abc1$dlat, abc1$olong, abc1$olat, col=c("green"))

#Plotting Delay paths from LA
abc <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.Origin='LAX' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin AND comp.tot_delay >500 AND comp.tot_delay <1500 AND comp.DepTime >1600 AND comp.DepTime<1800")
points(abc$dlong,abc$dlat , col="red" )
segments(abc$dlong, abc$dlat, abc$olong, abc$olat, col=c("red"))

#Plotting Delay paths from LA
abc1 <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.Origin='LAX' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin AND comp.tot_delay > 1000 AND comp.DepTime >1600 AND comp.DepTime<1800")
points(abc1$dlong,abc1$dlat , col="blue" )
segments(abc1$dlong, abc1$dlat, abc1$olong, abc1$olat, col=c("blue"))


# Finding the Carrier name with maximm delay over the year
max_delay <-sqldf("select c1.UniqueCarrier from comp AS c1,(SELECT MAX(comp.tot_delay) AS maxdelay from comp) AS d1 where c1.tot_delay=d1.maxdelay")
port<-sqldf("select Description from carriers,max_delay where carriers.Code = max_delay.UniqueCarrier")
View(port)

# Finding the Carrier name with minimium delay over the year
min_delay <-sqldf("select c1.UniqueCarrier from comp AS c1,(SELECT MIN(comp.tot_delay) AS mindelay from comp) AS d1 where c1.tot_delay=d1.mindelay")
port_min<-sqldf("select Description from carriers,min_delay where carriers.Code = min_delay.UniqueCarrier LIMIT 1")
View(port_min)

#No. of departures from LAX to BOS throughout the week histogram
deplist<-sqldf("select DayofWeek from comp where Origin='LAX'AND Dest='BOS'")
dayfreq<-table(deplist)
deplist<-as.data.frame(dayfreq)
names(deplist)[1]<-"Day"
deplist$Day<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
ggplot(deplist, aes(x=Day, fill=Freq, y=Freq)) + geom_histogram(binwidth=.5, alpha=1, position="identity")


#Delay from LAX to BOS throughout the week histogram
deplist<-sqldf("select DayofWeek,AVG(tot_delay) AS avg_delay from comp where Origin='LAX'AND Dest='BOS' GROUP BY DayofWeek")

names(deplist)[1]<-"Day"
deplist$Day<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
ggplot(deplist, aes(x=Day, fill=avg_delay, y=avg_delay)) + geom_histogram(binwidth=.5, alpha=1, position="identity")


#Histogram of Flights operated by Carriers in 2008
car_tbl <- table(comp$UniqueCarrier)
car_tbl_fr<-as.data.frame(car_tbl)
leg<-sqldf("select Description,Var1 from carriers,car_tbl_fr where car_tbl_fr.Var1 = carriers.Code")
ggplot(data=car_tbl_fr, aes(x=Var1, y=Freq, fill=Freq),xlab="Flight Carriers", ylab="Number of Flights operated", main="Histogram of Flights operated by each carrier in 2008") + geom_bar(stat="identity")+scale_x_discrete(name="Carrier Code") + scale_y_continuous(name="No. of Flights operated")+ggtitle("Histogram of Flights operated by Carriers in 2008")

#Bar graph of Average Delay per Month in 2008
del_hr <- sqldf("select Month, avg(tot_delay) AS totaldelay from comp group by month ")
ggplot(data=del_hr, aes(x=c("Jan","Feb","March","April","May", "June", "July", "August", "September", "October", "November", "December"), y=totaldelay, fill=totaldelay))+ geom_histogram(binwidth=.5, position="dodge")+ggtitle("Average Delay per Month in 2008")+scale_x_discrete(name="Months") +
scale_y_continuous(name="Average Delay")

# K means
km <- na.omit(data.frame(sqldf("select comp.Distance,comp.tot_delay from comp where Dest = 'BOS' LIMIT 500")))
mat = as.matrix(km)
kclus <- kmeans(mat,centers=3)
#plotcluster(mat,kclus$cluster)
plot(mat,col = kclus$cluster)
library(cluster)
clusplot(sqldf("select comp.Distance,comp.tot_delay from comp where Dest = 'BOS' LIMIT 500"), kclus$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

#Map of flights of Largest Carriers and small local carrier
#Plotting paths by Major Carrirer
map('state',boundary=TRUE,col=terrain.colors(8, alpha = .6),fill=TRUE,resolution=1, title("Flight Paths of SouthWest Airlines (Major Carrier)"))
abc <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.UniqueCarrier='WN' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin ")
points(abc$dlong,abc$dlat , col="red" )
segments(abc$dlong, abc$dlat, abc$olong, abc$olat, col=c("red"))

#Plotting paths by Major Carrirer
map('state',boundary=TRUE,col=terrain.colors(8, alpha = .6),fill=TRUE,resolution=1, title("Flight Paths of American Airlines (Major Carrier)"))
abc <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.UniqueCarrier='AA' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin ")
points(abc$dlong,abc$dlat , col="red" )
segments(abc$dlong, abc$dlat, abc$olong, abc$olat, col=c("red"))

#Plotting paths by Minor Carrirer
map('state',boundary=TRUE,col=terrain.colors(8, alpha = .6),fill=TRUE,resolution=1, title("Flight Paths of  AirTran (Minor Carrier)"))
abc <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.UniqueCarrier='FL' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin ")
points(abc$dlong,abc$dlat , col="red" )
segments(abc$dlong, abc$dlat, abc$olong, abc$olat, col=c("red"))

#Plotting paths by Minor Carrirer
map('state',boundary=TRUE,col=terrain.colors(8, alpha = .6),fill=TRUE,resolution=1, title("Flight Paths of Continental Air Lines Inc (Minor Carrier)"))
abc <- sqldf("select comp.Origin,ap1.lat AS olat,ap1.long AS olong,comp.Dest,ap2.lat AS dlat, ap2.long AS dlong from comp,ap AS ap1,ap AS ap2 where comp.UniqueCarrier='CO' AND comp.Dest = ap2.Origin AND comp.origin = ap1.Origin ")
points(abc$dlong,abc$dlat , col="red" )
segments(abc$dlong, abc$dlat, abc$olong, abc$olat, col=c("red"))

#Line plot
ggplot(comp, aes(x=CRSDepTime, y=ArrDelay))+geom_point(shape=1)+geom_smooth(method=lm)
