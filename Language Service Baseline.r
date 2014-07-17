# Load SiteCatalyst program
library(RSiteCatalyst)
SCAuth("username:company","secret key")

# Get report suites and metrics
bbg <- GetReportSuites()
seg <- GetSegments("bbgunitvoakurdish")

# QueueOvertime gets metric(s) overtime, QueueTrended gets same plus an element
ash <- QueueOvertime("bbgunitvoakurdi","2014-03-16","2014-06-28",segment_id="All Visits",metrics=c("event3","event5"),"week")
trend <- QueueTrended("bbgunitvoakurdish",dateFrom="2013-09-29",dateTo="2014-06-28","week","visits","geocountry",segment_id="All Visits")
n.week <- c(1:15)
n.hour <- c(1:6552)
ash <- data.frame(ash,n.week)

# Descriptive Statistics
mean(q$visits)
# Highlights which is the max
which.max(q[,6])
q[10,]
which.min([,6])
q[27,]
sd(q$visits)

# Fits linear model
lm3 <- lm(visits~n.week,data=q)
p_conf1 <- predict(lm3,interval="confidence")
p_pred1 <- predict(lm1,interval="prediction")
summary(lm3)
plot(q$n.week,q$visits,main="October 2013-June 2014 Weekly Visit Trend",xlab="Week",ylab="Visits",type="l",ylim=c(0,8500))

new <- data.frame(n.week =seq(40,52,1) ,name=NA,year=2014,month=6:9,day=1,segment="All Visits",visits=NA,stringsAsFactors=FALSE)
newq <- rbind(q,new)

# Plot with error bars
plot(newq$n.month,newq$visits,main="August 2013-June 2014 Visit Trend",xlab="Month",ylab="Visits",type="l")
abline(lm3, col="red")
matlines(q$n.week,p_conf1[,c("lwr","upr")],col=2,lty=1,type="b",pch="+")

pred <- predict(lm3,newq,se.fit = TRUE)
plot(pred$se.fit)

# Plot using moving averages
library(forecast)
lm4 <- lm(visits~n.month,data=q)
p_conf2 <- predict(lm4,interval="confidence")
ma2 <- ma(newq$visits, order=2, centre=FALSE)
pred2 <- predict(lm4,newq,se.fit = TRUE)
plot(ma2,type="l",col="red",main="August 2013-June 2014 2 Month Moving Average Visit Trend",xlab="Month",ylab="Visits")
abline(lm4, col="blue")

# Hour of day

kurd <- QueueOvertime("bbgunitvoakurdish","2014-06-01","2014-06-30",segment_id="All Visits","visits","hour")

plot(kurd$hour,kurd$visits,col="purple",main="June 2014 Hourly Visits",xlab="Hour",ylab="Visits")

# Loads ggplot2 package for better plotting
library(ggplot2)
# Creates new variable
n.hour <- c(1:6552)
ggplot(ash, aes(x = n.week, y = event5)) +
  geom_point(size = 3,color="blue") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  labs(title = "Weekly Video Views March - June 2014",x="Week",y="Video Views")+
  ylim(0,400)
  geom_errorbar(aes(ymax = 120, ymin = 0))

# plyr allows you to manipulate data if needed
library(plyr)

hour$Country <- hour$GeoSegmentation.Countries
ds <- ddply(q,.(hour,visits),summarise, mean = mean(visits), sd = sd(visits))
ggplot(q, aes(x = country, y = visits)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean),
             colour = 'red', size = 3)

ggplot() +
  geom_point(data = q, aes(x = hour, y = visits)) +
  geom_point(data = ds, aes(x = hour, y = mean),
             colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x = hour, y = mean,
                               ymin = mean - sd, ymax = mean + sd),
                colour = 'red', width = 0.4)

# Plot colored by language
ggplot(kurdig,aes(x=country,y=visits,color=language)) +
  geom_point(size=3) +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  labs(title = "Weekly Visits by Country and Language - October '13 - June '14",x="Country",y="Visits")

# ggplot Boxplot
ggplot(kurdishg, aes(x=country, y=visits)) + geom_boxplot(color="black",fill="green") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  labs(title = "VOA Kurdish Visits October - June 2014",x="Country",y="Weekly Visits")
