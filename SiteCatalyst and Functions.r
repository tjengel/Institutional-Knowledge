# Load Site Catalyst program
library(RSiteCatalyst)
# Authorization
SCAuth("username:company","secret key")
# Get report suite names and metrics
bbg <- GetReportSuites()
e <- GetAvailableElements("bbgunitvoaenglish")
m <- GetAvailableMetrics("bbgunitvoaenglish")
q <- QueueOvertime("bbgunitvoakurdish","2013-08-01","2014-06-30",segment_id="All Visits","visits","month")

# Create variable to plot with
n.month <- c(1:11)
n.month <- as.numeric(n.month)
q <- data.frame(q,n.month)

# Fits linear model
lm1 <- lm(q$pageviews~q$n.month)
p_conf1 <- predict(lm1,interval="confidence")
p_pred1 <- predict(lm1,interval="prediction")

# Plot with error bars
plot(q$n.month,q$pageviews,main="Page View Trend",xlab="Month",ylab="Page Views")
abline(lm1, col="red")
matlines(q$n.month,p_conf1[,c("lwr","upr")],col=2,lty=1,type="b",pch="+")

# Fit parabola to above data
plot(q$n.month,fitted(lm(q$pageviews~poly(q$n.month,2))),type="l")

plot(q$n.month,q$pageviews,main="Page View Trend",xlab="Month",ylab="Page Views",type="l", col=grey(.5))

# Plot using moving averages
library(forecast)
ma <- ma(q$pageviews, order=3, centre=FALSE)
plot(ma,type="l",col="red")

# Function to iterate alert setting all report suites
rs <- bbg$rsid
f <- function(i){ 
  l <- vector(length = length(i))
  for(i in 1:493){
	pageviews <- QueueOvertime(bbg$rsid[i],"2014-04-01","2014-05-31",segment_id="All Visits","pageviews","day")
	y <- median(pageviews$pageviews) - 3*sd(pageviews$pageviews)
	x <- median(pageviews$pageviews) - 2*sd(pageviews$pageviews)
	l[i] <- ifelse(y < 0, print(x), print (y))
  print (x)
  print (l)
}
return(l)
}

# Average Visits Function
f2 <- function(i){ 
  l <- vector(length = length(i))
  for(i in 1:493){
    visits <- QueueOvertime(bbg$rsid[i],"2013-09-29","2014-06-28",segment_id="All Visits","visits","week")
    y <- sum(visits$visits)/nrow(visits)
    l[i] <- print(y)
  }
  return(l)
}

# Page Views/Visit Function
f3 <- function(i){ 
  l <- vector(length = length(i))
  for(i in 1:493){
    pageviews <- QueueOvertime(bbg$rsid[i],"2013-09-29","2014-06-28",segment_id="All Visits",metrics=c("pageviews","visits"),"week")
    x <- sum(pageviews$pageviews)/sum(pageviews$visits)
    l[i] <- print(x)
  }
  return(l)
}

# Average Weekly Visitors Function
f4 <- function(i){ 
  l <- vector(length = length(i))
  for(i in 1:493){
    visitors <- QueueOvertime(bbg$rsid[i],"2013-09-29","2014-06-28",segment_id="All Visits","visitorsweekly","week")
    z <- sum(visitors$visitorsweekly)/nrow(visitors)
    l[i] <- print(z)
  }
  return(l)
}
