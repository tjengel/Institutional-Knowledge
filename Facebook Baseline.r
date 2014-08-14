Load Data

library(ggplot2)
library(reshape2)
library(plyr)
library(scales)

# Edits data imported from Facebook
Reachface$m13.34percent <- Reachface$m13.34total/Reachface$mtotal
Reachface$total <- Reachface$mtotal+Reachface$ftotal
Reachface$mpercent <- Reachface$mtotal/Reachface$total
Reachface$fpercent <- Reachface$ftotal/Reachface$total
Reachface$f13.34percent <- Reachface$f13.34total/Reachface$ftotal
Reachface$m13.34percent <- Reachface$m13.34total/Reachface$mtotal
viface$total.13.34 <- viface$m.13.34.total + viface$f.13.34.total
viface$percent.13.34 <- viface$total.13.34/viface$total
swahili$f.percent.all <- swahili$f.13.34total/swahili$total

# Date transformation
fabook <- reshape(vbook,varying=c("f.percent","m.percent"),v.names="Percent",timevar = "Date",times = c("f.percent","m.percent"),direction = "long")

# Not sure if I need this
fabook <- melt(vbook,value.name="value")

# Turns data from data into R data object
kbook$date <- as.Date(kbook$date,format="%m/%d/%Y")

# Rename factor levels using plyr package
fabook$Date <- revalue(fabook$Date, c("f.percent"="Female Percent", "m.percent"="Male Percent"))
fabook$Date <-as.factor(fabook$Date)
fabook$Date <- factor(fabook$Date, levels = rev(levels(fabook$Date)))
tube$Week <- as.Date(cut(tube$Date,breaks = "week"),start.on.monday = TRUE)
plyt <- ddply(vtype, .(device.type), summarize, plays=sum(plays))

# Date plotting
ggplot(data = kbook,aes(date, engagement.rate)) +  
  stat_summary(fun.y = sum,geom = "point",color="blue",size=3)+
  scale_x_date(labels = date_format("%m/%d/%Y"),breaks = "2 weeks")+
  stat_smooth(method="lm",fullrange=TRUE) +
  labs(title = "Weekly Engagement Rate - October '13 - June '14",x="Week",y="Percent")+
  theme(axis.text.x = element_text(angle=90,size=15))+
  theme(axis.text.y = element_text(size=15))+
  ylim(0,1)

# Stack area charts
ggplot(fabook, aes(x = date, y = Percent, group=Date,fill =Date)) + 
  geom_area(position = 'fill')+
  scale_fill_brewer(palette=1)+
  scale_x_date(labels = date_format("%m/%d/%Y"),breaks = "2 weeks")+
  theme(axis.text.x = element_text(angle=90)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  labs(title="Male/Female Percent of Audience - October '13 - June '14",x="Week",y="Percent of Audience")

ggplot(book,aes(x=Date,y=plays)) +
  geom_point(size=3,color="blue") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  stat_smooth(method="lm",fullrange=TRUE) +
  labs(title = "Weekly Audio Plays - March - June 2014",x="Week",y="Video Views")+
  ylim(0,6000)

ggplot(Reachface,aes(x=n.week,y=ftotal)) +
  geom_point(size=3,color="blue") +
  theme(axis.text.x = element_text(hjust = 1, size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  stat_smooth(method="lm",fullrange=TRUE) +
  labs(title = "Weekly Female Reach - October '13- June '14",x="Week",y="Number of Females")

ggplot(Reachface,aes(x=n.week,y=m13.34total)) +
  geom_point(size=3,color="blue") +
  theme(axis.text.x = element_text(hjust = 1, size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  stat_smooth(method="lm",fullrange=TRUE) +
  labs(title = "Weekly Male 13-34 Reach - October '13 - June '14",x="Week",y="Number of Males Aged 13-34")

ggplot(Reachface,aes(x=n.week,y=percent13.34)) +
  geom_point(size=2,color="blue") +
  theme(axis.text.x = element_text(hjust = 1, size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  stat_smooth(method="lm",fullrange=TRUE) +
  labs(title = "Weekly 13-34 Reach - October '13 - June '14",x="Week",y="Percent of Individuals Aged 13-34")+
  ylim(0,1)

# Loess Smoothing
ggplot(swahili,aes(x=n.date,y=total13.34)) +
  geom_point(size=2,color="blue") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  stat_smooth(method="loess",span=.95,fullrange=TRUE) +
  labs(title = "Weekly Engaged Individuals Aged 13-34 October '13 - June '14",x="Week",y="Weekly Engaged Individuals Aged 13-34")+
  ylim(0,500)+
  xlim(0,52)

# Average per group
ggplot(h, aes(x=factor(hour), y=visits),color="blue")) + stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  labs(title = "Average Hourly Visits October '13 - June '14",x="Hour",y="Average Number of Visits")

# Plot by device type
ggplot(data = vtype, aes(x = week, y = video.views, group=device.type,colour = device.type)) + geom_line(size=1.5)+
  theme(axis.text.x = element_text(size=13,angle=90)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  labs(title = "Weekly Video Views by Device March  - June 2014",x="Week",y="Video Views")

# Hour factor plot
camh$Time <- camh$hour
camh$Time[camh$Time<11] <- 2
camh$Time[camh$Time>17] <- 2
camh$Time[camh$Time>2] <- 1
camh$Time <- as.factor(camh$Time)
camh$Time <- factor(camh$Time,levels=c(1,2),labels=c("Night","Day"))

ggplot(q, aes(x=factor(hour), y=visits,fill=Time)) + stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  scale_fill_brewer(palette="Paired")+
  labs(title = "Average Hourly Visits October '13 - June '14",x="Hour",y="Average Number of Visits")

# Steps for combining histogram variables
v <- vtype
v$audio.play <- NULL
melt_df <- melt(v)
ggplot(melt_df, aes(variable,value)) + 
  geom_bar(stat="identity",fill="blue")+
  labs(title = "Video Conversions March - June 2014",x="How Much Video Completed",y="Count")

write.csv(Reachface,"Reachface.csv")

sum <- rowsum(tube$Views, format(tube$Date, "%m/%d/%Y"))

library(plyr)
tube$weekd <- format(tube$Date, format="%Y-%U")
