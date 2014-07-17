Load Data

# Creates new variables from Facebook Data
Reachface$m13.34percent <- Reachface$m13.34total/Reachface$mtotal
Reachface$total <- Reachface$mtotal+Reachface$ftotal
Reachface$mpercent <- Reachface$mtotal/Reachface$total
Reachface$fpercent <- Reachface$ftotal/Reachface$total
Reachface$f13.34percent <- Reachface$f13.34total/Reachface$ftotal
Reachface$m13.34percent <- Reachface$m13.34total/Reachface$mtotal
Reachface$total13.34 <- Reachface$m13.34total+Reachface$f13.34total
Reachface$percent13.34 <- Reachface$total13.34/Reachface$total

# Plot Facebook data
ggplot(Reachface,aes(x=n.week,y=f13.34total)) +
  geom_point(size=3,color="blue") +
  theme(axis.text.x = element_text(hjust = 1, size=13)) +
  theme(axis.text.y = element_text(hjust = 1, size=13)) +
  stat_smooth(method="lm",fullrange=TRUE) +
  labs(title = "Weekly Female 13-34 Reach - October '13 - June '14",x="Week",y="Number of Females Aged 13-34")

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

# Export data after creating new variables
write.csv(Reachface,"Reachface.csv")
