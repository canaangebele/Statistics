Mass_Shooting6 = read.csv(file.choose())

#Clean dataset

library(ggplot2)
library(dplyr)
library(MASS)
Mass_Shooting6 = filter(Mass_Shooting6, Shooter.Name != "Unknown")
Mass_Shooting6 = filter(Mass_Shooting6, Shooter.Age.s. != "Unknown")
Mass_Shooting6 = filter(Mass_Shooting6, Average.Shooter.Age != "Unknown")
Mass_Shooting6 = filter(Mass_Shooting6, Type.of.Gun...Detailed != "Unknown")
Mass_Shooting6 = filter(Mass_Shooting6, Type.of.Gun...General != "Unknown")
Mass_Shooting6 = filter(Mass_Shooting6, Total.Number.of.Guns != "Unknown")

#Subset specific instances of high profile event 

Pre = filter(Mass_Shooting, Year > 1966 & Year < 1999)
Columbine = filter(Mass_Shooting, Year > 1999)
summary(Pre)
summary(Columbine)

VT_2007 = filter(Mass_Shooting, Year == 2007) 
Sandy_2013 = filter(Mass_Shooting, Year == 2013)
Columbine_1999 = filter(Mass_Shooting, Year == 1999)
Texas_1991 = filter(Mass_Shooting, Year == 1991)
Cali_1984 = filter(Mass_Shooting, Year == 1984)

Mass_Shooting1 = filter(Mass_Shooting, Total.Number.of.Fatalities > 4)
Mass_Shooting2 = filter(Mass_Shooting, Total.Number.of.Victims >= 4)

summary(Mass_Shooting)

#Create model with predictors

model1 = lm(Total.Number.of.Fatalities~Total.Number.of.Guns+
              Fate.of.Shooter.at.the.scene+Shooter.Race+
              Number.of.Victims.Injured, Mass_Shooting)
summary(model1)

model5 = lm(Total.Number.of.Fatalities~Number.of.Victims.Injured+
              Total.Number.of.Guns+Fate.of.Shooter.at.the.scene, Mass_Shooting)
summary(model5)

#Corning Cali Elementary school

predict(model1,data.frame(Number.of.Victims.Injured = 12,
                          Total.Number.of.Guns = "4",
                          Fate.of.Shooter.at.the.scene = "Killed",
                          Shooter.Race = "White American or European American"), 
        interval = 'prediction',level = .95)
        
#Dallas shootings

predict(model1,data.frame(Number.of.Victims.Injured = 9,
                          Total.Number.of.Guns = "3",
                          Fate.of.Shooter.at.the.scene = "Killed",
                          Shooter.Race = "Black American or African American"), 
        interval = 'prediction',level = .95)

model2 = lm(Total.Number.of.Fatalities~Fate.of.Shooter.at.the.scene, Mass_Shooting)
summary(model2)

model3 = lm(Total.Number.of.Fatalities~Place.Type, Mass_Shooting)
summary(model3)

#View Distributions

hist(Mass_Shooting$Year)

table(Mass_Shooting$Year)

DPM_2012<-Year_2012%>%group_by(Month)%>%summarise(DPM_12=sum(Total.Number.of.Fatalities))
DPM_2013<-Year_2013%>%group_by(Month)%>%summarise(DPM_13=sum(Total.Number.of.Fatalities))
DPM_2014<-Year_2013%>%group_by(Month)%>%summarise(DPM_14=sum(Total.Number.of.Fatalities))
DPM_2015<-Year_2015%>%group_by(Month)%>%summarise(DPM_15=sum(Total.Number.of.Fatalities))
DPM_2016<-Year_2016%>%group_by(Month)%>%summarise(DPM_16=sum(Total.Number.of.Fatalities))

Year_2012 = filter(Mass_Shooting, Year == 2012)
Year_2013 = filter(Mass_Shooting, Year == 2013)
Year_2014 = filter(Mass_Shooting, Year == 2014)
Year_2015 = filter(Mass_Shooting, Year == 2015)
Year_2016 = filter(Mass_Shooting, Year == 2016)

qplot(Year_2015$Month,
      geom="histogram",
      binwidth = 1,  
      main = "Number of Incidents in 2015 per month", 
      xlab = "Month",  
      fill=I("red"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(-0.01,12.5),
      ylim=c(0,15))

qplot(DPM_2012$DPM_12,
      geom="histogram",
      binwidth = 1,  
      main = "Number of Incidents in 2015 per month", 
      xlab = "Month",  
      fill=I("red"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(-0.01,12.5),
      ylim=c(0,35))
      
#ANOVA and Tukey

AOV_Model = aov(Total.Number.of.Fatalities~Total.Number.of.Guns, data=Mass_Shooting)
summary(AOV_Model)

TukeyHSD(AOV_Model)

#Step function

step = stepAIC(lm(Total.Number.of.Fatalities~1,data=Mass_Shooting),
               scope=~State+Number.of.Victims.Injured+
                 Month+Year+Day.of.Week+Average.Shooter.Age+
                 Shooter.Sex+Shooter.Race+Type.of.Gun...General+
                 Total.Number.of.Guns+Fate.of.Shooter.at.the.scene+
                 School.Related+Place.Type+History.of.Mental.Illness...General,
               direction="both")
               
#Display summarized results             
               
step$anova 
summary(step)

ggplot(Mass_Shooting,aes(x=,y=response_times))+
  geom_boxplot()

write.csv(Sandy_2013, file="Sandy_2013.csv")
write.csv(Columbine_1999, file="Columbine_1999.csv")
write.csv(VT_2007, file="VT_2007.csv")
write.csv(Cali_1984, file="Cali_1984.csv")
write.csv(Texas_1991, file="Texas_1991.csv")
write.csv(Sandy_2012, file="Sandy_2012.csv")

Columbine$School.Related <- sub("^", "Post", Columbine$School.Related)
Pre$School.Related <- sub("^", "Pre", Pre$School.Related)
School <- rbind(Pre,Columbine)

AOV_Model1 = aov(Year~School.Related, data=School)
summary(AOV_Model1)
TukeyHSD(AOV_Model1)

AOV_Model2 = aov(Year~School.Related, data=Mass_Shooting)
TukeyHSD(AOV_Model2)

School_related = Mass_Shooting[30]

School_related = sum(School_related = "Yes")

