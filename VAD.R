rm(list=ls())
library(ggplot2)
library(gridExtra)
library(hrbrthemes)
library(dplyr)
library(cowplot)
library(ggpubr)
library(caTools)
library(randomForest)


setwd("C:\\Users\\mladi\\Desktop\\MASTER_2\\Bio\\Final project")

Vad = read.csv("VascularAccessData.csv")


str(Vad)
summary(Vad$id)
summary(Vad$Gender)
summary(Vad$Age_years)
summary(Vad$AverageBloodFlow)
summary(Vad$HYPERTENSION)
summary(Vad$DIABETES)
summary(Vad$MALIGNANCY)
summary(Vad$PERIPHERAL_VASCULAR_DISEASE)
summary(Vad$HOPB)
summary(Vad$VALVULAR_HEART_DISEASE)
summary(Vad$CARDIOMYOPATHY)
summary(Vad$HYPOTHYREOSIS)
summary(Vad$HYPERTHREOSIS)
summary(Vad$STROKE)
summary(Vad$HEART_ATTACK)
summary(Vad$timeOnStudy)
summary(Vad$Achieved_VA)
summary(Vad$AchievedParticular_VA)
summary(Vad$totalTimeSpentOnTempCath_days)
summary(Vad$totalTimeSpentOnAVF_days)
summary(Vad$nrTempCaths)
summary(Vad$nrAVFs)
summary(Vad$nrTunCaths)


#Removing 11 patient from current dataset
Vad<-Vad[!is.na(Vad$HYPERTENSION),]

#Removing 11 patient from current dataset
Vad<-Vad[!is.na(Vad$HOPB),]


Vad<-Vad[Vad$AchievedParticular_VA !="Tunneled Catheter", ]  



#Creating dataset with only AVF
AVF<-Vad[Vad$AchievedParticular_VA !="Temporary Catheter" & Vad$AchievedParticular_VA !="Tunneled Catheter", ]  

#Creating dataset with only Temporary Catheter
Temporary_Catheter<-Vad[Vad$AchievedParticular_VA !="AVF" & Vad$AchievedParticular_VA !="Tunneled Catheter", ]  

#Creating dataset with only Tunneled Catheter
Tunneled_Catheter<-Vad[Vad$AchievedParticular_VA !="AVF" & Vad$AchievedParticular_VA !="Temporary Catheter", ]  



#-----------------------Analysis---------------------------

#Pie chart of Genders 
ggplot(Vad, aes(x="", y="", fill=Gender)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels

#percentage
summary(Vad$Gender)
prop.table(table(Vad$Gender))
summary(Vad$AchievedParticular_VA)




bar <- ggplot(data = Vad) + 
  geom_bar(
    mapping = aes(x = AchievedParticular_VA, fill = AchievedParticular_VA), 
    show.legend = FALSE,
    width = 0.75
  ) + 
  theme(aspect.ratio = 0.5) +
  labs(x = NULL, y = NULL)+
  scale_y_continuous(name = "Number Of patient",
                        limits = c(0, 225),
                        breaks = seq(0, 225, by = 25))

bar + coord_flip()


ggplot(data = Vad) + 
  geom_bar(mapping = aes(x = AchievedParticular_VA, fill = Gender), position = "dodge")+
  scale_y_continuous(name = "Number Of patient")+ xlab("Type of vascular access that patient recived")




table(Vad$HYPERTENSION)
prop.table(table(Vad$HYPERTENSION))

table(Vad$DIABETES)
prop.table(table(Vad$DIABETES))


table(Vad$MALIGNANCY)
prop.table(table(Vad$MALIGNANCY))

table(Vad$PERIPHERAL_VASCULAR_DISEASE)
prop.table(table(Vad$PERIPHERAL_VASCULAR_DISEASE))

table(Vad$HOPB)
prop.table(table(Vad$HOPB))

table(Vad$VALVULAR_HEART_DISEASE)
prop.table(table(Vad$VALVULAR_HEART_DISEASE))

table(Vad$CARDIOMYOPATHY)
prop.table(table(Vad$CARDIOMYOPATHY))

table(Vad$HYPERTHREOSIS)
prop.table(table(Vad$HYPERTHREOSIS))

table(Vad$HYPOTHYREOSIS)
prop.table(table(Vad$HYPOTHYREOSIS))

table(Vad$STROKE)
prop.table(table(Vad$STROKE))

table(Vad$HEART_ATTACK)
prop.table(table(Vad$HEART_ATTACK))

prop.table(table(Vad$Gender, Vad$AchievedParticular_VA))


#----------------Female---------------------

Female<-Vad[Vad$Gender !="M", ]

summary(Female$Age_years)
sd(Female$Age_years)
plot(Female$Age_years)


ggplot(Female, aes(x = Age_years, fill = Age_years)) + 
  geom_bar()+
  xlab("Year") + ylab("")+
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(name = "Count of Female pati",
                     limits = c(0, 8),
                     breaks = seq(0, 8, by = 1))+
  scale_x_continuous(name = "Age",
                     limits = c(20, 82),
                     breaks = seq(20, 82, by = 2))


ggplot(Female, aes(x=Age_years)) +
    geom_histogram( binwidth=3, fill="#C10808", color="#C10808", alpha=0.9) +
    ggtitle("Female patient") +
    theme(plot.title = element_text(size=10))+
  scale_y_continuous(name = "Count of Female patient",
                     limits = c(0, 20),
                     breaks = seq(0, 20, by = 1))+
  scale_x_continuous(name = "Age",
                     limits = c(20, 86),
                     breaks = seq(20, 86, by = 2))+theme_minimal() 

Female <- Female[order(Female$Age_years),]




summary(Female$Age_years)


ggplot(data = Female) + 
  geom_bar(mapping = aes(x = AchievedParticular_VA, fill = Gender), position = "dodge")+
  scale_y_continuous(name = "Number Of patient")+ xlab("Type of vascular access that patient recived")


table(Female$AchievedParticular_VA)
prop.table(table(Female$AchievedParticular_VA))


Female$Age_years
Female$timeOnStudy



ggplot(Female, aes(x="", y="", fill=AchievedParticular_VA)) +
  ggtitle("Temporary and permanent access
to the vascular system")+
  labs(fill = "Three types of access:")+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()


summary(Female$timeOnStudy)
plot()  

ggplot(Female, aes(x=Age_years, y=timeOnStudy, color=AchievedParticular_VA)) +
  labs(color = "Three types of access:")+
  scale_x_continuous(name = "Age",
                     limits = c(20, 90),
                     breaks = seq(20, 90, by = 5))+
  scale_y_continuous(name = "Time on Study",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, by = 250))+
  geom_point(size=4)+theme_minimal() 



#Creating dataset with only AVF
FemaleAVF<-Female[Female$AchievedParticular_VA !="Temporary Catheter" & Female$AchievedParticular_VA !="Tunneled Catheter", ]  

#Creating dataset with only Temporary Catheter
FemaleTemporary_Catheter<-Female[Female$AchievedParticular_VA !="AVF" & Female$AchievedParticular_VA !="Tunneled Catheter", ]  

#Creating dataset with only Tunneled Catheter
FemaleTunneled_Catheter<-Female[Female$AchievedParticular_VA !="AVF" & Female$AchievedParticular_VA !="Temporary Catheter", ]  



summary(FemaleAVF$Age_years)
sd(FemaleAVF$Age_years)

summary(FemaleTemporary_Catheter$Age_years)
sd(FemaleTemporary_Catheter$Age_years)
   
summary(FemaleTunneled_Catheter$Age_years)
sd(FemaleTunneled_Catheter$Age_years)



summary(FemaleAVF$timeOnStudy)
sd(FemaleAVF$timeOnStudy)

summary(FemaleTemporary_Catheter$timeOnStudy)
sd(FemaleTemporary_Catheter$timeOnStudy)

summary(FemaleTunneled_Catheter$timeOnStudy)
sd(FemaleTunneled_Catheter$timeOnStudy)


#-------------Male-----------------

Male<-Vad[Vad$Gender !="F", ]


summary(Male$Age_years)
sd(Male$Age_years)
plot(Male$Age_years)


ggplot(Male, aes(x=Age_years)) +
  geom_histogram( binwidth=3, fill="#0056C7", color="#0056C7", alpha=0.9) +
  ggtitle("Male patient") +
  theme(plot.title = element_text(size=10))+
  scale_y_continuous(name = "Count of Male patient",
                     limits = c(0, 22),
                     breaks = seq(0, 22, by = 1))+
  scale_x_continuous(name = "Age",
                     limits = c(10, 90),
                     breaks = seq(10, 90, by = 2))+theme_minimal() 


summary(Male$Age_years)
sd(Male$Age_years)


ggplot(data = Male) + 
  geom_bar(mapping = aes(x = AchievedParticular_VA, fill = Gender), position = "dodge")+
  scale_y_continuous(name = "Number Of patient")+ xlab("Type of vascular access that patient recived")


table(Male$AchievedParticular_VA)
prop.table(table(Male$AchievedParticular_VA))


Male$Age_years
Male$timeOnStudy



ggplot(Male, aes(x="", y="", fill=AchievedParticular_VA)) +
  ggtitle("Temporary and permanent access
to the vascular system")+
  labs(fill = "Three types of access:")+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()


summary(Male$timeOnStudy)
plot()  

ggplot(Male, aes(x=Age_years, y=timeOnStudy, color=AchievedParticular_VA)) +
  labs(color = "Three types of access:")+
  scale_x_continuous(name = "Age",
                     limits = c(20, 90),
                     breaks = seq(20, 90, by = 5))+
  scale_y_continuous(name = "Time on Study",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, by = 250))+
  geom_point(size=4)+theme_minimal() 

summary(Male$timeOnStudy)


#Creating dataset with only AVF
MaleAVF<-Male[Male$AchievedParticular_VA !="Temporary Catheter" & Male$AchievedParticular_VA !="Tunneled Catheter", ]  

#Creating dataset with only Temporary Catheter
MaleTemporary_Catheter<-Male[Male$AchievedParticular_VA !="AVF" & Male$AchievedParticular_VA !="Tunneled Catheter", ]  

#Creating dataset with only Tunneled Catheter
MaleTunneled_Catheter<-Male[Male$AchievedParticular_VA !="AVF" & Male$AchievedParticular_VA !="Temporary Catheter", ]  



summary(MaleAVF$Age_years)
sd(MaleAVF$Age_years)

summary(MaleTemporary_Catheter$Age_years)
sd(MaleTemporary_Catheter$Age_years)

summary(MaleTunneled_Catheter$Age_years)
sd(MaleTunneled_Catheter$Age_years)



summary(MaleAVF$timeOnStudy)
sd(MaleAVF$timeOnStudy)

summary(MaleTemporary_Catheter$timeOnStudy)
sd(MaleTemporary_Catheter$timeOnStudy)

summary(MaleTunneled_Catheter$timeOnStudy)
sd(MaleTunneled_Catheter$timeOnStudy)

#-------------Mean,sd,95% intervals-------------------



Vad$AverageBloodFlow<-as.double(Vad$AverageBloodFlow) 

Vad <- Vad%>%
  dplyr::mutate(Bloodflow = if_else(
    is.na(AverageBloodFlow)
    , mean(AverageBloodFlow, na.rm = TRUE)
    , AverageBloodFlow
  )
  )
Vad<-Vad[-c(4)]
Vad<-Vad[-c(20,23)]


ggplot(Vad, aes(x=Bloodflow, y=timeOnStudy, color=AchievedParticular_VA)) +
  labs(color = "Two types of access:")+
  scale_y_continuous(name = "Time on Study",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, by = 250))+
  geom_point(size=4)+theme_minimal() 


summary(Vad$timeOnStudy)

Vad$AverageBloodFlow<-as.double(Vad$AverageBloodFlow)


AVFBloodFlow <- AVF%>%
  dplyr::group_by(Gender)%>%
  dplyr::summarise(GenderMean = MeanCI(Bloodflow,method=c("classic"))[1]
                   ,lowerCi= MeanCI(Bloodflow,method=c("classic"))[2]
                   ,upperCI= MeanCI(Bloodflow,method=c("classic"))[3])
 

p1<-ggplot(data=AVFBloodFlow)
p1<-p1+geom_point(aes(x=Gender,y=GenderMean),size=5,color="blue")
p1<-p1+geom_errorbar(aes(x=Gender,y=GenderMean, ymin=lowerCi, ymax=upperCI),width=.1,color="red")
p1<-p1+geom_text(aes(x=Gender,y=lowerCi,label=round(lowerCi,2)),hjust= - 1 )
p1<-p1+geom_text(aes(x=Gender,y=upperCI,label=round(upperCI,2)),hjust= - 1 )
p1<-p1+geom_text(aes(x=Gender,y=GenderMean,label=round(GenderMean,2)),hjust= - 0.5 )
p1<-p1+labs(x="Gender",y="mean")
p1<-p1+labs(title="AVF group")
p1<-p1+theme_classic()
p1


Temporary_Catheter


Temporary_CatheterBloodFlow <- Temporary_Catheter%>%
  dplyr::group_by(Gender)%>%
  dplyr::summarise(GenderMean = MeanCI(Bloodflow,method=c("classic"))[1]
                   ,lowerCi= MeanCI(Bloodflow,method=c("classic"))[2]
                   ,upperCI= MeanCI(Bloodflow,method=c("classic"))[3])


p2<-ggplot(data=Temporary_CatheterBloodFlow)
p2<-p2+geom_point(aes(x=Gender,y=GenderMean),size=5,color="blue")
p2<-p2+geom_errorbar(aes(x=Gender,y=GenderMean, ymin=lowerCi, ymax=upperCI),width=.1,color="red")
p2<-p2+geom_text(aes(x=Gender,y=lowerCi,label=round(lowerCi,2)),hjust= - 1 )
p2<-p2+geom_text(aes(x=Gender,y=upperCI,label=round(upperCI,2)),hjust= - 1 )
p2<-p2+geom_text(aes(x=Gender,y=GenderMean,label=round(GenderMean,2)),hjust= - 0.5 )
p2<-p2+labs(x="Gender",y="mean")
p2<-p2+labs(title="Temporary Catheter group")
p2<-p2+theme_classic()
p2



plot_grid(p1, p2, labels = "AUTO")





VadAgeYears <- Vad%>%
  dplyr::group_by(AchievedParticular_VA)%>%
  dplyr::summarise(Mean = MeanCI(Age_years,method=c("classic"))[1]
                   ,lowerCi= MeanCI(Age_years,method=c("classic"))[2]
                   ,upperCI= MeanCI(Age_years,method=c("classic"))[3])

sd(AVF$Age_years)
sd(Temporary_Catheter$Age_years)


Vad$Bloodflow

VadBloodFlow <- Vad%>%
  dplyr::group_by(AchievedParticular_VA)%>%
  dplyr::summarise(Mean = MeanCI(Bloodflow,method=c("classic"))[1]
                   ,lowerCi= MeanCI(Bloodflow,method=c("classic"))[2]
                   ,upperCI= MeanCI(Bloodflow,method=c("classic"))[3])

sd(AVF$Bloodflow)
sd(Temporary_Catheter$Bloodflow)



Vad$timeOnStudy

VadTimeOnStudy <- Vad%>%
  dplyr::group_by(AchievedParticular_VA)%>%
  dplyr::summarise(Mean = MeanCI(timeOnStudy,method=c("classic"))[1]
                   ,lowerCi= MeanCI(timeOnStudy,method=c("classic"))[2]
                   ,upperCI= MeanCI(timeOnStudy,method=c("classic"))[3])

sd(AVF$timeOnStudy)
sd(Temporary_Catheter$timeOnStudy)


t.test(Vad$totalTimeSpentOnAVF_days)
sd(Vad$totalTimeSpentOnAVF_days)

t.test(Vad$nrAVFs)
sd(Vad$nrAVFs)


t.test(Temporary_Catheter$totalTimeSpentOnTempCath_days)
sd(Temporary_Catheter$totalTimeSpentOnTempCath_days)

t.test(Temporary_Catheter$nrTempCaths)
sd(Temporary_Catheter$nrTempCaths)


table(AVF$HYPERTENSION)
prop.table(table(AVF$HYPERTENSION))

table(Temporary_Catheter$HYPERTENSION)
prop.table(table(Temporary_Catheter$HYPERTENSION))

Vad$DIABETES

table(AVF$DIABETES)
prop.table(table(AVF$DIABETES))

table(Temporary_Catheter$DIABETES)
prop.table(table(Temporary_Catheter$DIABETES))


Vad$MALIGNANCY

table(AVF$MALIGNANCY)
prop.table(table(AVF$MALIGNANCY))

table(Temporary_Catheter$MALIGNANCY)
prop.table(table(Temporary_Catheter$MALIGNANCY))


Vad$PERIPHERAL_VASCULAR_DISEASE

table(AVF$PERIPHERAL_VASCULAR_DISEASE)
prop.table(table(AVF$PERIPHERAL_VASCULAR_DISEASE))

table(Temporary_Catheter$PERIPHERAL_VASCULAR_DISEASE)
prop.table(table(Temporary_Catheter$PERIPHERAL_VASCULAR_DISEASE))

table(AVF$HOPB)
prop.table(table(AVF$HOPB))

table(Temporary_Catheter$HOPB)
prop.table(table(Temporary_Catheter$HOPB))

AVF$VALVULAR_HEART_DISEASE

table(AVF$VALVULAR_HEART_DISEASE)
prop.table(table(AVF$VALVULAR_HEART_DISEASE))

table(Temporary_Catheter$VALVULAR_HEART_DISEASE)
prop.table(table(Temporary_Catheter$VALVULAR_HEART_DISEASE))


AVF$CARDIOMYOPATHY

table(AVF$CARDIOMYOPATHY)
prop.table(table(AVF$CARDIOMYOPATHY))

table(Temporary_Catheter$CARDIOMYOPATHY)
prop.table(table(Temporary_Catheter$CARDIOMYOPATHY))


AVF$HYPOTHYREOSIS

table(AVF$HYPOTHYREOSIS)
prop.table(table(AVF$HYPOTHYREOSIS))

table(Temporary_Catheter$HYPOTHYREOSIS)
prop.table(table(Temporary_Catheter$HYPOTHYREOSIS))


AVF$HYPERTHREOSIS

table(AVF$HYPERTHREOSIS)
prop.table(table(AVF$HYPERTHREOSIS))

table(Temporary_Catheter$HYPERTHREOSIS)
prop.table(table(Temporary_Catheter$HYPERTHREOSIS))

table(AVF$STROKE)
prop.table(table(AVF$STROKE))

table(Temporary_Catheter$STROKE)
prop.table(table(Temporary_Catheter$STROKE))

table(AVF$HEART_ATTACK)
prop.table(table(AVF$HEART_ATTACK))

table(Temporary_Catheter$HEART_ATTACK)
prop.table(table(Temporary_Catheter$HEART_ATTACK))




#---------------------Two sample t Test-----------------------#

install.packages("ggpubr")
library(ggpubr)


#-----Age--------

t.test(Vad$Age_years~Vad$AchievedParticular_VA ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)

compare_means(Age_years ~ AchievedParticular_VA,  data = Vad, ref.group = ".all.",
              method = "t.test")

Ag1<-ggboxplot(Vad, x = "AchievedParticular_VA", y = "Age_years",
          color="AchievedParticular_VA",palette = "jco",
          add = "jitter")+ stat_compare_means(method = "t.test")      


Ag2<-ggboxplot(Vad, x = "AchievedParticular_VA", y = "Age_years",
          color = "AchievedParticular_VA", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.",label.y = 40) 




plot_grid(Ag1, Ag2, labels = "AUTO")

#-----Blood Flow--------

t.test(Vad$Bloodflow~Vad$AchievedParticular_VA ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)

compare_means(Bloodflow ~ AchievedParticular_VA,  data = Vad, ref.group = ".all.",
              method = "t.test")

Bf1<-ggboxplot(Vad, x = "AchievedParticular_VA", y = "Bloodflow",
          color="AchievedParticular_VA",palette = "jco",
          add = "jitter")+ stat_compare_means(method = "t.test")      


Bf2<-ggboxplot(Vad, x = "AchievedParticular_VA", y = "Bloodflow",
          color = "AchievedParticular_VA", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.",label.y = 40) 


plot_grid(Bf1, Bf2, labels = "AUTO")

#----Time on study--------



t.test(Vad$timeOnStudy~Vad$AchievedParticular_VA ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)

compare_means(timeOnStudy ~ AchievedParticular_VA,  data = Vad, ref.group = ".all.",
              method = "t.test")

Ts1<-ggboxplot(Vad, x = "AchievedParticular_VA", y = "timeOnStudy",
          color="AchievedParticular_VA",palette = "jco",
          add = "jitter")+ stat_compare_means(method = "t.test")      


Ts2<-ggboxplot(Vad, x = "AchievedParticular_VA", y = "timeOnStudy",
          color = "AchievedParticular_VA", palette = "jco")+ 
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.",label.y = 40) 



plot_grid(Ts1, Ts2, labels = "AUTO")

t.test(Vad$totalTimeSpentOnTempCath_days~Vad$AchievedParticular_VA ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(Vad$totalTimeSpentOnAVF_days~Vad$AchievedParticular_VA ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(Vad$nrTempCaths~Vad$AchievedParticular_VA ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(Vad$nrAVFs~Vad$AchievedParticular_VA ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)


compare_means(totalTimeSpentOnTempCath_days ~ AchievedParticular_VA,  data = Vad, ref.group = ".all.",
              method = "t.test")

compare_means(totalTimeSpentOnAVF_days ~ AchievedParticular_VA,  data = Vad, ref.group = ".all.",
              method = "t.test")

compare_means(nrTempCaths ~ AchievedParticular_VA,  data = Vad, ref.group = ".all.",
              method = "t.test")

compare_means(nrAVFs ~ AchievedParticular_VA,  data = Vad, ref.group = ".all.",
              method = "t.test")


ggboxplot(Vad, x = "AchievedParticular_VA", y = "Age_years",
          color="AchievedParticular_VA",palette = "jco",
          add = "jitter")+ stat_compare_means(method = "t.test")      



ggboxplot(Vad, x = "AchievedParticular_VA", y = "Age_years",
          color = "AchievedParticular_VA", palette = "jco")+  
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.",label.y = 40) 


#_------------ Logistic Regression --------------

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(Vad$Age_years, SplitRatio = 0.65)

# Split up the data using subset
Vadtrain = subset(Vad, split==TRUE)
Vadtest = subset(Vad, split==FALSE)

Vadtrain$Age_years
Vadtrain$Gender
Vadtrain$AchievedParticular_VA
Vadtrain$timeOnStudy
Vadtrain$Bloodflow
Vadtrain$totalTimeSpentOnTempCath_days
Vadtrain$totalTimeSpentOnAVF_days
Vadtrain$nrAVFs
Vadtrain$nrTempCaths
Vadtrain$HYPERTENSION
Vadtrain$DIABETES
Vadtrain$VALVULAR_HEART_DISEASE
Vadtrain$HYPERTHREOSIS


Vadtrain$totalTimeSpentOnTempCath_days<-as.factor(Vadtrain$totalTimeSpentOnTempCath_days) 
Vadtrain$totalTimeSpentOnAVF_days<-as.factor(Vadtrain$totalTimeSpentOnAVF_days) 
Vadtrain$nrAVFs<-as.factor(Vadtrain$nrAVFs) 
Vadtrain$nrTempCaths<-as.factor(Vadtrain$nrTempCaths) 
AchievedParticular_VA

LR_Model_1 = glm(AchievedParticular_VA ~Gender+Age_years+timeOnStudy+Bloodflow+HYPERTENSION+DIABETES+
                   MALIGNANCY+PERIPHERAL_VASCULAR_DISEASE+HOPB+VALVULAR_HEART_DISEASE+CARDIOMYOPATHY+HYPOTHYREOSIS+
                   HYPERTHREOSIS+STROKE+HEART_ATTACK, data = Vadtrain, family = binomial("logit"), maxit = 100)
summary(LR_Model_1)


plot(LR_Model_1)

LR_Model_2 = glm(AchievedParticular_VA ~ Gender+Age_years+timeOnStudy+Bloodflow, data = Vadtrain, family=binomial)
summary(LR_Model_2)


#-----------------Random Forests ----------------


Vad$Gender

test1<-rfImpute(Gender~.,data=Vad,iter=6)
Rf1<-randomForest(Gender ~ ., data = test1,proximity=TRUE)

ob.error.data <- data.frame(
  Trees=rep(1:nrow(Rf$err.rate), times=3),
  Type=rep(c("OOB", "M", "F"), each=nrow(Rf$err.rate)),
  Error=c(Rf$err.rate[,"OOB"], 
          Rf$err.rate[,"M"],
          Rf$err.rate[,"F"]))

ggplot(data=ob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))+theme_classic()


