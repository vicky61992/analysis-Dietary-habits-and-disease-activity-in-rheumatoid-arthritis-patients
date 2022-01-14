data <- load("C:/Users/marti/OneDrive - Università degli Studi di Milano/UNIVERSITA/DSE/Labs/Nutritional Epidemiology/baseline_database_RA.Rdata")
data <- tot_bl_DSE

summary(data)
attach(data)
dim(data)
colnames(data)
str(data)
dim(data)

library(dplyr)
library("tidyverse")
library("descr")
library("ggplot2")
library("gmodels")
library("gridExtra")

palette(c("royalblue","cadetblue3","cornflowerblue", "lightskyblue", "steelblue3","dodgerblue", "steelblue1","dodgerblue2" ))

glimpse(data)

sapply(data,function(x)sum(is.na(x)))  
data<-data[complete.cases(data)]  
dim(data) 
#data <- na.omit(data, na.action ="omit", fill= NULL)


table(data$FR)/length(data$FR)*100 
plot(data$FR,xlab="FR",main="FR")


table(data$FR)/length(data$FR)
table(data$ETA_ESORDIO,data$FR) 
plot(data$ETA_ESORDIO,data$FR)

boxplot(data$FR,data$ETA_ESORDIO)
plot(data$CITTADINANZA,data$ETA_ESORDIO)
plot(data$SESSO,data$ETA_ESORDIO)

table(data$SESSO)
table(data$SESSO)/length(data$SESSO)*100
plot(data$SESSO)
plot(data$SESSO[data$FR=="1"])
table(data$SESSO,data$FR) #contingency table for GENDER
CrossTable(data$SESSO,data$FR,prop.chisq=FALSE) #crosstable

ggplot(data=data,aes(x=ETA_ESORDIO,fill=SESSO)) + geom_histogram(color= "navy") +    
  facet_wrap(. ~ SESSO) +                                                 
  theme(legend.title=element_blank(), legend.position="bottom") +     
  labs(x="Età Esordio",y="Frequency",title="Età esordio and gender")+
  theme_minimal()+
  scale_fill_manual(values=palette())

ggplot(data=data,aes(x=DURATA_MALATTIA,fill=SESSO)) + geom_histogram(color= "navy") +    
  facet_wrap(. ~ SESSO) +                                                 
  theme(legend.title=element_blank(), legend.position="bottom") +     
  labs(x="Durata Malattia",y="Frequency",title="Durata Malattia and Gender")+
  theme_minimal()+
  scale_fill_manual(values=palette())

ggplot(data=data,aes(x=BMI,fill=SESSO)) + geom_histogram(color= "navy") +    
  facet_wrap(. ~ SESSO) +                                                 
  theme(legend.title=element_blank(), legend.position="bottom") +     
  labs(x="BMI",y="Frequency",title="BMI and Gender")+
  theme_minimal()+
  scale_fill_manual(values=palette())

ggplot(data=data,aes(x=BMI,fill=Fattore_reumatoide)) + geom_histogram(color= "navy") +    
  facet_wrap(. ~ Fattore_reumatoide) +                                                 
  theme(legend.title=element_blank(), legend.position="bottom") +     
  labs(x="BMI",y="Frequency",title="BMI and FR")+
  theme_minimal()+
  scale_fill_manual(values=palette())

ggplot(data = data) + 
  geom_bar(mapping = aes(x = BMI, fill = FR), color= "navy") +
  theme_minimal() +
  scale_fill_manual(values=palette())

newdata <- data[c(2:90, 109)]

## Logistic 

Logit1<-glm(FR~.,data=newdata,family=binomial) 
summary(Logit1)

newdata <- newdata %>% mutate(pred.Logit1=ifelse(Logit1$fitted.values > 0.50, "0", "1"))
mat1<-table(newdata$FR,newdata$pred.Logit1)
mat1

FNON1 <- mat1[1,2]
FATT1 <- mat1[2,1]
TNON1 <- mat1[2,2]
TATT1 <- mat1[1,1]

accur1 <- (TNON1+TATT1)/(TNON1+TATT1+FNON1+FATT1)
accur1 #0.791
prec1 <- TNON1/(TNON1+FNON1)
prec1 #0.782
sensit1 <- TNON1/(TNON1+FATT1)
sensit1 #0.666
specif1 <- TATT1/(TATT1+FNON1)
specif1 #0.875

library(aod)

plot(FR~., data=newdata, col="red4")

library(corrplot)
corrplot(as.matrix(newdata), is.corr= FALSE, method="circle") #error for NA 
corrplot(newdata, method="circle")

