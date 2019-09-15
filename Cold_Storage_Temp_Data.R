
##Installing packages for plotting
##install.packages("ggplot2")
##library(ggplot2)

##Set the path of the Data file
setwd("C:/test/Project")
getwd()

##Reading the input file
Temp_Storage=read.csv("Cold_Storage_Temp_Data.csv",header=TRUE)
Temp_Storage

#datatype for each variable
str(Temp_Storage)
## 5 point summary
summary(Temp_Storage)
#Check any NULL or NA value in the dataset
sum(is.na(Temp_Storage))
#by attaching you can call variables directly (you could avoid using $)
attach(Temp_Storage)


#Find mean cold storage temperature for Summer, Winter and Rainy Season

# Subset of data having season as Summer
SummerDS= subset(Temp_Storage[which(Season=="Summer"),])
#Show the first 6 lines
head(SummerDS)
#Show the last 6 lines 
tail(SummerDS)

# Subset of data having season as Winter
WinterDS= subset(Temp_Storage[which(Season=="Winter"),])


# Subset of data having season as Rainy
RainDS= subset(Temp_Storage[which(Season=="Rainy"),])

#mean cold storage temperature for Summer
SummerDS_Mean=mean(SummerDS$Temperature)
SummerDS_Mean
## Univariate Analysis
boxplot(SummerDS$Temperature,main="Boxplot of Summer Temperature",col="green",horizontal = TRUE,xlab="Summer Temperature")
##ggplot(SummerDS,aes(x=1,y=SummerDS$Temperature,fill=SummerDS$Temperature))+ geom_boxplot(fill=c("green"))  +xlab("Frequency") +ylab("Summer Temperature") + ggtitle("Mean of Summer Temperature")

#mean cold storage temperature for Winter
WinterDS_Mean = mean(WinterDS$Temperature)
WinterDS_Mean
## Univariate Analysis
boxplot(WinterDS$Temperature,main="Boxplot of Winter Temperature",col="yellow",horizontal = TRUE,xlab="Winter Temperature")
##ggplot(WinterDS,aes(x=1,y=WinterDS$Temperature,fill=WinterDS$Temperature))+ geom_boxplot(fill=c("yellow"))  +xlab("Frequency") +ylab("Winter Temperature") + ggtitle("Boxplot of Winter Temperature")##outliers Winter
boxplot(WinterDS$Temperature,main="Boxplot of Winter Temperature",col="yellow",horizontal = TRUE,xlab="Winter Temperature",outcol="red")$out
#mean cold storage temperature for Rainy
RainDS_Mean = mean(RainDS$Temperature)
RainDS_Mean
## Univariate Analysis
boxplot(RainDS$Temperature,main="Boxplot of Rainy Temperature",col="red",horizontal = TRUE,xlab="Rainy Temperature")
##ggplot(RainDS,aes(x=1,y=RainDS$Temperature,fill=RainDS$Temperature))+ geom_boxplot(fill=c("red"))  +xlab("Frequency") +ylab("Rainy Temperature") + ggtitle("Boxplot of Rainy Temperature")

##Bivariate Analysis

boxplot(SummerDS$Temperature ~ SummerDS$Season,main="Boxplot of Temperature vs Summer Season",col="green",horizontal = TRUE,xlab="Temperature",ylab="Season")
boxplot(WinterDS$Temperature ~ WinterDS$Season,main="Boxplot of Temperature vs Winter Season",col="red",horizontal = TRUE,xlab="Temperature",ylab="Season")
boxplot(RainDS$Temperature ~ RainDS$Season,main="Boxplot of Temperature vs Rainy Season",col="blue",horizontal = TRUE,xlab="Temperature",ylab="Season")

##outliers Rainy
boxplot(RainDS$Temperature,main="Boxplot of Rainy Temperature",col="red",horizontal = TRUE,xlab="Rainy Temperature",outcol="green")$out

#overall mean for the full year
Temp_Storage_Mean=mean(Temperature)
Temp_Storage_Mean
## Univariate Analysis
boxplot(Temperature,main="Boxplot of full year Temperature",col="blue",horizontal = TRUE,xlab="Year Temperature")

#Standard Deviation for the full year
Temp_Storage_SD=sd(Temperature)
Temp_Storage_SD

##Assume Normal distribution, what is the probability of temperature having fallen below 2 deg C? 

x=2
mu=2.96274
sd=0.508589

pnorm(x,mu,sd,lower.tail = TRUE)#0.02918142


###Assume Normal distribution, what is the probability of temperature having gone above 4 deg C? 
x1=4
mu=2.96274
sd=0.508589

pnorm(x1,mu,sd,lower.tail = FALSE)#0.02070079

##########################################################
#Problem 2
##########################################################

getwd()

#Reading the Data File
March_Storage=read.csv("Cold_Storage_Mar2018.csv",header=TRUE)
#datatype for each variable
str(March_Storage)
### 5 point summary
summary(March_Storage)
#Check any NULL or NA value in the dataset
sum(is.na(March_Storage))
#Find mean of Temperature of MARCH 2018 Dataset
Xbar=mean(March_Storage$Temperature)
Xbar
# Std. Deviation from MARCH 2018 Dataset
sd_MarchTemp = sd(March_Storage$Temperature)
sd_MarchTemp

#1. State the Hypothesis, do the calculation using z test
## Null Hypothesis HO = 3.9 and Alternative Hypothesis is HA >3.9
#Number of Samples
N=35
Mu=3.9
##Temp_storage_SD is the sd of the full year data

ZResult=(Xbar-Mu)/(Temp_Storage_SD/(N^0.5))
ZResult

#Pvalue = (1-pt(ZResult,34))*2

Pvalue=pnorm(-abs(ZResult))
Pvalue
##Above P is higher than alpha, hence NULL Hypothesis is failed to reject.

##2.State the Hypothesis, do the calculation using t-test
###T Test
## Null Hypothesis HO = 3.9 and Alternative Hypothesis is HA >3.9
??t.test
Tvalue= t.test(March_Storage$Temperature,NULL,Xbar,alternative="greater",paired=FALSE,conf.level =0.1)
Tvalue

#P-value is 0.5 which is higher than alpha , hence NULL Hypothesis failed to reject.
