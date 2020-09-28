                                        #READ DATA
library(readr)
Taiwan <- read_table2("Taiwan defaults - data.txt",
                      col_types = cols(AGE = col_integer(), 
                                       BILL_AMT1 = col_number(), BILL_AMT2 = col_number(), 
                                       BILL_AMT3 = col_number(), BILL_AMT4 = col_number(), 
                                       BILL_AMT5 = col_number(), BILL_AMT6 = col_number(), 
                                       Default = col_factor(levels = c("0", 
                                                                       "1")), Gender = col_factor(levels = c("1", 
                                                                                                             "2")), LIMIT_BAL = col_number(), 
                                       MARRIAGE = col_factor(levels = c("1", 
                                                                        "2","3")), PAY_AMT1 = col_number(), 
                                       PAY_AMT2 = col_number(), PAY_AMT3 = col_number(), 
                                       PAY_AMT4 = col_number(), PAY_AMT5 = col_number(), 
                                       PAY_AMT6 = col_number()))
View(Taiwan)
head(Taiwan)



#eliminate the first column that represents the row number, not a variable:
Taiwan$ROW <- NULL
str(Taiwan)


#Look for the existence of NA:
sum(is.na(Taiwan))

#order dataset by integer,numeric and factor:
TW<-Taiwan[,c(4,1,5:16,2,3,17)]
str(TW)
View(TW)


                                    #STATISTICAL ANALYSIS

summary(TW)

# standard deviation of the variables:
apply(TW[,1:14],2,sd)

# mean:
apply(TW[,1:14],2,mean)

# median:
apply(TW[,1:14],2,median)

# Interquartile Amplitude:
apply(TW[,c(1:14)],2,function(x) quantile(x,0.75)-quantile(x,0.25))

# Variation coeficient:
VC <- function(x){sd(x)/mean(x)}
apply(TW[,c(1:14)],2,VC)



                                  #GRAPHICAL ANALYSIS

#ratio of default value graph
library(plotrix)

#Divide data by default variable value 

plot1 <- c(nrow(TW[TW$Default == 0,1:17]),
           nrow(TW[TW$Default == 1,1:17]))
plot1 <-as.matrix(plot1)


rate <- t(round(plot1/sum(plot1)*100,1))
colnames(rate)<-c(0,1)
names(rate) <-c(0,1)

label<-paste(names(rate),'\n','\n',rate,'%',sep="")
label

#Pie graph
pie3D(rate,explode=0.05,main="Pie Chart Default Values",
      labelcex = 1, radius =1.3,labels=label)

#bar graph
library(ggplot2)
ggplot(TW, aes(x = as.factor(Default))) + geom_bar() + theme_light() + 
        labs(title = "Default Credit Card Clients - target value - data unbalance\n (Default = 0, Not Default = 1)", x = "default.payment.next.month", y = "Values")

#Check data unbalance:

install.packages('ggplot2')
library(ggplot2)
ggplot(TW, aes(x = as.factor(Default))) + geom_bar() + theme_light() + 
        labs(title = "Default Credit Card Clients - target value - data unbalance\n (Default = 0, Not Default = 1)", x = "default.payment.next.month", y = "Values")

# Observe the influence of the variable Marriage on the dependent variable:
ggplot(TW, aes(x = as.factor(Default), fill = MARRIAGE)) + geom_bar() + theme_light() + 
        labs(title = "Default payment next month and Marriage Status", x = "Default payment next month", y = "Values")

# Observe the influence of the variable Gender on the dependent variable
ggplot(TW, aes(x = as.factor(Default), fill = Gender)) + geom_bar() + theme_light() + 
        labs(title = "Default payment next month and Gender", x = "Default payment next month", y = "Values")

#Observe the Age distribution of the people
hist(TW$AGE,main="Age",col="Blue",xlab="Clients age (in years)")

#Observe the distribution of Limit_Bal:
boxplot(TW$LIMIT_BAL,main="Box Plot of Limit_Bal")

#Observe the distribution of the Bill amount of the 6 months:
boxplot(TW$BILL_AMT1,main="Box Plot of bill amount - September 2005")
boxplot(TW$BILL_AMT2,main="Box Plot of bill amount - August 2005")
boxplot(TW$BILL_AMT3,main="Box Plot of bill amount - July 2005")
boxplot(TW$BILL_AMT4,main="Box Plot of bill amount - June 2005")
boxplot(TW$BILL_AMT5,main="Box Plot of bill amount - May 2005")
boxplot(TW$BILL_AMT6,main="Box Plot of bill amount - April 2005")

#Observe the distribution of the Pay Amount of the 6 months:
boxplot(TW$PAY_AMT1,main="Box Plot of the paid amount in September 2005")
boxplot(TW$PAY_AMT2,main="Box Plot of the paid amount in August 2005")
boxplot(TW$PAY_AMT3,main="Box Plot of the paid amount in July 2005")
boxplot(TW$PAY_AMT4,main="Box Plot of the paid amount in June 2005")
boxplot(TW$PAY_AMT5,main="Box Plot of the paid amount in May 2005")
boxplot(TW$PAY_AMT6,main="Box Plot of the paid amount in April 2005")


#Histogram of each variable and default value

#age
library(plyr)
Amu <- ddply(TW, "Default", summarise, grp.mean=mean(AGE))
Ag<-ggplot(TW, aes(x=AGE, color=Default)) +
        geom_histogram(fill="white", position="dodge")+
        geom_vline(data=Amu, aes(xintercept=grp.mean, color=Default),
                   linetype="dashed")
Ag

#Limit bal
LBmu <- ddply(TW, "Default", summarise, grp.mean=mean(LIMIT_BAL))
LB<-ggplot(TW, aes(x=LIMIT_BAL, color=Default)) +
        geom_histogram(fill="white", position="dodge")+
        geom_vline(data=LBmu, aes(xintercept=grp.mean, color=Default),
                   linetype="dashed")
LB

#Barplot

#gender 
Gen<-ggplot(TW, aes(x=Gender, color=Default))+
        geom_bar(fill = "white", position="dodge")

Gen

#Marriage 
Ma<-ggplot(TW, aes(x=MARRIAGE, color=Default))+
        geom_bar(fill = "white", position="dodge")

Ma      

#check correlations between numeric variables

library(GGally)
ggcorr(TW, name="corr", label=T)

#or

library(corrplot)
correlations <- cor(TW[,1:14])
corrplot(correlations, method="ellipse")


                              #Logistic regression

##Fisrt model with all variables:
Mod1res <- glm(Default  ~. ,family = binomial,data=TW)
summary(Mod1res)

AIC(Mod1res)

##Second model: step of the first model
Mod2res <- step(Mod1res)
summary(Mod2res)

#  Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -6.882e-01  7.166e-02  -9.605  < 2e-16 ***
#  LIMIT_BAL   -1.505e-06  3.218e-07  -4.677 2.91e-06 ***
#  BILL_AMT1   -7.631e-06  2.721e-06  -2.804  0.00504 ** 
#  BILL_AMT2    5.372e-06  3.561e-06   1.508  0.13144    
#BILL_AMT3    6.282e-06  2.435e-06   2.579  0.00990 ** 
#  PAY_AMT1    -3.052e-05  7.195e-06  -4.242 2.21e-05 ***
#  PAY_AMT2    -3.921e-05  8.487e-06  -4.620 3.84e-06 ***
#  PAY_AMT4    -1.593e-05  5.451e-06  -2.923  0.00347 ** 
#  MARRIAGE2   -2.890e-01  7.051e-02  -4.099 4.14e-05 ***
#  MARRIAGE3   -2.097e-01  2.947e-01  -0.711  0.47684   


##Third model: update of the second model
Mod3res <- update(Mod2res, ~. -BILL_AMT2)
summary(Mod3res)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -6.891e-01  7.169e-02  -9.611  < 2e-16 ***
#  LIMIT_BAL   -1.527e-06  3.215e-07  -4.749 2.04e-06 ***
#  BILL_AMT1   -4.739e-06  1.764e-06  -2.686  0.00724 ** 
#  BILL_AMT3    8.761e-06  1.927e-06   4.547 5.45e-06 ***
#  PAY_AMT1    -2.693e-05  6.707e-06  -4.016 5.93e-05 ***
#  PAY_AMT2    -4.163e-05  8.348e-06  -4.987 6.12e-07 ***
#  PAY_AMT4    -1.590e-05  5.440e-06  -2.922  0.00348 ** 
#  MARRIAGE2   -2.874e-01  7.049e-02  -4.077 4.57e-05 ***
#  MARRIAGE3   -2.118e-01  2.948e-01  -0.718  0.47247 


##Fourth model: upsate of the third model
Mod4res <- update(Mod3res, ~. -MARRIAGE)
summary(Mod4res)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -8.680e-01  5.709e-02 -15.205  < 2e-16 ***
#  LIMIT_BAL   -1.370e-06  3.180e-07  -4.310 1.63e-05 ***
#  BILL_AMT1   -4.652e-06  1.755e-06  -2.651  0.00803 ** 
#  BILL_AMT3    8.736e-06  1.917e-06   4.556 5.20e-06 ***
#  PAY_AMT1    -2.736e-05  6.727e-06  -4.067 4.77e-05 ***
#  PAY_AMT2    -4.188e-05  8.389e-06  -4.993 5.96e-07 ***
#  PAY_AMT4    -1.618e-05  5.441e-06  -2.973  0.00295 ** 
aic<-c(1:4)
aic[1]<-AIC(Mod1res)
aic[2]<-AIC(Mod2res)
aic[3]<-AIC(Mod3res)
aic[4]<-AIC(Mod4res)
aic<-data.frame(aic)
ind<-c("model1","model2","model3","model4")
colnames(aic)<-c("aic")
aic$ind<-ind

ggplot(data = aic, aes(x=ind,y=aic))+
  geom_point(size = 3)


#Calculation of VIF:
library(car)
vif(Mod3res)

#GVIF Df GVIF^(1/(2*Df))
#LIMIT_BAL  1.204397  1        1.097450
#BILL_AMT1 12.920472  1        3.594506
#BILL_AMT3 13.644397  1        3.693832
#PAY_AMT1   1.236766  1        1.112100
#PAY_AMT2   1.338575  1        1.156968
#PAY_AMT4   1.106314  1        1.051815
#MARRIAGE   1.023254  2        1.005764

#BIll_AMt1,BILL_AMT3 VIF is over 2.5, so it means those variables are correlated with each other

##Fifth model: update of the third model
Mod5res<-update(Mod3res,~. - BILL_AMT1)
summary(Mod5res)


##Sixth model: update of the third model
Mod6res<-update(Mod3res,~. - BILL_AMT3)
summary(Mod6res)


##Seventh model: update of the third mode
Mod7res<-update(Mod3res,~. - BILL_AMT1-BILL_AMT3)
summary(Mod7res)

#Calculation of the AIC and VIF 
AIC(Mod5res)
#[1] 5115.543
vif(Mod5res)
#GVIF Df GVIF^(1/(2*Df))
#LIMIT_BAL 1.202556  1        1.096611
#BILL_AMT3 1.473007  1        1.213675
#PAY_AMT1  1.247127  1        1.116748
#PAY_AMT2  1.324096  1        1.150694
#PAY_AMT4  1.106228  1        1.051774
#MARRIAGE  1.022897  2        1.005676

AIC(Mod6res)

#[1] 5132.431

vif(Mod6res)

#GVIF Df GVIF^(1/(2*Df))
#LIMIT_BAL 1.204557  1        1.097523
#BILL_AMT1 1.345220  1        1.159836
#PAY_AMT1  1.215002  1        1.102271
#PAY_AMT2  1.228401  1        1.108332
#PAY_AMT4  1.101059  1        1.049314
#MARRIAGE  1.023219  2        1.005755
AIC(Mod7res)
#[1] 5150.812

vif(Mod7res)
#GVIF Df GVIF^(1/(2*Df))
#LIMIT_BAL 1.113425  1        1.055190
#PAY_AMT1  1.114695  1        1.055791
#PAY_AMT2  1.123628  1        1.060013
#PAY_AMT4  1.066574  1        1.032750
#MARRIAGE  1.023337  2        1.005784

#Models with lower AIC: Mod2res, Mod3res, Mod5res
#So, we'll do ANOVA chi-squared test to know the best model

anova(Mod1res,Mod2res,Mod3res,Mod5res,test='Chisq')

# Selection of the final model
finalmodel<-Mod5res
summary(finalmodel)

#Check the AUC (Area under the curve):

#install.packages("ROCR")
library(ROCR)
library(caret)

df = as.data.frame(TW)
vec<-as.vector(df[,17])
vecis.vector(vec)

train_test_split<-createDataPartition(y=vec, p = 0.8 , list = FALSE)
data_train<-df[train_test_split,]
data_test<-df[-train_test_split,]

p <- predict(finalmodel, newdata=data_test, type="response")
pr <- prediction(p, data_test$Default)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,main="ROC curve")


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#install.packages("effects")
library(effects)
plot(allEffects(finalmodel))


confint(finalmodel)
#                2.5 %        97.5 %
#(Intercept) -8.433009e-01 -5.633145e-01
#LIMIT_BAL   -2.262398e-06 -1.003487e-06
#BILL_AMT3    2.652875e-06  5.124315e-06
#PAY_AMT1    -4.096775e-05 -1.403610e-05
#PAY_AMT2    -5.627253e-05 -2.361870e-05
#PAY_AMT4    -2.862625e-05 -7.238096e-06
#MARRIAGE2   -4.231495e-01 -1.470470e-01
#MARRIAGE3   -8.239851e-01  3.382187e-01


#plot probability of the result of Finalmodel


Cfd<- predict(finalmodel)

Taiwanindex<-c(1:nrow(TW))

Cfd<-exp(Cfd)/(1+exp(Cfd))
Cfd<-cbind(Cfd ,Taiwanindex)
Cfd<-data.frame(Cfd)
Cfd$Default<-TW$Default
colnames(Cfd)<-c('probability',"Taiwanindex","Default")
Cfd
finalgraph =ggplot(Cfd, aes(x = Taiwanindex, y = probability)) +
  geom_point(aes(color = Default), size =4) 
#+ geom_point(size = 4) +
#  geom_errorbar(aes(ymax = upr, ymin = lwr))

Meanprob <- ddply(Cfd, "Default", summarise, grp.mean=mean(Cfd))

finalgraph

