#READ DATA
library(readr)

rm(list=ls())
Taiwan <- read_table2("C:/Users/Asus/Desktop/Taiwan defaults - data.txt", 
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
library(MASS)


#Levels
levels(Taiwan$Default)
levels(Taiwan$Default)<-c("YES","NO")

levels(Taiwan$MARRIAGE)
levels(Taiwan$MARRIAGE)<-c("Married","Single","Others")

levels(Taiwan$Gender)
levels(Taiwan$Gender)<-c("Male","Female")


#Log-linear analysis of the table
Taiwantable <- xtabs( ~ Default + MARRIAGE + Gender,data=Taiwan)
Taiwantable
summary(Taiwantable)

ftable(Taiwantable)

#Independence model
loglm(~ Default + MARRIAGE + Gender ,data=Taiwantable)
TaiwanInde <- loglm(~  Default + MARRIAGE + Gender,data=Taiwantable)

#Residuals: Graphical representation
mosaicplot(Taiwantable,shade=TRUE)
mosaicplot(Taiwantable,margin=list(c(1,2),c(1,3),c(2,3)),shade=TRUE)

#Residuals
residuals(TaiwanInde,type="pearson")  
residuals(TaiwanInde,type="response")

#Saturated model
loglm(~ Default*MARRIAGE*Gender,data=Taiwantable)


#Homogeneous association model:
loglm(~ Default*MARRIAGE + Default*Gender + Gender*MARRIAGE,data=Taiwantable)

#Conditional models:
loglm(~ Default*MARRIAGE + Default*Gender,data=Taiwantable) # Marriage and Gender conditionally dependent
loglm(~ Default*Gender + Gender*MARRIAGE,data=Taiwantable) #Default and Marriage conditionally independent
loglm(~ Default*MARRIAGE + Gender*MARRIAGE,data=Taiwantable) #Default and Gender conditionally dependent

#Models definition
Taiwanfinal <- loglm(~ Default*MARRIAGE + Default*Gender + Gender*MARRIAGE,data=Taiwantable)

#Expected frequencies
fitted(Taiwanfinal)

#Coeficients
coef(Taiwanfinal)

