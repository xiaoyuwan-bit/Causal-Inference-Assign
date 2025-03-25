########################################################
## CSI 2024 Major Assignment 1 R code
########################################################

##############
# Question 1 #
##############
library(table1)
library(EValue)

setwd("C:/Users/xiaoyuwan/OneDrive/unimelb/causal inference/csi assignment1")
rm(list=ls())

#Read in the datasets:
data01 <- read.csv( file="CSI2024_data01.csv")
data02 <- read.csv( file="CSI2024_data02.csv")
data03 <- read.csv( file="CSI2024_data03.csv")
data04 <- read.csv( file="CSI2024_data04.csv")

#a) ACE of A on Y 
#without adjusting for Z:
reg01_data01 <- lm(Y~A, data=data01)
summary(reg01_data01)
confint(reg01_data01, "A")

reg01_data02 <- lm(Y~A, data=data02)
summary(reg01_data02)
confint(reg01_data02, "A")

reg01_data03 <- lm(Y~A, data=data03)
summary(reg01_data03)
confint(reg01_data03, "A")

reg01_data04 <- lm(Y~A, data=data04)
summary(reg01_data04)
confint(reg01_data04, "A")

#adjusting for Z:
reg02_data01 <- lm(Y~A + Z, data=data01)
summary(reg02_data01)
confint(reg02_data01, "A")

reg02_data02 <- lm(Y~A + Z, data=data02)
summary(reg02_data02)
confint(reg02_data02, "A")

reg02_data03 <- lm(Y~A + Z, data=data03)
summary(reg02_data03)
confint(reg02_data03, "A")

reg02_data04 <- lm(Y~A + Z, data=data04)
summary(reg02_data04)
confint(reg02_data04, "A")

####################################################################
# Question 2

require(ggplot2)
require(survey)
require(EValue)
require(table1)

#Read in the data
nighticu <- read.csv("NighttimeICU.csv")

#Look at the means/proportions in each exposure group
table1(~ Saps_3 + factor(Reason_index_ICU_admisssion)  + factor(Admission_Source) +
         factor(Systemic_hypertension) + factor(Diabetes_mellitus) +
         factor(Cancer) + factor(Congestive_heart_failure) + factor(COPD) +
         factor(Chronic_Kidney_disease) + factor(Liver_cirrhosis) | Nighttime_ICU_discharge, 
       data=nighticu)

#Look at density of SAPS 3 across exposure groups
ggplot(nighticu, aes(Saps_3, colour=as.factor(Nighttime_ICU_discharge))) + 
  geom_density()

#Calculate the standardised differences:
#Standardised differences
#Binary confounders:
#unweighted counts, percentages:
p0 = 100*sum(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 0]==1)/sum(!is.na(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 0]))  
p1 = 100*sum(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 1]==1)/sum(!is.na(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 1]))  
#standardised differences:
stdiff1 = ((p1 - p0)/sqrt(((p0/100)*(1-p0/100) + (p1/100)*(1-(p1/100)))/2))
print(paste("For admission reason standardised difference =", round(stdiff1, 2)))
#replace Reason_index_ICU_admisssion with other binary confounders in the above to get standardised differences
#for other binary confounders

#Categorical confounder: here we need to consider each level of the 
#confounder: we'll consider Admission_Source
table(nighticu$Admission_Source, nighticu$Nighttime_ICU_discharge) 
for(j in 0:4){
  p0 = 100*sum(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 0]==j)/sum(!is.na(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 0]))  
  p1 = 100*sum(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 1]==j)/sum(!is.na(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 1]))  
  #standardised differences:
  stdiff1 = ((p1 - p0)/sqrt(((p0/100)*(1-p0/100) + (p1/100)*(1-(p1/100)))/2))
  print(paste("For source=",j," standardised difference =", round(stdiff1, 2)))
  
}                                                                             

#Continuous confounder: Saps_3
#unweighted means, sds:
mean0 = mean(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 0])
var0 = var(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 0])
mean1 = mean(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 1])
var1 = var(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 1])
#standardised differences:
stdiff1= 100*((mean1 - mean0)/sqrt((var1 +var0)/2))
print(paste("For Saps 3 standardised difference =", round(stdiff1, 2)))


#Fit the propensity score model
propmodel <- glm(Nighttime_ICU_discharge ~ Saps_3 + as.factor(Reason_index_ICU_admisssion) +
                   as.factor(Admission_Source) + Systemic_hypertension+ Diabetes_mellitus +
                   Cancer + Congestive_heart_failure + COPD + Chronic_Kidney_disease +
                   Liver_cirrhosis, 
                 family=binomial(logit), data=nighticu)
nighticu$ps <-  propmodel$fitted.values

#Take a look at the distribution of propensity scores 

ggplot(nighticu, aes(x=ps, fill=as.factor(Nighttime_ICU_discharge))) + 
  geom_histogram(bins=20, alpha=.5, position="identity")

ggplot(nighticu, aes(x=ps, fill=as.factor(Nighttime_ICU_discharge))) + 
  geom_density()

ggplot(nighticu, aes(as.factor(Nighttime_ICU_discharge), ps))  + 
  geom_boxplot()

#Look at the maximums and minimums in each exposure group
minps0 <- min(nighticu$ps[nighticu$Nighttime_ICU_discharge==0])
maxps0 <- max(nighticu$ps[nighticu$Nighttime_ICU_discharge==0])
minps1 <- min(nighticu$ps[nighticu$Nighttime_ICU_discharge==1])
maxps1 <- max(nighticu$ps[nighticu$Nighttime_ICU_discharge==1])

#Generate the IPWs:
nighticu$ipw[nighticu$Nighttime_ICU_discharge == 1] <-   1/(nighticu$ps[nighticu$Nighttime_ICU_discharge == 1])
nighticu$ipw[nighticu$Nighttime_ICU_discharge == 0] <-   1/(1-nighticu$ps[nighticu$Nighttime_ICU_discharge == 0])

#Take a look at the plots
ggplot(nighticu, 
       aes(ps, colour=as.factor(Nighttime_ICU_discharge))) + geom_density() 

ggplot(nighticu, 
       aes(ps, colour=as.factor(Nighttime_ICU_discharge),weight=ipw)) + 
  geom_density() 

#Take a look at the plots: considering the distribution of Saps_3 
#in unweighted and weighted spaces 
ggplot(nighticu, 
       aes(Saps_3, colour=as.factor(Nighttime_ICU_discharge))) + 
  geom_density() 
ggplot(nighticu, 
       aes(Saps_3, colour=as.factor(Nighttime_ICU_discharge),weight=ipw)) +
  geom_density() 


#Weighted std diffs
#Binary confounders:
#unweighted counts, percentages:
p0 = 100*sum(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 0]==1)/sum(!is.na(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 0]))  
p1 = 100*sum(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 1]==1)/sum(!is.na(nighticu$Reason_index_ICU_admisssion[nighticu$Nighttime_ICU_discharge == 1]))  
#weighted counts, percentages:
p0w = 100*sum(nighticu$ipw[nighticu$Reason_index_ICU_admisssion ==1 & nighticu$Nighttime_ICU_discharge == 0])/sum((nighticu$ipw[nighticu$Nighttime_ICU_discharge == 0 & !is.na(nighticu$Reason_index_ICU_admisssion)]))  
p1w = 100*sum(nighticu$ipw[nighticu$Reason_index_ICU_admisssion ==1 &nighticu$Nighttime_ICU_discharge == 1])/sum((nighticu$ipw[nighticu$Nighttime_ICU_discharge == 1 & !is.na(nighticu$Reason_index_ICU_admisssion)]))  
#standardised differences: Note the denominator of the weighted stddiff!
stdiff1 = ((p1 - p0)/sqrt(((p0/100)*(1-p0/100) + (p1/100)*(1-(p1/100)))/2))
stdiffW = ((p1w - p0w)/sqrt(((p0/100)*(1-p0/100) + (p1/100)*(1-(p1/100)))/2))
print(paste("For Reason_index_ICU_admisssion standardised difference =", round(stdiff1, 2), "and the weighted standardised difference is ", round(stdiffW, 2)))


#Categorical confounder: here we need to consider each level of the 
#confounder: we'll consider Admission_Source
table(nighticu$Admission_Source, nighticu$Nighttime_ICU_discharge) 
for(j in 1:4){
  p0 = 100*sum(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 0]==j)/sum(!is.na(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 0]))  
  p1 = 100*sum(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 1]==j)/sum(!is.na(nighticu$Admission_Source[nighticu$Nighttime_ICU_discharge == 1]))  
  #standardised differences:
  p0w = 100*sum(nighticu$ipw[nighticu$Admission_Source ==j & nighticu$Nighttime_ICU_discharge == 0])/sum((nighticu$ipw[nighticu$Nighttime_ICU_discharge == 0 & !is.na(nighticu$Admission_Source)]))  
  p1w = 100*sum(nighticu$ipw[nighticu$Admission_Source ==j &nighticu$Nighttime_ICU_discharge == 1])/sum((nighticu$ipw[nighticu$Nighttime_ICU_discharge == 1 & !is.na(nighticu$Admission_Source)]))  
  #standardised differences: Note the denominator of the weighted stddiff!
  stdiff1 = ((p1 - p0)/sqrt(((p0/100)*(1-p0/100) + (p1/100)*(1-(p1/100)))/2))
  stdiffW = ((p1w - p0w)/sqrt(((p0/100)*(1-p0/100) + (p1/100)*(1-(p1/100)))/2))
  print(paste("For Admission_Source=",j," std diff =", round(stdiff1, 2), "and the weighted std diff is ", round(stdiffW, 2)))
  stdiff1 = ((p1 - p0)/sqrt(((p0/100)*(1-p0/100) + (p1/100)*(1-(p1/100)))/2))
}                                                                             

#Continuous confounder: 
#unweighted means, sds:
mean0 = mean(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 0])
var0 = var(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 0])
mean1 = mean(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 1])
var1 = var(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge == 1])
#weighted means:
mean0w = weighted.mean(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge==0], w=nighticu$ipw[nighticu$Nighttime_ICU_discharge==0])
mean1w = weighted.mean(nighticu$Saps_3[nighticu$Nighttime_ICU_discharge==1], w=nighticu$ipw[nighticu$Nighttime_ICU_discharge==1])
#standardised differences:
stdiff1= 100*((mean1 - mean0)/sqrt((var1 +var0)/2))
stdiffW= 100*((mean1w - mean0w)/sqrt((var1 +var0)/2))
print(paste("For Saps_3 std diff =", round(stdiff1, 2),"and weighted std. diff =", round(stdiffW, 2)))

#Fit the regression models
#Unadjusted
unadj <- glm(Status_Hospital_discharge ~ Nighttime_ICU_discharge, 
    family= binomial(link="logit"),data=nighticu)
exp(confint(unadj))

#Adjusted
adj <- glm(Status_Hospital_discharge ~ Nighttime_ICU_discharge +
             Saps_3 + as.factor(Reason_index_ICU_admisssion) +
             as.factor(Admission_Source) + Systemic_hypertension+ Diabetes_mellitus +
             Cancer + Congestive_heart_failure + COPD + Chronic_Kidney_disease +
             Liver_cirrhosis, 
             family= binomial(link="logit"),data=nighticu)
exp(confint(adj))

#IPW 
ipwadj <- (svyglm(Status_Hospital_discharge ~ as.factor(Nighttime_ICU_discharge), 
                  family=binomial(link="logit"),
                  design = svydesign(~ 1, weights = ~ ipw,
                                     data = nighticu)))
exp(confint(ipwadj))


#E-value
evalue(OR(exp(ipwadj$coefficients[2]), rare=TRUE), lo=exp(confint(ipwadj)[2,])[1])

evalue(OR(exp(confint(ipwadj)[2,])[1], rare=TRUE))

