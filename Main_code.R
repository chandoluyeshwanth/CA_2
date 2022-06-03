
# Data preparation------------------------------------------------------------



# Load the packages
library(readr)
library(dplyr)
install.packages("ggplot2")
# Colour each chart point witha colour palette
install.packages("viridis")
library(ggplot2)
library(viridis)
library(corrplot)
install.packages("tidyverse")
install.packages("ROCR")
install.packages("bestglm")
library(bestglm)
library(ROCR)
library(ggplot2)



# Read in the data from the raw folder
stroke_dt <- read_csv("stroke.csv")


# Cleaning & Understanding Data------------------------------------------------
# Check the dimensions and structure of the data
glimpse(stroke_dt)
str(stroke_dt)
summary(stroke_dt)
# using `glimpse(), str(), summary()`following are the observation drawn

# 1.  Date format has Full week, month and year with century structure

# 2.  most of the columns which are in num class needs to be changed

# 3.  for further understanding data cleaning is required

stroke_dt <- stroke_dt[stroke_dt$gender != "Other", ]
stroke_dt$gender <- as.factor(stroke_dt$gender)
stroke_dt$ever_married <- as.factor(stroke_dt$ever_married)
stroke_dt$work_type <- as.factor(stroke_dt$work_type)
stroke_dt$Residence_type <- as.factor(stroke_dt$Residence_type)
stroke_dt$smoking_status <- as.factor(stroke_dt$smoking_status)
stroke_dt$bmi <- as.integer(stroke_dt$bmi)


# To work with values in character class data coercion is required Using factor
# function on these character class values a value can be established
# checking the factor result 
str(stroke_dt)
summary(stroke_dt)


#Changing date structure
# Check for repeating ids 

dim(stroke_dt[duplicated(stroke_dt$id),])[1]

# Since there are no continuous records for same id we will not use date 
# there is no need for converting date column 

# Checking for NA

colSums(is.na(stroke_dt))

##Only bmi has NA values. Further consideration is required to make a decision on bmi NA's
bmi_na_percentage <- (sum(is.na(stroke_dt$bmi))/length(stroke_dt$bmi)*100)
percentage_of_Bmi_stroke <- (sum(stroke_dt$stroke                                                                                [is.na(stroke_dt$bmi)])/
                               sum(stroke_dt$stroke >= 1)*100)
percentage_of_Bmi_stroke
bmi_na_percentage


# Overall NA % in BMI is 3.93%.That holds a 16.06% of instance where stroke
# occurred.
# Repleacing these NA's with mean value is a better option considering their 
# effect on stroke data

# Replace the missing values with mean of bmi attribute


stroke_dt$bmi[is.na(stroke_dt$bmi)] <- mean(stroke_dt$bmi,na.rm = TRUE)
sum(is.na(stroke_dt$bmi))
stroke_dt['AgeCat'] <- NA
stroke_dt$AgeCat[stroke_dt$age >= 45] <- "Elder"
stroke_dt$AgeCat[stroke_dt$age >= 26 & stroke_dt$age <= 44] <- "Middle Aged"
stroke_dt$AgeCat[stroke_dt$age <= 25] <- "Young"
stroke_dt$AgeCat<- factor(stroke_dt$AgeCat, order = TRUE,
                          levels = c("Young", "Middle Aged", "Elder"))

summary(stroke_dt)
# 1. In this data, it is known that there are 2116 male sex and 2994 female sex
# 2. The average age is 43.23 years, with the youngest age of 0.08 years and the oldest age of 82 years
# 3. From 5110 data, there were 3353 people who were married and 1757 people who were not married
# 4. private is the most work type with 2925 data
# 5. there are 2514 people living in rural and 56 people living in urban
# 6. average glucose level : 106.15, min: 55.12, and max: 271.74
# 7. average BMI : 29.08, min: 10.30, and max: 97.60
# 8. From 5110 data, there were 789 people who smoked and 1892 people who didn’t smoke

# Exploratory data analysis-----------------------------------------------------------------------
#understanding correlation


stroke_dt$heart_disease <- factor(stroke_dt$heart_disease, levels = c("0", "1"), 
                             labels = c("No","Yes"))
stroke_dt$hypertension <- factor(stroke_dt$hypertension,levels = c(0,1),
                            labels = c('No','Yes'))

variables_of_interest <- c("age",
                           "bmi",
                           "avg_glucose_level")
pairs(stroke_dt[variables_of_interest])
corrplot(corr = cor(stroke_dt[variables_of_interest]),
         tl.col = "Black", tl.cex = 0.9)

# As per the correlation plot age and bmi 
# value are more positively correlates than average_glucose level   

aggregate(bmi ~ work_type, stroke_dt, mean)

# AS per the data BMI value higer for patient working
# in private and self-employed sectors have 

###
hist(stroke_dt$avg_glucose_level,xlab = 'Glucose level')
hist(stroke_dt$bmi,xlab = 'BMI')
###

gender_stroke <-aggregate(bmi~gender,stroke_dt ,FUN = mean )
gender_stroke

# female patients has slightly higher bmi value than male

stroke_dt$stroke <- as.numeric(stroke_dt$stroke) 
str(stroke_dt)
work_stroke <- aggregate(stroke ~ work_type, stroke_dt, FUN = sum)
work_stroke

# Never_worked is the only category where 
# their has been no record of stroke in the the given data set 

smoke_stroke <- aggregate(stroke ~ smoking_status+gender, stroke_dt, FUN = sum)
smoke_stroke

# As per the data females who have never smoked had stroke than who formaerly 
# smoked or still having the habit 
# while male smokers has an even distribution 

variables_of_interest <- c("stroke",
                           "hypertension",
                           "heart_disease")
pairs(stroke_dt[variables_of_interest])
corrplot(corr = cor(stroke_dt[variables_of_interest]),
         tl.col = "Black", tl.cex = 0.9)
cor(stroke_dt$heart_disease, stroke_dt$stroke)

hhe_stroke <- table(stroke_dt$ever_married,stroke_dt$hypertension)

# As per the data  married patients have hypertension more than unmarried  

urg_avg <- aggregate(bmi+avg_glucose_level ~ Residence_type,stroke_dt,
                     FUN = mean)

urg_avg
# These isn't much variation  in the mean value of avg_glucose_level 
# between urban and rural regions
# which traits leads to stroke

par(mfrow = c(1, 1))
plot(xtabs(stroke~ work_type + ever_married, stroke_dt))
xtabs(stroke ~ work_type + smoking_status, stroke_dt)
xtabs( stroke~ work_type + AgeCat+smoking_status, stroke_dt)
plot(xtabs(stroke ~ work_type + smoking_status, stroke_dt))
######

cor(stroke_dt$bmi, stroke_dt$age)
scatter.smooth(x = stroke_dt$age,
               y = stroke_dt$bmi,
               main = "BIM ~ age",
               xlab = "AGE",
               col = "blue",
               lpars = list(col = "brown", lwd = 2),
               ylab = "BMI")
###

stroke_dt_scaled <- within(stroke_dt, rm(Date))
stroke_dt_scaled$avg_glucose_level <- scale(stroke_dt_scaled$avg_glucose_level)
stroke_dt_scaled$bmi <- scale(stroke_dt_scaled$bmi)
###
# As per the data elder patient working in private sector who never smoked 
# is the trait with most stroke 
final_dt <- subset(stroke_dt, AgeCat == "Elder" & work_type == "Private" & 
                     smoking_status == "never smoked" & stroke == "1")


#Model evaluation#############################################################


R<-10 # 
output <- matrix(NA,10,3) # creating an empty matrix of size 100x4
colnames(output) <- c("Training accuracy", "Validation accuracy", "Testing accuracy")




for(r in 1:10){  
  N<-nrow(stroke_dt_scaled)
  train <- sample(1:N, size = 0.70*N) 
  val <- sample( setdiff(1:N, train), size = 0.15*N )
  test <- setdiff(1:N, union(train, val))
  moddata <- stroke_dt_scaled[,1:11]
  
  # Logistic regression 
  fitlog <-glm(stroke ~ ., data = stroke_dt_scaled[train,],family = "binomial")
  predObj <- prediction(fitted(fitlog),stroke_dt_scaled$stroke[train])
  sens <- performance(predObj,"sens") # senstivity is calculated using performance
  spec <- performance(predObj,"spec") # specifity is calculated
  tau <- sens@x.values[[1]] 
  sensSpec <- sens@y.values[[1]]+spec@y.values[[1]] #senstivity and specifity are added 
  best <- which.max(sensSpec) # the maximum of senstivity + specifity is taken
  tau[best] # the tau value is taken for which the senstivity + specifity is maximum
  
  predtrian <-ifelse(predict(fitlog,newdata=moddata[train,]) > tau[best],1,0)
  logtrain<-table(stroke_dt_scaled$stroke[train],predtrian) # cross tabulation is done for actual vs prediction
  acctrain <-  sum(diag(logtrain))/sum(logtrain)
  
  predval <-ifelse(predict(fitlog,newdata=moddata[val,]) > tau[best],1,0)
  logval<-table(stroke_dt_scaled$stroke[val],predval) # cross tabulation is done for actual vs prediction
  accval <-  sum(diag(logval))/sum(logval)
  
  predtest <-ifelse(predict(fitlog,newdata=moddata[test,]) > tau[best],1,0)
  logtest<-table(stroke_dt_scaled$stroke[test],predtest) # cross tabulation is done for actual vs prediction
  acctest <-  sum(diag(logtest))/sum(logtest)
  
  output[r,1] <- acctrain
  output[r,2] <- accval
  output[r,3] <- acctest
}

output

#spliting data#####
N <- nrow(stroke_dt_scaled)
train <- sample(1:N, size = 0.70*N)
val <- sample(setdiff(1:N, train), size = 0.15*N )
test <- setdiff(1:N, union(train, val))

length(train)
length(val)
length(test)

library(bestglm)
library(ROCR)

moddata <- stroke_dt_scaled[,1:11]

head(stroke_dt_scaled)
fitlog <- glm(stroke ~ ., data = stroke_dt_scaled[train,],family = "binomial")
summary(fitlog)

## NA values are seen when the following columns are removed for categorical variables 
logodd <- predict(fitlog)
phat <- fitted(fitlog)


colr <- c("orange","blue")
plot(logodd,jitter(phat, amount = 0.1),pch =c(0,1),
     col =adjustcolor(colr,0.7),cex =0.7,
     xlab="log-odds",ylab="fitted probabilities")


predObj <- prediction(fitted(fitlog),stroke_dt_scaled$stroke[train])

pref <- performance(predObj,"tpr","fpr")
#plot is done for true postive rate and false positive rate which is a ROC curve
plot(pref,ylab="True Positive Rate- TPR",xlab="False Positive Rate-FPR", main="ROC")
auc <- performance(predObj,"auc") # auc is calculated to calculate the are under the ROC curve 
auc@y.values 


sens <- performance(predObj,"sens") # senstivity is calculated using performance
spec <- performance(predObj,"spec") # specifity is calculated
tau <- sens@x.values[[1]] 
sensSpec <- sens@y.values[[1]]+spec@y.values[[1]] #senstivity and specifity are added 
best <- which.max(sensSpec) # the maximum of senstivity + specifity is taken
plot(tau,sensSpec,type="l",ylab="senstivity+specifity",xlab="Threshold",main="Plot for finding optimal Tau") # threshold vs senstivity + specifity
points(tau[best],sensSpec[best],pch=19,col=adjustcolor("darkorange2",0.5)) #the maximum point is marked
tau[best] # the tau value is taken for which the senstivity + specifity is maximum


predtrian <-ifelse(predict(fitlog,newdata=moddata[train,]) > tau[best],1,0)
logtrain<-table(stroke_dt_scaled$stroke[train],predtrian) # cross tabulation is done for actual vs prediction
acctrain <-  sum(diag(logtrain))/sum(logtrain)
acctrain

predval <-ifelse(predict(fitlog,newdata=moddata[val,]) > tau[best],1,0)


logval<-table(stroke_dt_scaled$stroke[val],predval) # cross tabulation is done for actual vs prediction
accval <-  sum(diag(logval))/sum(logval)
accval

predtest <-ifelse(predict(fitlog,newdata=moddata[test,]) > tau[best],1,0)
logtest<-table(stroke_dt_scaled$stroke[test],predtest) # cross tabulation is done for actual vs prediction
acctest <-  sum(diag(logtest))/sum(logtest)
acctest
