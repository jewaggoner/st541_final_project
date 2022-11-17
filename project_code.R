# Jake and Ben final project code

#setwd("C:\\Users\\Jake\\OneDrive - The University of Alabama\\ST 541 - R\\st541_final_project")
library("PerformanceAnalytics")
library(corrplot)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))

df <- read.csv("diabetes.csv")

# Summary Statistics of Data

head(df)
summary(df)
#No missing values, all numeric
# Nothing is on the same scale
# Only BMI and Pedigree Function are continuous, everything else is discrete integer values and the target is binary

str(df)
#768 observations with 9 variables

# Visual Exploration of Data
vars <- colnames(df)

par(mfrow=c(3,3))
for (i in 1:9) {
  hist(df[,i], xlab = vars[i], main = paste("Histogram of",vars[i]))
}


par(mfrow=c(3,3))
for (i in 1:8) {
  boxplot(df[,i], xlab = vars[i], main = paste("Boxplot of",vars[i]), horizontal = TRUE)
}

#Right Skewed: Pregnancies, SkinThickness, Insulin, DiabetesPedigree, Age
#Normal: Glucose (slightly unusual), Blood Pressure (w/ outlier), BMI 
#Bi-modal: Outcome

# Is it really possible to have 0 glucose?

par(mfrow=c(1,1))
hist(df$Glucose, main ="Histogram of Glucose", xlab = "Glucose level")

hist(df$Pregnancies, main ="Histogram of Pregnancies", xlab = "Pregnancy")
# Don't appear to be all women in sample

# Boxplots and T-test

for (i in 1:8) {
  print(ggplot(data=df,aes_string(y=vars[i]))+
          geom_boxplot(width=.5)+
          facet_wrap(~Outcome)+
          ggtitle(paste("Boxplot of",vars[i], "by Outcome"))
        )
}
?geom_boxplot
#Independent-samples T-test

# Iterate through t-tests for ll variables

for (i in 1:8) {
  print(t.test(formula(paste("df$",vars[i],"~","df$Outcome",sep=""))))
}
#The Welch Two sample t-test:We accept the null hypothesis for Blood pressure and SkinThickness. The P-Value is high, conclusively there is no significant difference in mean for BloodPressure between the two outcome category (1 and 0) and same holds for SkinThickness.


#another way to do all the t-tests
#lapply(vars[-9], function(x) t.test(formula(paste("df$",x,"~","df$Outcome",sep=""))))
#------------------------------------------

##Pairwise correlation coefficient and scatterplot

#remove outcome
df_corr<- df[,1:8]

#correlation and level of correlation
c1<-cor(df_corr)
chart.Correlation(c1,lower.panel = NULL, histogram=TRUE, pch=19)

#scatter plot
par(mfrow=c(3,3))
for (i in 1:8) {
  plot(df[,i], df$Outcome , xlab = vars[i], main = paste("plot of",vars[i]))
}
plot(df$DiabetesPedigreeFunction,df$Outcome)

par(mfrow=c(1,2))
#check for correlation between predictors
pairs(df$Pregnancies~df$Glucose+df$BloodPressure+df$SkinThickness+df$Insulin+df$BMI+df$DiabetesPedigreeFunction+df$Age,pch = 20, main= "Pairwise Scatterplot",lower.panel=NULL)

#plot the logged pairs
#pairs(log(df$Pregnancies)~log(df$Glucose)+log(df$BloodPressure)+log(df$SkinThickness)+log(df$Insulin)+log(df$BMI)+log(df$DiabetesPedigreeFunction)+log(df$Age),pch = 20, main= "Pairwise Scatterplot",lower.panel=NULL)

library(ggcorrplot)
c2 <- cor_pmat(df_corr)
ggcorrplot(c2, type = 'upper', title='Correlation of Predictors', lab=TRUE)

#------------------------------

## Model Building

### Full Model
m1 <- glm(Outcome ~. ,family = binomial(), data=df)
summary(m1)



library(alr4)
mmps(m1,layout=c(2,3))


## Backward Selection, BIC Criterion

n <- length(df$Outcome)
backBIC <- step(m1, direction="backward", data=df, k=log(n))
backBIC$coefficients

anova(backBIC)

### Stepwise Selection, BIC Criterion

stepwiseBIC <- step(m1, direction="both", data=df, k=log(n))
stepwiseBIC$coefficients


##Reduced model based on the most significant predictors
m2 <- glm(Outcome~Pregnancies+Glucose + BloodPressure+BMI+DiabetesPedigreeFunction,family = binomial(), data=df)
summary(m2)

#difference between the two models
anova(m2,m1,test="Chisq")

library(alr4)
mmps(m2,layout=c(2,3))





## prediction and ROC
pred <- predict(m2, type="response")

cbind(df$Outcome, pred)


# classification table / confusion matrix
table(round(pred))
sum(pred>=0.5)



# table

class.outcome <- data.frame(response = df$Outcome, predicted = round(pred,0))
xtabs(~ predicted + response, data = class.outcome)

# ROC curve/ AUC
par(mfrow=c(1,1))
library(pROC)
outcome.roc <- roc(Outcome ~ pred, plot = TRUE, print.auc = TRUE)

library(ROCR)    
pred <- prediction(pred, Outcome)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line


## goodness-of-fit
library(ResourceSelection)
hoslem.test(Outcome, fitted(m2), g=10)


detach(df)

