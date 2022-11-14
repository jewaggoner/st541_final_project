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
          geom_boxplot()+
          facet_wrap(~Outcome)+
          ggtitle(paste("Boxplot of",vars[i], "by Outcome"))
        )
}

#Independent-samples T-test
t.test(df$Age~df$Outcome)
# Iterate through t-tests for ll variables

for (i in 1:8) {
  print(t.test(formula(paste("df$",vars[i],"~","df$Outcome",sep=""))))
}
#The Welch Two sample t-test:We accept the null hypothesis for Blood pressure and SkinThickness. The P-Value is high, conclusively there is no significant difference in mean for BloodPressure between the two outcome category (1 and 0) and same holds for SkinThickness.


#another way to do all the t-tests
#lapply(vars[-9], function(x) t.test(formula(paste("df$",x,"~","df$Outcome",sep=""))))


##Pairwise correlation coefficient and scatterplot

#remove outcome
df_corr<- df[,1:8]

#correlation and level of correlation
c1<-cor(df_corr)
chart.Correlation(c1,lower.panel = NULL, histogram=TRUE, pch=19)

#check for correlation between predictors
pairs(df$Pregnancies~df$Glucose+df$BloodPressure+df$SkinThickness+df$Insulin+df$BMI+df$DiabetesPedigreeFunction+df$Age,pch = 20, main= "Pairwise Scatterplot",lower.panel=NULL)

#plot the logged pairs
#pairs(log(df$Pregnancies)~log(df$Glucose)+log(df$BloodPressure)+log(df$SkinThickness)+log(df$Insulin)+log(df$BMI)+log(df$DiabetesPedigreeFunction)+log(df$Age),pch = 20, main= "Pairwise Scatterplot",lower.panel=NULL)



