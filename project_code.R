# Jake and Ben final project code

# setwd("C:\\Users\\Jake\\OneDrive - The University of Alabama\\ST 541 - R\\st541_final_project")

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
#Bimodal: Outcome

# Is it really possible to have 0 glucose?

par(mfrow=c(1,1))
hist(df$Glucose)

hist(df$Pregnancies)
# Don't appear to be all women in sample

# Boxplots and T-test

for (i in 1:8) {
  print(ggplot(data=df,aes_string(y=vars[i]))+
          geom_boxplot()+
          facet_wrap(~Outcome)+
          ggtitle(paste("Boxplot of",vars[i], "by Outcome"))
        )
}

attach(df)

outcome_ttest <- function(var_name) {
  t.test(var_name~Outcome)
}

for (i in 1:8) {

  t.test(formula(substitute(vars[i] ~ Outcome)), df)
  
}

?substitute
