library(tidyverse)
library(modelsummary)
library(mice)

setwd("/Users/murphychesley/Desktop/Data\ Science\ for\ Economists/DScourse26/ProblemSets/PS7" )
df <- read.csv("wages.csv")

#Drop observations where hgc or tenure is missing.

df <- df[!is.na(df$hgc), ]
df <- df[!is.na(df$tenure), ]


#Use modelsummary to produce table.

datasummary_skim(df)
datasummary_skim(df, output = "summary_table.tex")

#Logwages are missing at a rate of about 25 percent, and it is likely that logwages are 
#missing not at random (MNAR). This is because wages are self-reported
#and some women may choose to not report. It is MNAR because the values are likely missing
#due to the nature of the variable itself, and not due to some other factor.

#List wise imputation of logwage
df_listwise <- df[!is.na(df$logwage), ]

listwise <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df_listwise)
summary(listwise)

#Mean imputation of logwage

df_mean <- df
df_mean$logwage[is.na(df_mean$logwage)] <- mean(df_mean$logwage, na.rm = TRUE)

mean <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df_mean)
summary(mean)

#Predictive mean matching imputation of logwage

df_predictive <- df
df_predictive$logwage[is.na(df_predictive$logwage)] <- predict(listwise, newdata = df_predictive[is.na(df_predictive$logwage), ])

predictive <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df_predictive)
summary(predictive)

#Multiple imputation of logwage

imputed <- mice(df, m = 5, method = "norm", seed = 100)
multi_impute1 <- with(imputed, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
multi_impute2 <- pool(multi_impute1)
summary(multi_impute2)

#Model summary list

models <- list(
  "Listwise" = listwise,
  "Mean Imputation" = mean,
  "Predicted Imputation" = predictive,
  "Multiple Imputation" = multi_impute2
)

modelsummary(models, output = "regression_table.tex")
modelsummary(models)
