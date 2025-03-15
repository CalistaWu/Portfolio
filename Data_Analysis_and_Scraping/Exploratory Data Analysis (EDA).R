#install.packages("corrplot")

library(tidyverse)
library(ggplot2)
library(corrplot)

dt<-fread("bike_buyers.csv")

summary(dt)

ggplot(dt, aes(x = Income)) +
  geom_histogram() +
  labs(title = "Distribution of Income")

cat("Number of rows with missing values:", length(incomplete_rows), "\n")

missing_locations <- which(is.na(dt$Income), arr.ind = TRUE)
print(missing_locations)

clean_income<-dt$Income

clean_income[which(is.na(clean_income))]=mean(dt$Income, na.rm = TRUE)
dt$Age[which(is.na(dt$Age))]=mean(dt$Age, na.rm = TRUE)
df_clean_income<- data.frame(income=clean_income)

ggplot(df_clean_income,aes(x=income)) +
  geom_histogram() +
  labs(title = "Distribution of Income (After Imputation)")

corr_matrix <- cor(clean_income,dt$Age)

boxplot(clean_income, main = "Box Plot of Income")


