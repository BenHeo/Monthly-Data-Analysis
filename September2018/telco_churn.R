library(tidyverse)
library(data.table)
churn <- fread("Telco_Customer_Churn.csv")
head(churn)
str(churn)
ggplot(churn) + geom_bar(aes(gender))
ggplot(churn) + geom_bar(aes(SeniorCitizen))
ggplot(churn) + geom_bar(aes(Partner))
ggplot(churn) + geom_bar(aes(Dependents))
ggplot(churn) + geom_bar(aes(tenure))
ggplot(churn) + geom_bar(aes(PhoneService))