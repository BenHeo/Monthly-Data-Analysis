library(tidyverse)
library(data.table)
library(xgboost)
library(DMwR)
library(Matrix)
library(pdp)
# library(mlr)
# library(mmpf)
library(Ckmeans.1d.dp)
library(ROCR)
roc.plot = function(pred_prob, y, model_name = NULL){
  AUC = performance(prediction(pred_prob , y) , "auc")
  ROC = performance(prediction(pred_prob ,y) , "tpr","fpr")
  df = data.frame(tpr = ROC@y.values[[1]], fpr = ROC@x.values[[1]])
  title = paste(model_name,"\n AUC:", round(as.numeric(AUC@y.values),4))
  ggplot(df, aes(fpr, tpr)) + geom_line(size = 2, color = "slateblue3") +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", size = 2) +
    ggtitle(title) + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
}

cfm <- function(t) {
  acc = (t[1,1]+t[2,2])/(sum(t))
  prec = t[1,1]/(t[1,1]+t[2,1])
  rec = t[1,1]/(t[1,1]+t[1,2])
  f1 = 2*prec*rec/(prec+rec)
  ans = list(accuracy = acc, precision = prec, recall = rec, f1_score = f1)
  ans
}

churn <- fread("Telco_Customer_Churn.csv")
head(churn)
str(churn)
nrow(churn)
apply(is.na(churn), 2, sum)
churn <- churn[!is.na(churn$TotalCharges),]
churnYN <- table(churn$Churn)
churn_ratio <- churnYN[1]/churnYN[2]
nrow(churn)
# ggplot(churn) + geom_bar(aes(gender, fill = Churn))
# ggplot(churn) + geom_bar(aes(SeniorCitizen, fill = Churn))
# ggplot(churn) + geom_bar(aes(Partner, fill = Churn))
# ggplot(churn) + geom_bar(aes(Dependents, fill = Churn))
# ggplot(churn) + geom_histogram(aes(tenure, fill = Churn))
# ggplot(churn) + geom_bar(aes(PhoneService, fill = Churn))
# ggplot(churn) + geom_histogram(aes(MonthlyCharges, fill = Churn))
# ggplot(churn) + geom_histogram(aes(TotalCharges, fill = Churn))
ggplot(churn) + geom_bar(aes(Churn, fill = Churn)) # about twice


ID <- churn$customerID
info <- churn[,-"customerID"]
iii <- info %>% mutate(NofServices = ifelse(OnlineSecurity == "Yes", 1, 0) +
                  ifelse(OnlineBackup == "Yes", 1, 0) + ifelse(DeviceProtection == "Yes", 1, 0) +
                  ifelse(TechSupport == "Yes", 1, 0) + ifelse(StreamingTV == "Yes", 1, 0) +
                  ifelse(StreamingMovies == "Yes", 1, 0))
# Customers with 0 service or 4~6 services are non-churning than yardstick churn ratio
ggplot(iii) + geom_bar(aes(NofServices, fill = Churn)) 
service_churn <- table(iii$NofServices, iii$Churn)
cbind(service_churn, ratio = service_churn[,1]/service_churn[,2], yardstick=churn_ratio)


# No internet service users 
jjj <- info %>% mutate(NoInternetService = ifelse(OnlineSecurity == "No internet service", 1, 0) +
                           ifelse(OnlineBackup == "No internet service", 1, 0) + ifelse(DeviceProtection == "No internet service", 1, 0) +
                           ifelse(TechSupport == "No internet service", 1, 0) + ifelse(StreamingTV == "No internet service", 1, 0) +
                           ifelse(StreamingMovies == "No internet service", 1, 0))
ggplot(jjj) + geom_bar(aes(as.factor(NoInternetService), fill = Churn)) 
noInternetService_churn <- table(jjj$NoInternetService, jjj$Churn)
cbind(noInternetService_churn, ratio = noInternetService_churn[,1]/noInternetService_churn[,2], yardstick=churn_ratio)
apply((info[,9:14] == "No internet service"), 2, sum) # No internet service is All or Nothing
info %>%
  filter(InternetService == "No") %>%
  nrow() # No internet service customers...

# zoom in
## for streaming service 0 service users are less probable to churn
info2 <- info %>% 
  mutate(NofStreamingServices = ifelse(StreamingTV == "Yes", 1, 0) + ifelse(StreamingMovies == "Yes", 1, 0)) 
ggplot(info2, aes(NofStreamingServices, fill = Churn)) + geom_bar()
stream_churn <- table(info2$NofStreamingServices, info2$Churn)
cbind(stream_churn, ratio = stream_churn[,1]/stream_churn[,2], yardstick=churn_ratio)

## for insurance service 3-4(max) service users are less probable to churn
info2 <- info2 %>% mutate(NofInsuranceServices = ifelse(OnlineSecurity == "Yes", 1, 0) +
                  ifelse(OnlineBackup == "Yes", 1, 0) + ifelse(DeviceProtection == "Yes", 1, 0) +
                  ifelse(TechSupport == "Yes", 1, 0) ) 
ggplot(info2, aes(NofInsuranceServices, fill = Churn)) + geom_bar()
insurance_churn <- table(info2$NofInsuranceServices, info2$Churn)
cbind(insurance_churn, ratio = insurance_churn[,1]/insurance_churn[,2], yardstick=churn_ratio)

# as tenure increase proportion of no churn increase too
info2 <- info2 %>%
  mutate(tenure2 = factor(ifelse(tenure <= 12, "Tenure 0-12", 
                         ifelse(tenure <= 24, "Tenure 12-24",
                                ifelse(tenure <= 48, "Tenure 24-48",
                                       ifelse(tenure <= 60, "Tenure 48-60", "Tenure > 60")))),
                         levels = c("Tenure 0-12", "Tenure 12-24", "Tenure 24-48", "Tenure 48-60", "Tenure > 60")))
ggplot(info2, aes(tenure2, fill = Churn)) + 
  geom_bar() + xlab("tenure")
tenure_churn <- table(info2$tenure2, info2$Churn)
cbind(tenure_churn, ratio = tenure_churn[,1]/tenure_churn[,2], yardstick=churn_ratio)


# as factor
info2$gender <- as.factor(info2$gender)
info2$SeniorCitizen <- as.factor(info2$SeniorCitizen)
info2$Partner <- as.factor(info2$Partner)
info2$Dependents <- as.factor(info2$Dependents)
info2$PhoneService <- as.factor(info2$PhoneService)
info2$MultipleLines <- as.factor(info2$MultipleLines)
info2$InternetService <- as.factor(info2$InternetService)
info2$OnlineSecurity <- as.factor(info2$OnlineSecurity)
info2$OnlineBackup <- as.factor(info2$OnlineBackup)
info2$DeviceProtection <- as.factor(info2$DeviceProtection)
info2$TechSupport <- as.factor(info2$TechSupport)
info2$StreamingTV <- as.factor(info2$StreamingTV)
info2$StreamingMovies <- as.factor(info2$StreamingMovies)
info2$Contract <- as.factor(info2$Contract)
info2$PaperlessBilling <- as.factor(info2$PaperlessBilling)
info2$PaymentMethod <- as.factor(info2$PaymentMethod)
info2$Churn <- as.factor(info2$Churn)

test <- sample.int(length(info2$Churn), round(length(info2$Churn)*0.3))
train_set <- info2[-test,]
test_set <- info2[test,]
output_vector <- info2$Churn == "No"
y.test <- output_vector[test]

smote <- SMOTE(Churn~., data = train_set) # over sampling
ggplot(smote, aes(Churn, fill = Churn)) + geom_bar()

smote.y <- smote$Churn

smote.X.train <- sparse.model.matrix(Churn~SeniorCitizen+tenure+MultipleLines+InternetService+
                                       Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges+
                                       NofInsuranceServices+NofStreamingServices, smote)[,-1]
X.test <- sparse.model.matrix(Churn~SeniorCitizen+tenure+MultipleLines+InternetService+
                                Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges+
                                NofInsuranceServices+NofStreamingServices, test_set)[,-1]

smote.y.train <- smote.y == "No"


val.err = c()
candidates = seq(from = 100, to = 500, by = 20)
for (i in candidates){
  xgb.fit <- xgboost(smote.X.train, smote.y.train, max.depth = 2,
                     nrounds = i, eta = 0.1, objective = "binary:logistic", verbose = 1)
  xgb.pred <- predict(xgb.fit, X.test, type = "prob")
  val.err = c(val.err, mean(y.test != round(xgb.pred)))
}
val.err
which.min(val.err)

xgb.fit = xgboost(data = smote.X.train, label = smote.y.train, max.depth = 2,
                    eta = 0.1, nround = candidates[which.min(val.err)], objective = "binary:logistic", verbose = 0)
xgb.pred <- predict(xgb.fit, X.test)
import_mat = xgb.importance(colnames(smote.X.train), model = xgb.fit)
print(import_mat)

xgb.plot.importance(importance_matrix = import_mat)

roc.plot(xgb.pred, y.test, "Gradient Boosting")

xgb.YN <- ifelse(xgb.pred >= 0.4, "No", "Yes")
xgb.table <- table(real = y.test, pred = xgb.YN)[,c(2,1)]
cfm(xgb.table)


################# partial dependence plot ######################
#### this page helped me a lot : http://uc-r.github.io/gbm_regression ####

####### PDP ######
pdp.monthcharge <- partial(xgb.fit, pred.var = "MonthlyCharges", train = smote.X.train) %>% 
  autoplot(rug = TRUE, color = "black", train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("Monthly Charge PDP") + xlab("Monthly Charges") + ylab("") + aes(size = 3)

pdp.stream <- partial(xgb.fit, pred.var = "NofStreamingServices", train = smote.X.train) %>% 
  autoplot(rug = TRUE, color = "skyblue", train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("# of streaming services PDP") + xlab("# of stream") + ylab("") + aes(size = 3)

pdp.insure <- partial(xgb.fit, pred.var = "NofInsuranceServices", train = smote.X.train) %>% 
  autoplot(rug = TRUE, color = "orange", train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("# of insurance services PDP") + xlab("# of insure") + ylab("") + aes(size = 3)

pdp.tenure <- partial(xgb.fit, pred.var = "tenure", train = smote.X.train) %>% 
  autoplot(rug = TRUE, color = "green", train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("Tenure PDP") + xlab("Tenure") + ylab("") + aes(size = 3)

grid.arrange(pdp.monthcharge, pdp.tenure, pdp.stream, pdp.insure, ncol = 2)

####### ICE #######
par.monthcharge <- partial(xgb.fit, pred.var = "MonthlyCharges", ice = TRUE, center = TRUE, 
                           train = smote.X.train) %>% 
  autoplot(rug = TRUE, alpha = 0.1, train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("Monthly Charge ICE") + xlab("Monthly Charges") + ylab("")

par.stream <- partial(xgb.fit, pred.var = "NofStreamingServices", ice = TRUE, center = TRUE,
                      train = smote.X.train) %>% 
  autoplot(rug = TRUE, alpha = 0.1, train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("# of streaming services ICE") + xlab("# of stream") + ylab("")

par.insure <- partial(xgb.fit, pred.var = "NofInsuranceServices", ice = TRUE, center = TRUE, 
                      train = smote.X.train) %>% 
  autoplot(rug = TRUE, alpha = 0.1, train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("# of insurance services ICE") + xlab("# of insure") + ylab("")

par.tenure <- partial(xgb.fit, pred.var = "tenure", ice = TRUE, center = TRUE,
                      train = smote.X.train) %>% 
  autoplot(rug = TRUE, alpha = 0.1, train = smote.X.train) + scale_y_discrete("Churn N/Y") +
  ggtitle("Tenure ICE") + xlab("Tenure") + ylab("")

grid.arrange(par.monthcharge, par.tenure, par.stream, par.insure, ncol = 2)

###### Two variables ######
partial(xgb.fit, pred.var = c("tenure", "TotalCharges"), plot = TRUE,
        train = smote.X.train)
partial(xgb.fit, pred.var = c("NofInsuranceServices", "NofStreamingServices"), plot = TRUE,
        train = smote.X.train)


###### pdp 11.30 ######
pdp.monthcharge <- partial(xgb.fit, pred.var = "MonthlyCharges", train = smote.X.train, which.class = 2, prob = TRUE)
g1 <- ggplot(pdp.monthcharge, aes(MonthlyCharges, yhat)) + geom_line(size = 2) +
  ggtitle("Monthly Charge PDP") + xlab("Monthly Charges") + ylab("Churn Y/N") 

pdp.stream <- partial(xgb.fit, pred.var = "NofStreamingServices", train = smote.X.train, which.class = 2, prob = TRUE)
g2 <- ggplot(pdp.stream, aes(NofStreamingServices, yhat)) + geom_line(size=2, color = "skyblue") +
  ggtitle("# of streaming services PDP") + xlab("# of stream") + ylab("Churn Y/N")

pdp.insure <- partial(xgb.fit, pred.var = "NofInsuranceServices", train = smote.X.train, which.class = 2, prob = TRUE)
g3 <- ggplot(pdp.insure, aes(NofInsuranceServices, yhat)) + geom_line(size = 2, color = "orange") +
  ggtitle("# of insurance services PDP") + xlab("# of insure") + ylab("Churn Y/N")

pdp.tenure <- partial(xgb.fit, pred.var = "tenure", train = smote.X.train, which.class = 2, prob = TRUE)
g4 <- ggplot(pdp.tenure, aes(tenure, yhat)) + geom_line(size=2, color = "green") + 
  ggtitle("Tenure PDP") + xlab("Tenure") + ylab("Churn Y/N")

grid.arrange(g1, g4, g2, g3, ncol = 2)

####### ICE 11.30 #######
par.monthcharge <- partial(xgb.fit, pred.var = "MonthlyCharges", ice = TRUE, train = smote.X.train, which.class = 2, prob = TRUE)
p1 <- ggplot(par.monthcharge, aes(MonthlyCharges, yhat, group=yhat.id)) + geom_line(alpha=0.02) + 
  ggtitle("Monthly Charge ICE") + xlab("Monthly Charges") + ylab("Churn Y/N")

par.stream <- partial(xgb.fit, pred.var = "NofStreamingServices", ice = TRUE, train = smote.X.train, which.class = 2, prob = TRUE)
p2 <- ggplot(par.stream, aes(as.factor(as.integer(NofStreamingServices)), yhat)) + geom_boxplot() +
  ggtitle("# of streaming services ICE") + xlab("# of stream") + ylab("Churn Y/N")

par.insure <- partial(xgb.fit, pred.var = "NofInsuranceServices", ice = TRUE, train = smote.X.train, which.class = 2, prob = TRUE) 
p3 <- ggplot(par.insure, aes(as.factor(as.integer(NofInsuranceServices)), yhat)) + geom_boxplot() + 
  ggtitle("# of insurance services ICE") + xlab("# of insure") + ylab("Churn Y/N")

par.tenure <- partial(xgb.fit, pred.var = "tenure", ice = TRUE, train = smote.X.train, which.class = 2, prob = TRUE) 
p4 <- ggplot(par.tenure, aes(tenure, yhat, group=yhat.id)) + geom_line(alpha = 0.02) + 
  ggtitle("Tenure ICE") + xlab("Tenure") + ylab("Churn Y/N")

grid.arrange(p1, p4, p2, p3, ncol = 2)
