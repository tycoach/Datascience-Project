
library(caTools)
library(caret)
library(pROC)

#mplementing Our Models 

loan_new<-loan_data %>%
  select(- c(ZIP.Code, Experience))

set.seed(123)
split <- sample.split(loan_new$PersonalLoan , SplitRatio = 0.7)
train_loan <- subset(x = loan_new , split == TRUE)
test_loan <- subset(x = loan_new, split==FALSE)

train_loan <- train_loan %>% 
  mutate(PersonalLoan = as.factor(PersonalLoan),
         SecuritiesAccount = as.factor(SecuritiesAccount))

log_model<- glm(PersonalLoan ~ Age + Income + Family + CCAvg + Education +
                  Mortgage + SecuritiesAccount + CDAccount + Online + CreditCard, data = train_loan , family = binomial(link="logit"))

log_model2<- glm(PersonalLoan ~ Income + Family + CCAvg + Education + CDAccount + Online + CreditCard, data = train_loan , family = binomial(link="logit"))
summary(log_model2)
#Model 1 Offers a lower AIC which will be better for our prediction 
#Exploring Important Feature 
importance<- varImp(log_model)
importance %>%
  arrange(desc(Overall)) %>%
  top_n(10)

pred<- predict(log_model, test_loan , type = "response")
pred_label<- as.factor(if_else(pred < 0.5, 0, 1))
table(pred_label)
hist(predict(log_model, test_loan, type = "response"), breaks = 100,
     xlab = "Logistic Model Probability")

confusionMatrix(pred_label, test_loan$PersonalLoan, positive = "1")

exp(coef(log_model))
anova(log_model , test = "Chisq")
#Plotting ROC AND AUC Curve 


pred_prob <- predict(object = log_model, newdata = test_loan, type = "response")
test_roc = roc(test_loan$PersonalLoan ~ pred_prob, plot = TRUE, print.auc = TRUE)
plot(test_roc)

roc_object <- roc( test_loan$PersonalLoan, pred)
auc(roc_object)

#computing model performance metrics
data.frame( R2 = R2(predictions, testing_dataset $ sales),
            RMSE = RMSE(predictions, testing_dataset $ sales),
            MAE = MAE(predictions, testing_dataset $ sales))
