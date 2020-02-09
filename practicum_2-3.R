# (5 pts) Investigate the Bank Marketing Data Set. Use the bank-full.csv in bank.zip for training your model. Use bank.csv in bank.zip for validating your model.

# I am unsure what is required for points here


# (0 pts) What is the actual algebraic regression equation for the model? Be sure you know how to write the equation.

library(tidyverse)

# y is target var
bank <- read_delim("data/bank/bank-full.csv", delim = ";") %>% 
  mutate_if(is.character, as.factor)


# (10 pts) Construct a logistic regression model to predict the probability of a customer subscribing to a term deposit. Test the statistical significance of all parameters and eliminate those that have a p-value > 0.05 using stepwise backward elimination. Be sure to encode non-binary categorical variables as dummy codes. In an actual project, we would need to transform those features or omit them -- in this exercise, we choose to ignore any skew. That leads to a poorly performing model, but here we are primarily concerned about the process of building a regression model. 


# Backward elimination is a bad practice. I will reiterate that. There is also no need to convert characters to dummy variables, it will be handled automatically. 

base_mod <- glm(y ~ ., 
                family = "binomial", 
                data = bank)

mod2 <- glm(y ~ .,
                family = "binomial",
                data = select(bank, -age))

mod3 <- glm(y ~ .,
            family = "binomial",
            data = select(bank, -default, -age))


mod4 <- glm(y ~ .,
            family = "binomial",
            data = select(bank, -default, -age, -pdays))


mod5 <- glm(y ~ .,
            family = binomial,
            data = select(bank, -default, -age, -pdays, -previous))

# there are no other variables to remove

# (10 pts) Test the model against the test data set (bank.csv) and determine its prediction accuracy (as a percentage correct).

bank_test <- read_delim("data/bank/bank.csv", delim = ";")

preds <- predict(mod5, bank_test, type = "response")
actuals <- as.factor(bank_test$y)

library(pROC)
# solution for identifying cutoff
# https://stats.stackexchange.com/questions/133320/logistic-regression-class-probabilities
mod_roc <- roc(response = actuals, predictor = preds)

e <- cbind(mod_roc$thresholds,
           mod_roc$sensitivities + mod_roc$specificities)

opt_t <- subset(e,e[,2]==max(e[,2]))[,1]

# calculate accuracy with said cut off
acc <- mean(ifelse(preds > opt_t, "yes", "no") == actuals)
acc


# (10 pts) What is the probability that a 41 year old, married, customer working as a manager in an HVAC company with a BS in Economics who has never defaulted on a loan, has a mortgage but no personal loans, and has been contacted twice on his mobile during the most recent marketing campaign with the most recent contact being 20 days ago on October 11th.

pred_mod <- glm(y ~ age + marital + job + education + default + loan + 
                  campaign + housing + duration + month + day + contact, 
                data = bank,
                family = "binomial")

new_dat <- tribble(
  ~age, ~marital, ~job, ~education, ~default, ~loan, ~campaign, ~housing, ~duration, ~month, ~day, ~contact,
  45, "married", "management", "tertiary", "no", "no", 2, "yes", 604800, "oct", 11, "cellular"
)

# there is 100% probability of default
predict(pred_mod, new_dat, type = "response")

