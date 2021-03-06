library(tidyverse)
library(rsample)
library(recipes)

colnames <- c("age", "work_class", "fnlwgt", "education", "education_num", 
              "marital_status", "occupation", "relationship", "race", "sex", 
              "capital_gain", "capital_loss", "hours_per_week", "native_country", "inc_group")


census <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                   col_names = colnames) %>% 
  mutate_if(is.character, as.factor)


# split data
init_split <- initial_split(census, .8)
census_train <- training(init_split)
census_test <- testing(init_split)

#------------------------------- define formula -------------------------------#
fit_form <- formula(inc_group ~ education + native_country + work_class + race + age + sex + native_country)

# make recipe and discretize age
fit_rec <- recipe(fit_form, census_train) %>% 
  step_discretize(age, num_breaks = 5) %>% 
  prep()

# apply preprocessing 
baked_train <- bake(fit_rec, census_train)
baked_test <- bake(fit_rec, census_test)

#------------------------------------------------------------------------------#
#                                 Train Models                                 #
#------------------------------------------------------------------------------#
#---------------------------------- klaR fit ----------------------------------#
klar_nb <- klaR::NaiveBayes(fit_form, data = baked_train)

#------------------------------- naivebayes fit -------------------------------#
nb_nb <- naivebayes::naive_bayes(fit_form, data = baked_train)

#--------------------------------- e1071 fit ----------------------------------#
e1071_nb <- e1071::naiveBayes(fit_form, data = baked_train)

#-------------------------------- logit model ---------------------------------#
logit_fit <- glm(fit_form, family = "binomial", data = baked_train)

#------------------------------- ensemble model -------------------------------#
# There is no definition of what kind of ensembling to use. For that reason
# I will use a majority vote method

pred_ensemble <- function(new_data) {
  
  klar_preds <- predict(klar_nb, new_data)[[1]]
  nb_preds <- predict(nb_nb, new_data)
  e1071_preds <- predict(e1071_nb, new_data)
  logit_preds <- ifelse(predict(logit_fit, new_data, type = "response")  < .5, "<=50K", ">50K")
  
  # mode function for calculating mode
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  tibble(klar = klar_preds,
         nb = nb_preds,
         e1071 = e1071_preds,
         logit = logit_preds) %>% 
    mutate_all(as.character) %>% 
    mutate(ensemble_vote = pmap(list(klar, nb, e1071, logit), c),
           ensemble_vote = map_chr(ensemble_vote, Mode))
  
}


pred_ensemble(baked_test)


#------------------------------------------------------------------------------#
#                                   Predict                                    #
#------------------------------------------------------------------------------#
#------------------------ white female federal worker -------------------------#
rare_human <- tibble(
  education = "Doctorate",
  age = 35,
  sex = "Female",
  native_country = "Portugal",
  work_class = "Local-gov",
  race = "White"
) %>% 
  mutate_if(is.character, as.factor) %>% 
  bake(fit_rec, .)


pred_ensemble(rare_human)
# this woman makes less than 50k

#------------------------------------------------------------------------------#
#                               Model Evaluation                               #
#------------------------------------------------------------------------------#
#----------------------------- create predictions -----------------------------#
all_preds <- pred_ensemble(baked_test) %>% 
  mutate(truth = baked_test$inc_group)

#----------------------------- confusion matrices -----------------------------#
janitor::tabyl(all_preds, klar, truth)
janitor::tabyl(all_preds, nb, truth)
janitor::tabyl(all_preds, e1071, truth)
janitor::tabyl(all_preds, logit, truth)

#----------------------------- accuracy measures ------------------------------#
pivot_longer(all_preds, cols = -truth, "model") %>% 
  mutate(same = truth == value) %>% 
  group_by(model) %>% 
  summarise(accuracy = mean(same))

