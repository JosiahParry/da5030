# (0 pts) Download the data set Census Income Data for Adults along with its explanation. Note that the data file does not contain header names; you may wish to add those. The description of each column can be found in the data set explanation. Use the data in the file adult.data.
# (0 pts) Explore the data set as you see fit and that allows you to get a sense of the data and get comfortable with it. Is there distributional skew in any of the features? Is there a need to apply a transform? 
#   (0 pts) Split the data set 80/20 so you retain 20% for testing. 
# (25 pts) Using the Naive Bayes Classification algorithm from the KlaR, naivebayes, and e1071 packages, predict the binomial class membership for a white male adult who is a federal government worker with a bachelors degree who immigrated from India. Ignore any other features in your model. Are the predictions in agreement? You need to transform continuous variables into categorical variables by binning (use equal size bins from in to max).

# binning age is a rather arbitrary approach. 

# (25 pts) Calculate accuracy and prepare confusion matrices for all three Bayes implementations (KlaR, naivebayes, e1071). Compare the implementations and comment on differences. Be sure to use the same training data set for all three. The results should be the same but they may differ if the different implementations deal differently with LaPalace Estimators.

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
#------------------------------- define formula -------------------------------#
fit_form <- formula(inc_group ~ education + native_country + work_class + race + age)

#---------------------------------- klaR fit ----------------------------------#
klar_nb <- klaR::NaiveBayes(fit_form, data = baked_train)

#------------------------------- naivebayes fit -------------------------------#
nb_nb <- naivebayes::naive_bayes(fit_form, data = baked_train)

#--------------------------------- e1071 fit ----------------------------------#
e1071_nb <- e1071::naiveBayes(fit_form, data = baked_train)


all_nb <- list(klar_nb, nb_nb, e1071_nb)

#------------------------------------------------------------------------------#
#                                   Predict                                    #
#------------------------------------------------------------------------------#
#------------------------ white indian federal worker -------------------------#
rare_human <- tibble(
  education = "Bachelors",
  age = 40,
  native_country = "India",
  work_class = "Federal-gov",
  race = "White"
) %>% 
  bake(fit_rec, .)


mod_preds <- map_chr(.x = all_nb, ~{
  predict(.x, rare_human) %>% 
    pluck(1) %>% 
    as.character()
})

#------------------------------------------------------------------------------#
#                               Model Evaluation                               #
#------------------------------------------------------------------------------#
#----------------------------- create predictions -----------------------------#
all_preds <- map(all_nb, predict, baked_test)

#----------------------------- confusion matrices -----------------------------#
table(all_preds[[1]][[1]], baked_test$inc_group)
table(all_preds[[2]], baked_test$inc_group)
table(all_preds[[3]], baked_test$inc_group)


#----------------------------- accuracy measures ------------------------------#
mean(all_preds[[1]][[1]] == baked_test$inc_group)
mean(all_preds[[2]] == baked_test$inc_group)
mean(all_preds[[3]] == baked_test$inc_group)

