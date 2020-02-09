


library(tidyverse)
library(anomalize)
library(corrr)

house <- readxl::read_xlsx("data/uffidata.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(year_sold = as.factor(year_sold)) %>% 
  select(-observation)



# (2 pts) Are there outliers in the data set? How do you identify outliers and how do you deal with them? Remove them but create a second data set with outliers removed. Keep the original data set.

# since there is a small amount of data, use the GESD method of removal.
no_anoms <- house %>% 
  anomalize(sale_price, method = "gesd") %>% 
  filter(anomaly == "No") %>% 
  select(-sale_price_l1, -sale_price_l2, -anomaly)



# (3 pts) What are the correlations to the response variable and are there collinearities? Build a full correlation matrix.

cors <- correlate(no_anoms) %>% 
  shave()

# x45_years * observation = -.63, living_area * sale_price = .671


# (5 pts) What is the ideal multiple regression model for predicting home prices in this data set using the data set with outliers removed? Provide a detailed analysis of the model, including Adjusted R-Squared and RMSE. Use backward elimination by p-value or AIC to build the model. Some of your features may exhibit skew and may not be reasonably normally distributed. In an actual project, we would need to transform those features or omit them -- in this exercise, we choose to ignore any skew. That leads to a poorly performing model, but here we are primarily concerned about the process of building a regression model. Be sure to dummy code any multi-level categorical variables, i.e., those that are not binary.

# The ideal regression model for prediction home prices requires knowledge about realtor market. We cannot build regression models, in good conscience,  without any understanding of what we are modeling. The main purpose of regression is inference. We ought to include variables in the model so long as we believe them to be important fo the purposes of inference. If a variable is not "significant" by p-value, that is not enough to deem the variable unimportant in the model fit process. 

# With that said, all variables except observation should be included. There is no logical reason why we should include the observation number in the model unless this has a time component we should consider. Even so, we have the year that the house was sold and can include that as a dummy vartiable to create a fixed effect model. There are likely interaction effects that should be considered as well. There are two reasons to not consider them: 1) degrees of freedom and 2) I lack the domain expertise to make these assumptions. 




mod1 <- lm(sale_price ~ ., data = no_anoms)
mod2 <- lm(sale_price ~ ., data = select(no_anoms, -central_air))
mod3 <- lm(sale_price ~ ., data = select(no_anoms, -central_air, -x45_yrs))
mod4 <- lm(sale_price ~ ., data = select(no_anoms, -central_air, -x45_yrs, -bsmnt_fin_sf))
mod5 <- lm(sale_price ~ ., data = select(no_anoms, -central_air, -x45_yrs, -bsmnt_fin_sf, -brick_ext))
mod6 <- lm(sale_price ~ ., data = select(no_anoms, -central_air, -x45_yrs,
                                         -bsmnt_fin_sf, -brick_ext, -pool))
mod7 <- lm(sale_price ~ ., data = select(no_anoms, -central_air, -x45_yrs,
                                         -bsmnt_fin_sf, -brick_ext, -pool, -uffi_in))

# Based on backward elimination we end up with a fided effect model that controls for the year, lot area, enclosed parking spaces, and living area in square feet. This model explains 62% of the variance observed in the data.


# (5 pts) Assume that UFFI is a statistically significant predictor, on average, by how much do we expect UFFI to change the value of a property? Note that 1 indicates the presence of UFFI in the building.

# Under this assumption, we must work with model 6. On average, we can infer that the presence of UFFI reduces the value of a home by $7,382.


# (5 pts) If the home in question is 25 years old, has no finished basement, a lot area of 6270 square feet, 2 enclosed parking spaces, 3780 square feet of living space, no UFFI, central air, and a pool, what is its predicted value.
# (5 pts) What is the 95% prediction interval of the home above?

pred_mod <- lm(sale_price ~ x45_yrs + lot_area + living_area_sf +
                 enc_pk_spaces + uffi_in + central_air + pool, data = no_anoms)

new_dat <- tribble(
  ~x45_yrs, ~lot_area, ~living_area_sf, ~enc_pk_spaces, ~uffi_in, ~central_air, ~pool,
  0, 6270, 3780, 2, 0, 1, 1
) 


predict(pred_mod, new_dat, interval = "prediction")

# $333,126.3


