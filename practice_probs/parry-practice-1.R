library(tidyverse)

df <- read_csv("/Users/Josiah/Downloads/customertxndata.csv",
               col_names = c("nv","nt","os","mf","rev"))

# calculates na's means and sds
summary(df)

ggplot(df, aes(mf, rev)) +
  geom_bar(stat = "identity")




# (5 pts) What is the Pearson Moment of Correlation between number of visits and revenue? Comment on the correlation.
cor(df$rev, df$nv)
# (10 pts) Which columns have missing data? How did you recognize them? How would you impute missing values?
summary(df) # look at the NAs

#   (15 pts) Impute missing transaction and gender values. Use the mean for transaction (rounded to the nearest whole number) and the mode for gender.
df <- mutate(df, 
       nt = ifelse(is.na(nt),
                   yes = round(mean(nt, na.rm = TRUE), 0),
                   no = nt),
       mf = ifelse(is.na(mf), "Male", mf)
)


# (20 pts) Split the data set into two equally sized data sets where one can be used for training a model and the other for validation. Take every odd numbered case and add them to the training data set and every even numbered case and add them to the validation data set, i.e., row 1, 3, 5, 7, etc. are training data while rows 2, 4, 6, etc. are validation data.
index <- 1:nrow(df)
evens <- index %% 2 == 0
odds <- index %% 2 == 1

training <- slice(df, index[odds])
testing <- slice(df, index[evens])
# (10 pts) Calculate the mean revenue for the training and the validation data sets and compare them. Comment on the difference.
mean(training$rev)
mean(testing$rev)

# (15 pts) For many data mining and machine learning tasks, there are packages in R. Use the sample() function to split the data set, so that 60% is used for training and 20% is used for testing, and another 20% is used for validation. To ensure that your code is reproducible and that everyone gets the same result, use the number 77654 as your seed for the random number generator. Use the code fragment below for reference:
set.seed(0)

df <- mutate(df, id = row_number()) # add id for referencing

analysis <- sample_frac(df, 0.6)
assessment <- sample_frac(anti_join(df, analysis), .2)

validation <- anti_join(df, analysis) %>% 
  anti_join(assessment)
