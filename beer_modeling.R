# A script that provides the skeleton for parallel model training with caret

library(caret)
library(doParallel)

# This is how to set up the seeds (numbers change depending on tuning parameters)
set.seed(777)
seeds <- vector(mode = "list", length = 11)

# CV models
for(k in 1:10)
  seeds[[k]] <- sample.int(1000, 1)

# Final model
seeds[[11]] <- sample.int(1000, 1)

# Everything from here on may vary depending on what I'm training.
# Change these as I like and save in different places where they are used.
par.control <- trainControl(method = "cv", seeds = seeds, number = 10)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

beer.lm1 <- train(abv ~ ., 
                  method = "lm", 
                  data = pp.beer.data, 
                  trControl = par.control)

# Can I add some sort of error-handling here to ensure that cleanup happens?
stopCluster(cl)

# Create predictions for unlabeled data
predictions <- predict(beer.lm1, newdata = beer.test.nolabel)
postResample(predictions, beer.test$abv)
