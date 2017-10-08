library(stringr)
library(dplyr)
library(maps)
library(ggplot2)
library(caret)

beers <- read.csv("beers.csv", stringsAsFactors = FALSE)
breweries <- read.csv("breweries.csv", stringsAsFactors = FALSE)
colnames(breweries)[1] <- "brewery_id"

# Clean up the string data a bit
beers$name <- str_trim(beers$name)
beers$style <- str_trim(beers$style)
breweries$name <- str_trim(breweries$name)
breweries$city <- str_trim(breweries$city)
breweries$state <- str_trim(breweries$state)

# Match brewery information to beer
beer.data <- merge(beers, breweries, by="brewery_id")
colnames(beer.data)[2] <- "beer_id"
colnames(beer.data)[6] <- "beer_name"
colnames(beer.data)[9] <- "brewery_name"

# Add location by city, state for easy geocoding.
beer.data <- mutate(beer.data, location = paste(city, state, sep = ", "))

# Generate data frames for choropleths
lo.abv <- filter(beer.data, abv < 0.045)
med.abv <- filter(beer.data, 0.045 <= abv, abv < 0.06)
hi.abv <- filter(beer.data, 0.06 <= abv, abv < 0.08)
vhi.abv <- filter(beer.data, 0.08 <= abv)

loabv.df <- data.frame(table(factor(lo.abv$state, levels = c(state.abb, "DC"))))
medabv.df <- data.frame(table(factor(med.abv$state, levels = c(state.abb, "DC"))))
hiabv.df <- data.frame(table(factor(hi.abv$state, levels = c(state.abb, "DC"))))
vhiabv.df <- data.frame(table(factor(vhi.abv$state, levels = c(state.abb, "DC") )))

# Prep for mapping
loabv.df$Var1 <- tolower(state.name[match(loabv.df$Var1, c(state.abb, "DC"))])
medabv.df$Var1 <- tolower(state.name[match(medabv.df$Var1, c(state.abb, "DC"))])
hiabv.df$Var1 <- tolower(state.name[match(hiabv.df$Var1, c(state.abb, "DC"))])
vhiabv.df$Var1 <- tolower(state.name[match(vhiabv.df$Var1, c(state.abb, "DC"))])
states.map <- map_data("state")

# Put together some choropleths
loabv.map <- ggplot(loabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Low Alcohol Beer (<4.5% ABV) by State")

medabv.map <- ggplot(medabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Medium Alcohol Beer (4.5-6% ABV) by State")

hiabv.map <- ggplot(hiabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "High Alcohol Beer (6-8% ABV) by State")

vhiabv.map <- ggplot(vhiabv.df, aes(map_id = Var1)) +
  geom_map(aes(fill = Freq), map = states.map) +
  expand_limits(x = states.map$long, y = states.map$lat) + 
  labs(x = "Longitude", y = "Latitude", 
       title = "Very High Alcohol Beer (>8% ABV) by State")

# Prep data to fit linear regression models to predict ABV
model.beer.data <- select(beer.data, abv, ibu, style,
                          brewery_name, city, state)
model.beer.data <- filter(model.beer.data, !is.na(abv))
model.beer.data$style <- factor(model.beer.data$style)
model.beer.data$brewery_name <- factor(model.beer.data$brewery_name)
model.beer.data$city <- factor(model.beer.data$city)
model.beer.data$state <- factor(model.beer.data$state)

# Create dummy variables for the regression
beer.dummy <- dummyVars(~ ., data = model.beer.data)
dummied.data <- 
  data.frame(predict(beer.dummy, newdata = model.beer.data))

# Identify and remove near zero variance predictors
nzv <- nearZeroVar(dummied.data)
beer.final <- dummied.data[,-nzv]

# Impute missing values for IBU
pp <- preProcess(beer.final, method = c("knnImpute"))
pp.beer.data <- predict(pp, newdata = beer.final)

# Split into training and test groups
set.seed(777)
idx <- createDataPartition(pp.beer.data$abv,
                           times = 1,
                           p = 0.85,
                           list = FALSE)
beer.train <- pp.beer.data[idx,]
beer.test <- pp.beer.data[-idx,]
beer.test.nolabel <- select(beer.test, -abv)
