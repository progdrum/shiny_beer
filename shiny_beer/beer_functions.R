library(stringr)
library(dplyr)
library(maps)
library(ggplot2)


# Read in the data and clean it up for our purposes
read_and_clean <- function(beer_file, brew_file) {
  beers <- read.csv(beer_file, stringsAsFactors = FALSE)
  breweries <- read.csv(brew_file, stringsAsFactors = FALSE)
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
  
  return(beer.data)
}

# Break out data for ABV ranges
get_abv_dfs <- function(bdata) {
  # Generate data frames for choropleths
  lo.abv <- filter(bdata, abv < 0.045)
  med.abv <- filter(bdata, 0.045 <= abv, abv < 0.06)
  hi.abv <- filter(bdata, 0.06 <= abv, abv < 0.08)
  vhi.abv <- filter(bdata, 0.08 <= abv)
  
  loabv.df <- data.frame(table(factor(lo.abv$state, levels = c(state.abb, "DC"))))
  medabv.df <- data.frame(table(factor(med.abv$state, levels = c(state.abb, "DC"))))
  hiabv.df <- data.frame(table(factor(hi.abv$state, levels = c(state.abb, "DC"))))
  vhiabv.df <- data.frame(table(factor(vhi.abv$state, levels = c(state.abb, "DC") )))
  
  # Prep for mapping
  loabv.df$Var1 <- tolower(state.name[match(loabv.df$Var1, c(state.abb, "DC"))])
  medabv.df$Var1 <- tolower(state.name[match(medabv.df$Var1, c(state.abb, "DC"))])
  hiabv.df$Var1 <- tolower(state.name[match(hiabv.df$Var1, c(state.abb, "DC"))])
  vhiabv.df$Var1 <- tolower(state.name[match(vhiabv.df$Var1, c(state.abb, "DC"))])
  
  return(list("lo" = loabv.df, "med" = medabv.df, "hi" = hiabv.df, "vhi" = vhiabv.df))
}

make_maps <- function(abv_dfs) {
  # Load the map data
  mdata <- map_data("state")
  
  # Put together some choropleths
  loabv.map <- ggplot(abv_dfs$lo, aes(map_id = Var1)) +
    geom_map(aes(fill = Freq), map = mdata) +
    expand_limits(x = mdata$long, y = mdata$lat) + 
    labs(x = "Longitude", y = "Latitude", 
         title = "Low Alcohol Beer (<4.5% ABV) by State")
  
  medabv.map <- ggplot(abv_dfs$med, aes(map_id = Var1)) +
    geom_map(aes(fill = Freq), map = mdata) +
    expand_limits(x = mdata$long, y = mdata$lat) + 
    labs(x = "Longitude", y = "Latitude", 
         title = "Medium Alcohol Beer (4.5-6% ABV) by State")
  
  hiabv.map <- ggplot(abv_dfs$hi, aes(map_id = Var1)) +
    geom_map(aes(fill = Freq), map = mdata) +
    expand_limits(x = mdata$long, y = mdata$lat) + 
    labs(x = "Longitude", y = "Latitude", 
         title = "High Alcohol Beer (6-8% ABV) by State")
  
  vhiabv.map <- ggplot(abv_dfs$vhi, aes(map_id = Var1)) +
    geom_map(aes(fill = Freq), map = mdata) +
    expand_limits(x = mdata$long, y = mdata$lat) + 
    labs(x = "Longitude", y = "Latitude", 
         title = "Very High Alcohol Beer (>8% ABV) by State")
  
  return(list("lo" = loabv.map, 
              "med" = medabv.map, 
              "hi" = hiabv.map, 
              "vhi" = vhiabv.map))
}
