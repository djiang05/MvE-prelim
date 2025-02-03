# Data Sources:
# USDX: https://www.statista.com/statistics/1404145/us-dollar-index-historical-chart/
# Pound Sterling PPP: https://fred.stlouisfed.org/series/PPPTTLGBA618NUPN
# Conflict: https://correlatesofwar.org/data-sets/cow-war/
# Military Spending: https://ourworldindata.org/grapher/military-spending-sipri?tab=chart&country=GBR~USA
# US federal interest rates: https://www.statista.com/statistics/187616/effective-rate-of-us-federal-funds-monthly/
# UK federal interest rates: https://fred.stlouisfed.org/series/INTGSBGBM193N

library(dplyr) # Pipes and general data manipulation
library(countries) # auto merge
library(ggplot2) # Plots

library(randomForest) # Random Forest Libraries
library(rpart.plot)
library(rpart)
library(caret)

# Loading all data
USDX <- read.csv("/Users/djiang/Documents/R stuff/Final Project/USDX - (US).csv")
conflict_us <- read.csv("/Users/djiang/Documents/R stuff/Final Project/conflict - (US).csv")
military_expenditure_us <- read.csv("/Users/djiang/Documents/R stuff/Final Project/Military Expenditure - (US).csv")
interest_rate_us <- read.csv("/Users/djiang/Documents/R stuff/Final Project/Interest Rate - (US).csv")

PPP_UK <- read.csv("/Users/djiang/Documents/R stuff/Final Project/PPP - (UK).csv")
conflict_uk <- read.csv("/Users/djiang/Documents/R stuff/Final Project/conflict - (UK).csv")
military_expenditure_uk <- read.csv("/Users/djiang/Documents/R stuff/Final Project/Military Expenditure - (UK).csv")
interest_rate_uk <- read.csv("/Users/djiang/Documents/R stuff/Final Project/Interest Rate - (UK).csv")

# Convert monthly data to yearly average data
USDX <- USDX %>% group_by(Year) %>% summarize(USDX = mean(USDX), .groups = 'drop')
interest_rate_us <- interest_rate_us %>% group_by(Year) %>% summarize(interest_rate = mean(interest_rate), .groups = 'drop')
interest_rate_uk <- interest_rate_uk %>% group_by(Year) %>% summarize(interest_rate = mean(interest_rate), .groups = 'drop')

# Merge datasets into UK and US cumulative datasets
US <- na.omit(auto_merge(USDX, conflict_us, military_expenditure_us, interest_rate_us, by = c("Year"))) %>%
    as_tibble() %>%
    mutate(across(c(2, 4, 5), scale)) # Standardize numeric IVs, given they're measured across wildly different scales
UK <- na.omit(auto_merge(PPP_UK, conflict_uk, military_expenditure_uk, interest_rate_uk, by = c("Year"))) %>%
    as_tibble() %>%
    mutate(across(c(2, 4, 5), scale))

# Regressions
summary(lm(USDX ~ conflict + military_expenditure + interest_rate, data=US))
summary(lm(PPP ~ conflict + military_expenditure + interest_rate, data=UK))



# Random Forest analysis to additionally gauge the value of IVs in predicting DVs
# Want to use the IVs to predict whether dollar strength goes up or down in a given year
set.seed(2024) # for reproducability

US <- na.omit(auto_merge(USDX, conflict_us, military_expenditure_us, interest_rate_us, by = c("Year"))) # Get the unstandardized dataset again to operate on the raw values
US$del_USDX <- c(NA, diff(US$USDX)) # Get a variable that indicates the change in USDX from year to year
US$growth_USDX <- ifelse(US$del_USDX > 0, 1, 0) # Get a binary variable that indicates whether or not there was economic growth as indicated by the USDX
US$growth_USDX <- as.factor(US$growth_USDX) # turn into factor for random forest analysis

US <- US[, c(3, 4, 5, 7)] # Get only the variables wanted for random forest analysis; drop year, USDX, and del_USDX
US <- na.omit(US) %>% as_tibble() %>% mutate(across(c(2, 3), scale)) # Restandardize IVs in preparation for variable importance plot

# Create training and testing datasets
inTrain <- createDataPartition(US$growth_USDX, 
                               p = .8, 
                               list = FALSE, 
                               times = 1)
train<- US[inTrain,]
test<- US[-inTrain,]

# Create random forest
forest <- randomForest(growth_USDX~., data = train, ntree=250, importance=T, na.action=na.omit, mtry=4)
# Graph 1: Variable importance plot
varImpPlot(forest, type=1, col=3, sort = T, cex=1, pch=16, main="USDX Growth Random Forest")

# Repeat procedure for Pound Sterling
UK <- na.omit(auto_merge(PPP_UK, conflict_uk, military_expenditure_uk, interest_rate_uk, by = c("Year"))) # Get the unstandardized dataset again to operate on the raw values
UK$del_PPP <- c(NA, diff(UK$PPP)) # Get a variable that indicates the change in PPP from year to year
UK$growth_PPP <- ifelse(UK$del_PPP > 0, 1, 0) # Get a binary variable that indicates whether or not there was economic growth as indicated by the PPP
UK$growth_PPP <- as.factor(UK$growth_PPP) # turn into factor for random forest analysis

UK <- UK[, c(3, 4, 5, 7)] # Get only the variables wanted for random forest analysis; drop year, USDX, and del_USDX
UK <- na.omit(UK) %>% as_tibble() %>% mutate(across(c(2, 3), scale)) # Restandardize IVs in preparation for variable importance plot

# Create training and testing datasets
inTrain <- createDataPartition(UK$growth_PPP, 
                               p = .8, 
                               list = FALSE, 
                               times = 1)
train<- UK[inTrain,]
test<- UK[-inTrain,]

# Create random forest
forest <- randomForest(growth_PPP~., data = train, ntree=250, importance=T, na.action=na.omit, mtry=4)
# Graph 2: Variable importance plot
varImpPlot(forest, type=1, col=2, sort = T, cex=1, pch=16, main="Pound Sterling PPP Growth Random Forest")


