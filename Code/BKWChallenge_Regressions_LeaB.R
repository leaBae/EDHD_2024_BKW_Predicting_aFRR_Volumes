


# Energy Data Hackdays #

# Sum of Positive / Negative Areas summed up to get imbalance per day


# Preparation ----

## 0.1 Load Packages ----
library(openxlsx)
library(readxl)
library(tidyverse)
library(randomForest)

## 0.2 Load Data ----
wd <- "/Users/leabachlin/Desktop/Hackdays/EDHD - BKW - Data Share"
trainingwd <- paste0(wd, "/Data/Training/")
testwd <- paste0(wd, "/Data/Test/")
intradaywd <- paste0(wd, "/Data/IntradayPrices/")


dat18 <- read.csv(paste0(trainingwd, "/afrr_activation_2018.csv"))
dat19 <- read.csv(paste0(trainingwd, "/afrr_activation_2019.csv"))
dat20 <- read.csv(paste0(trainingwd, "/afrr_activation_2020.csv"))
dat21 <- read.csv(paste0(trainingwd, "/afrr_activation_2021.csv"))
dat22 <- read.csv(paste0(trainingwd, "/afrr_activation_2022.csv"))
dat23 <- read.csv(paste0(trainingwd, "/afrr_activation_2023.csv"))

input18 <- read.csv(paste0(trainingwd, "/input_features_2018.csv"))
input19 <- read.csv(paste0(trainingwd, "/input_features_2019.csv"))
input20 <- read.csv(paste0(trainingwd, "/input_features_2020.csv"))
input21 <- read.csv(paste0(trainingwd, "/input_features_2021.csv"))
input22 <- read.csv(paste0(trainingwd, "/input_features_2022.csv"))
input23 <- read.csv(paste0(trainingwd, "/input_features_2023.csv"))

input19$X <- as.POSIXct(input19$X, format = "%d.%m.%Y %H:%M")
input19$X <- gsub("CET", "", input19$X)

intraprices18 <- read.csv(paste0(intradaywd, "intraday_2018.csv"))
intraprices19 <- read.csv(paste0(intradaywd, "/intraday_2019.csv"))
intraprices20 <- read.csv(paste0(intradaywd, "/intraday_2020.csv"))
intraprices21 <- read.csv(paste0(intradaywd, "/intraday_2021.csv"))
intraprices22 <- read.csv(paste0(intradaywd, "/intraday_2022.csv"))
intraprices23 <- read.csv(paste0(intradaywd, "/intraday_2023.csv"))

Linked2018 <- dat18 %>% 
  left_join(input18, by = c("TimeStamp" = "X")) %>% 
  left_join(intraprices18, by = c("TimeStamp" = "X")) %>% 
  mutate(Year = 2018)
Linked2019 <- dat19 %>% 
  left_join(input19, by = c("TimeStamp" = "X")) %>% 
  left_join(intraprices19, by = c("TimeStamp" = "X")) %>% 
  mutate(Year = 2019)
Linked2020 <- dat20 %>% 
  left_join(input20, by = c("TimeStamp" = "X")) %>% 
  left_join(intraprices20, by = c("TimeStamp" = "X")) %>% 
  mutate(Year = 2020)
Linked2021 <- dat21 %>% 
  left_join(input21, by = c("TimeStamp" = "X")) %>% 
  left_join(intraprices21, by = c("TimeStamp" = "X")) %>% 
  mutate(Year = 2021)
Linked2022 <- dat22 %>% 
  left_join(input22, by = c("TimeStamp" = "X")) %>% 
  left_join(intraprices22, by = c("TimeStamp" = "X")) %>% 
  mutate(Year = 2022)
Linked2023 <- dat23 %>% 
  left_join(input23, by = c("TimeStamp" = "X")) %>% 
  left_join(intraprices23, by = c("TimeStamp" = "X")) %>% 
  mutate(Year = 2023)

LinkedTotal <- bind_rows(Linked2018, Linked2019, Linked2020, Linked2021, Linked2022, Linked2023) %>% 
  select(Year, everything())
LinkedTotal$TimeStamp <- as.POSIXct(LinkedTotal$TimeStamp, format = "%Y-%m-%d %H:%M:%S")
LinkedTotal <- LinkedTotal %>% 
  mutate(Year = as.factor(year(LinkedTotal$TimeStamp)),
         Month = as.factor(months(TimeStamp)),
         Week = as.factor(week(TimeStamp)),
         Day = as.factor(wday(TimeStamp)), 
         Hour = as.factor(hour(LinkedTotal$TimeStamp)),
         Quarter = as.factor(minute(LinkedTotal$TimeStamp)),
         Weekday = as.factor(Weekday.x),
         Holidays_CH = as.factor(Holidays_CH),
         Holidays_DE = as.factor(Holidays_DE),
         Holidays_AT = as.factor(Holidays_AT),
         Holidays_IT = as.factor(Holidays_IT),
         Holidays_FR = as.factor(Holidays_FR)) %>% 
rename(CEVpos = ActivatedBalancingVolume_CH_aFRRPositive,
       CEVneg = ActivatedBalancingVolume_CH_aFRRNegative,
       Weather_CH_Irradiance = Weather_CH_GlobalIrradianceClearSky) %>% 
  mutate(Weather_CH_Irradiance_minus1 = lag(Weather_CH_Irradiance),
         Date = paste0(day(TimeStamp), "_", month(TimeStamp)))


write.csv(LinkedTotal, paste0(wd, "/DataLinked.csv"))




## Irradiance ----

LinkedT_Day <- LinkedTotal %>% 
  filter(!Hour %in% c(1:7, 19:24))
LinkedT_Day_first <- LinkedT_Day %>% 
  filter(Hour %in% c(7:13))
LinkedT_Day_second <- LinkedT_Day %>% 
  filter(Hour %in% c(14:19))
LinkedT_Day_squareroot <- LinkedTotal %>% 
  mutate(Weather_CH_Irradiance_sqrt = sqrt(Weather_CH_Irradiance),
         Weather_CH_Irradiance_minus1_sqrt = sqrt(Weather_CH_Irradiance_minus1))

dataset =  LinkedT_Day

lmirradiancepos <- lm(CEVpos ~ Weather_CH_Irradiance + Weather_CH_Irradiance_minus1, data = dataset)
summary(lmirradiancepos)

lmirradianceneg <- lm(abs(CEVneg) ~ Weather_CH_Irradiance + Weather_CH_Irradiance_minus1, data = dataset)
summary(lmirradianceneg)

# Bessere Vorhersage in erster Hälfte (grösseres Beta)


plot(LinkedTotal$Weather_CH_Irradiance, LinkedTotal$ActivatedBalancingVolume_CH_aFRRPositive)
abline(a = lmirradiancepos$coefficients[1], b = lmirradiancepos$coefficients[2], col = "orange")

plot(LinkedTotal$Weather_CH_Irradiance_minus1, LinkedTotal$CEVpos)
abline(a = lmirradiancepos$coefficients[1], b = lmirradiancepos$coefficients[3], col = "red")

plot(LinkedTotal$Weather_CH_Irradiance, abs(LinkedTotal$CEVneg))
abline(a = lmirradianceneg$coefficients[1], b = lmirradianceneg$coefficients[2], col = "green")

plot(LinkedTotal$Weather_CH_Irradiance_minus1, abs(LinkedTotal$CEVneg))
abline(a = lmirradianceneg$coefficients[1], b = lmirradianceneg$coefficients[3], col = "blue")




## Rain ----


LinkedTotalRain <- LinkedTotal %>% 
  mutate(Weather_CH_DailyPrecipitationEnergy_lag = lag(Weather_CH_DailyPrecipitationEnergy))


dataset =  LinkedTotalRain

lmirainpos <- lm(CEVpos ~ Weather_CH_DailyPrecipitationEnergy + Weather_CH_DailyPrecipitationEnergy_lag, data = dataset)
summary(lmirainpos)

lmirainneg <- lm(abs(CEVneg) ~ Weather_CH_DailyPrecipitationEnergy + Weather_CH_DailyPrecipitationEnergy_lag, data = dataset)
summary(lmirainneg)


# Energy produced via rain only predicts the positive energy, so the surplus


plot(LinkedTotal$Weather_CH_DailyPrecipitationEnergy, LinkedTotal$CEVneg)
abline(a = lmirainpos$coefficients[1], b = lmirainpos$coefficients[2], col = "orange")

plot(LinkedTotal$Weather_CH_DailyPrecipitationEnergy_lag, LinkedTotal$CEVpos)
abline(a = lmirainpos$coefficients[1], b = lmirainpos$coefficients[3], col = "red")

plot(LinkedTotal$Weather_CH_DailyPrecipitationEnergy, abs(LinkedTotal$CEVneg))
abline(a = lmirainneg$coefficients[1], b = lmirainneg$coefficients[2], col = "green")

plot(LinkedTotal$Weather_CH_DailyPrecipitationEnergy_lag, abs(LinkedTotal$CEVneg))
abline(a = lmirainneg$coefficients[1], b = lmirainneg$coefficients[3], col = "blue")


## Temperature ----

LinkedTotalTemp <- LinkedTotal %>% 
  mutate(Weather_CH_DailyMeanTemperature_lag = lag(Weather_CH_DailyMeanTemperature))

## Prices ----

LinkedTotalPrice <- LinkedTotal %>% 
  mutate(Prices_CH_Dayahead_lag = lag(Prices_CH_Dayahead))


## General Loop ----

var <- "Weather_CH_DailyMeanTemperature"
dataset <- LinkedTotalTemp
formulapos <- as.formula(paste0("CEVpos", "~", var, "+", var, "_lag"))
formulaneg <- as.formula(paste0("abs(CEVneg)", "~", var, "+", var, "_lag"))

cor(dataset[, var], dataset$CEVpos, use = "pairwise.complete.obs")
cor(dataset[, paste0(var, "_lag")], dataset$CEVpos, use = "pairwise.complete.obs")
cor(dataset[, var], abs(dataset$CEVneg), use = "pairwise.complete.obs")
cor(dataset[, paste0(var, "_lag")], abs(dataset$CEVneg), use = "pairwise.complete.obs")

lmpos <- lm(formulapos, data = dataset)
summary(lmpos)

lmneg <- lm(formulaneg, data = dataset)
summary(lmneg)


plot(dataset[, var], dataset$CEVneg)
abline(a = lmpos$coefficients[1], b = lmpos$coefficients[2], col = "orange")
plot(LinkedTotal[, paste0(var, "_lag")], dataset$CEVpos)
abline(a = lmpos$coefficients[1], b = lmpos$coefficients[3], col = "red")

plot(dataset[, var], abs(dataset$CEVneg))
abline(a = lmneg$coefficients[1], b = lmneg$coefficients[2], col = "green")
plot(LinkedTotal[, paste0(var, "_lag")], abs(dataset$CEVneg))
abline(a = lmneg$coefficients[1], b = lmneg$coefficients[3], col = "blue")


boxplot(dataset$CEVpos)
boxplot(abs(dataset$CEVneg))
boxplot(dataset[var])



## Time Models -----
# List of variables
vars <- c("Month", "Day", "Hour", "Quarter", "Holidays_CH", "Weekday")

# Function to generate all combinations of variable interactions
generate_formulas <- function(vars, outputvar) {
  formulas <- list()
  idx <- 1
  
  # Loop over all combinations of interaction orders (1, 2, 3, 4)
  for (i in 1:length(vars)) {
    # Generate combinations of 'i' variables from the list
    for (comb in combn(vars, i, simplify = FALSE)) {
      interaction_term <- paste(comb, collapse = " * ")  # Create full interaction term
      remaining_vars <- setdiff(vars, comb)  # Remaining variables for the additive term
      
      # If there are remaining variables, create an additive term
      if (length(remaining_vars) > 0) {
        additive_term <- paste(remaining_vars, collapse = " + ")
        full_formula <- paste(outputvar, "~", interaction_term, "+", additive_term)  # Combine interaction and additive terms
      } else {
        # If no remaining variables, just use the interaction term
        full_formula <- paste(outputvar, "~", interaction_term)
      }
      
      formulas[[idx]] <- as.formula(full_formula)  # Store the formula
      idx <- idx + 1
    }
  }
  
  return(formulas)
}

# Generate the formulas
formulaspos <- generate_formulas(vars, "CEVpos")
formulasneg <- generate_formulas(vars, "CEVneg")



# Loop Positive ----
counter <- 0
vecpos <- c()
for (formula in formulaspos) {
  counter <- counter + 1
  lpos <- lm(formula, data = LinkedTotal)
  lpos_summary <- summary(lpos)
  vecpos[counter] <- round(lpos_summary$r.squared, 4)
} 

# Loop Negative ----
counter <- 0
vecneg <- c()
for (formula in formulasneg) {
  counter <- counter + 1
  lpos <- lm(formula, data = LinkedTotal)
  lpos_summary <- summary(lpos)
  vecneg[counter] <- round(lpos_summary$r.squared, 4)
} 

formulabestpos <- formulaspos[which(vecpos == max(vecpos))]
formulabestneg <- formulasneg[which(vecneg == max(vecneg))]
# Both times, the simple additive model is the best; but better for positive than negative data



generate_formulas_nextstep <- function(vars, outputvar) {
  formulas <- list()
  idx <- 1
  
  # Loop over all combinations of interaction orders (1, 2, 3, 4)
  for (i in 1:length(vars)) {
    # Generate combinations of 'i' variables from the list
    for (comb in combn(vars, i, simplify = FALSE)) {
      interaction_term <- paste(comb, collapse = " * ")  # Create full interaction term
      remaining_vars <- setdiff(vars, comb)  # Remaining variables for the additive term
      
      # If there are remaining variables, create an additive term
      if (length(remaining_vars) > 0) {
        additive_term <- paste(remaining_vars, collapse = " + ")
        full_formula <- paste(outputvar, "~ Month + Day + Hour * Quarter + Holidays_CH + Weekday +", interaction_term, "+", additive_term)  # Combine interaction and additive terms
      } else {
        # If no remaining variables, just use the interaction term
        full_formula <- paste(outputvar, "~ Month + Day + Hour * Quarter + Holidays_CH + Weekday +", interaction_term)
      }
      
      formulas[[idx]] <- as.formula(full_formula)  # Store the formula
      idx <- idx + 1
    }
  }
  
  return(formulas)
}


vars <- c("Weather_CH_DailyMeanTemperature", "Prices_CH_Dayahead", "Consumption_CH_Total",
          "Weather_CH_Irradiance", "Weather_CH_DailyPrecipitationEnergy")
formulaspos_next <- generate_formulas_nextstep(vars, "CEVpos")
formulaspneg_next <- generate_formulas_nextstep(vars, "CEVneg")

# Loop Positive ----
counter <- 0
vecposnext <- c()
for (formula in formulaspos_next) {
  print(formula)
  counter <- counter + 1
  lpos <- lm(formula, data = LinkedTotal)
  lpos_summary <- summary(lpos)
  vecposnext[counter] <- round(lpos_summary$r.squared, 4)
} 

# Loop Negative ----
counter <- 0
vecnegnext <- c()
for (formula in formulaspneg_next) {
  print(formula)
  counter <- counter + 1
  lpos <- lm(formula, data = LinkedTotal)
  lpos_summary <- summary(lpos)
  vecnegnext[counter] <- round(lpos_summary$r.squared, 4)
} 

formulabestposnext <- formulaspos_next[which(vecposnext == max(vecposnext))]
formulabestnegnext <- formulaspneg_next[which(vecnegnext == max(vecnegnext))]





## Random Forest -----

rf_model <- randomForest(CEVpos ~ ., data = LinkedTotal[1:5000,!colnames(LinkedTotal) %in% c("CEVneg")], importance = TRUE, na.action = na.omit)

# View importance of variables
importance(rf_model)
varImpPlot(rf_model)





