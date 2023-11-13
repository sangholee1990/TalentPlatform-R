#Data Wrangling

library(tidyverse)
library(openxlsx)
library(dplyr)

# rm(list = ls())

#Load
# setwd("C:\\Users\\yanni\\OneDrive\\Documents\\R\\Exam Econometrie & R\\Project\\Data\\")



#Data_wrangling
#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________
#Total_Crop_Production
Total_Crop_production <- read_tsv("Total crop production.tsv")
#Separate the first column into Crop, Structure of production, Country

# Split the InfoColumn into three columns
Total_Crop_production <- separate(data = Total_Crop_production, col = `crops,strucpro,geo\\time`, into = c("CropType", "StructureOfProduction", "Country"), sep = ",")

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("CropType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Total_Crop_production <- Total_Crop_production %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
Total_Crop_production <- Total_Crop_production %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Total_Crop_production <- Total_Crop_production %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Total_Crop_production <- Total_Crop_production %>%
  pivot_longer(cols = -c(Country, CropType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Total_Crop_production$Production <- as.numeric(Total_Crop_production$Production)
Total_Crop_production <- Total_Crop_production %>%
  group_by(Country, CropType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

#Get rid of crop type
Total_Crop_production <- Total_Crop_production[-1]

# Group by the "Country" column and then summarize the numerical columns by summing them
Total_Crop_production <- Total_Crop_production %>%
  group_by(Country, Year) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________

#Total_Emissions
Total_Emissions <- read.csv("Total_Emissions_Full3.csv")
class(Total_Emissions)
names(Total_Emissions)

Total_Emissions <- Total_Emissions[c(4, 10, 14)]
Total_Emissions <- Total_Emissions %>% rename(Country = Area,
                                              Emissions = Value)

# Group by the "Country" column and then summarize the numerical columns by summing them
Total_Emissions <- Total_Emissions %>%
  group_by(Country, Year) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________

#Organic_Crop_production_by_crops
Organic_Crop_production_by_crops <- read_tsv("Organic crop production by crops.tsv")

#Total_Crop_Production
class(Organic_Crop_production_by_crops)
#Separate the first column into Crop, Structure of production, Country
# Split the InfoColumn into three columns
Organic_Crop_production_by_crops <- separate(data = Organic_Crop_production_by_crops, col = `crops,unit,geo\\time`, into = c("CropType", "StructureOfProduction", "Country"), sep = ",")

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("CropType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>%
  select(all_of(columns_to_keep))


#Use filter to keep only Sweden, Poland and Spain
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))


Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>%
  pivot_longer(cols = -c(Country, CropType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Organic_Crop_production_by_crops$Production <- as.numeric(Organic_Crop_production_by_crops$Production)
imputed_data <- Organic_Crop_production_by_crops %>%
  group_by(Country, CropType) %>%
  mutate(production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Example t-test to compare means
t.test(Organic_Crop_production_by_crops$Production, imputed_data$production)

#Implement linear imputation
imputed_data <- Organic_Crop_production_by_crops %>%
  group_by(Country, CropType) %>%
  mutate(production = ifelse(is.na(Production),
                             predict(lm(Production ~ Year, data = .), newdata = .),
                             Production))

any(is.na(imputed_data$production))

# Example t-test to compare means
t.test(Organic_Crop_production_by_crops$Production, imputed_data$production)

#Apply it to original dataset
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>%
  group_by(Country, CropType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

#Get rid of crop type
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops[-1]

# Group by the "Country" column and then summarize the numerical columns by summing them
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>%
  group_by(Country, Year) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

class(Organic_Crop_production_by_crops)


#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________

setwd("C:\\Users\\yanni\\OneDrive\\Documents\\R\\Exam Econometrie & R\\Project\\Data\\")
Pig_head <- read_tsv("Number of pigs.tsv")

# Split the InfoColumn into three columns
Pig_head <- separate(data = Pig_head, col = `month,animals,unit,geo\\time`, into = c("Month", "AnimalType", "Production", "Country"), sep = ",")

names(Pig_head)

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("AnimalType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")
# Use select and select to keep the specified columns
Pig_head <- Pig_head %>%
  select(all_of(columns_to_keep))


#Use filter to keep only Sweden, Poland and Spain
Pig_head <- Pig_head %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Pig_head <- Pig_head %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))


Pig_head <- Pig_head %>%
  pivot_longer(cols = -c(Country, AnimalType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Pig_head$Production <- as.numeric(Pig_head$Production)

imputed_data <- Pig_head %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Example t-test to compare means
t.test(Pig_head$Production, imputed_data$production)

# Group by the "Country" column and then summarize the numerical columns by summing them
Pig_head <- Pig_head %>%
  group_by(Country, Year, AnimalType) %>%
  summarize_all(.funs = sum, na.rm = TRUE)



#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________

#Dairy Cows
Dairy_head <- read_tsv("Number of dairy cows.tsv")

# Split the InfoColumn into three columns
Dairy_head <- separate(data = Dairy_head, col = `animals,month,unit,geo\\time`, into = c("AnimalType", "Month", "Production", "Country"), sep = ",")

names(Dairy_head)

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("AnimalType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Dairy_head <- Dairy_head %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
Dairy_head <- Dairy_head %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Dairy_head <- Dairy_head %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Dairy_head <- Dairy_head %>%
  pivot_longer(cols = -c(Country, AnimalType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Dairy_head$Production <- as.numeric(Dairy_head$Production)

imputed_data <- Dairy_head %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Example t-test to compare means
t.test(Dairy_head$Production, imputed_data$Production)

Dairy_head <- Dairy_head %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Group by the "Country" column and then summarize the numerical columns by summing them
Dairy_head <- Dairy_head %>%
  group_by(Country, Year, AnimalType) %>%
  summarize_all(.funs = sum, na.rm = TRUE)



#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________


setwd("C:\\Users\\yanni\\OneDrive\\Documents\\R\\Exam Econometrie & R\\Project\\Data\\")
Sheep_head <- read_tsv("Number of sheep.tsv")


# Split the InfoColumn into three columns
Sheep_head <- separate(data = Sheep_head, col = `animals,month,unit,geo\\time`, into = c("AnimalType", "Month", "Production", "Country"), sep = ",")

names(Sheep_head)

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("AnimalType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Sheep_head <- Sheep_head %>%
  select(all_of(columns_to_keep))


#Use filter to keep only Sweden, Poland and Spain
Sheep_head <- Sheep_head %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Sheep_head <- Sheep_head %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Sheep_head <- Sheep_head %>%
  pivot_longer(cols = -c(Country, AnimalType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Sheep_head$Production <- as.numeric(Sheep_head$Production)

imputed_data <- Sheep_head %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Example t-test to compare means
t.test(Sheep_head$Production, imputed_data$Production)

Sheep_head <- Sheep_head %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Group by the "Country" column and then summarize the numerical columns by summing them
Sheep_head <- Sheep_head %>%
  group_by(Country, Year, AnimalType) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________

Bovine_head <- read_tsv("Number of bovine animals.tsv")


# Split the InfoColumn into three columns
Bovine_head <- separate(data = Bovine_head, col = `animals,month,unit,geo\\time`, into = c("AnimalType", "Month", "Production", "Country"), sep = ",")

names(Bovine_head)

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("AnimalType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")
# Use select and select to keep the specified columns
Bovine_head <- Bovine_head %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
Bovine_head <- Bovine_head %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Bovine_head <- Bovine_head %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Bovine_head <- Bovine_head %>%
  pivot_longer(cols = -c(Country, AnimalType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Bovine_head$Production <- as.numeric(Bovine_head$Production)

imputed_data <- Bovine_head %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Example t-test to compare means
t.test(Bovine_head$Production, imputed_data$Production)

Bovine_head <- Bovine_head %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Group by the "Country" column and then summarize the numerical columns by summing them
Bovine_head <- Bovine_head %>%
  group_by(Country, Year, AnimalType) %>%
  summarize_all(.funs = sum, na.rm = TRUE)


#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________


#Total_Poultry
Total_Poultry <- read_tsv("Poultry.tsv")

#Total_Poultry
class(Total_Poultry)

# Split into three columns
Total_Poultry <- separate(data = Total_Poultry, col = `animals,unit,geo\\time`, into = c("AnimalType", "StructureOfProduction", "Country"), sep = ",")

# Specify the columns you want to keep (change these to match your column names)
names(Total_Poultry)
columns_to_keep <- c("AnimalType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Total_Poultry <- Total_Poultry %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
Total_Poultry <- Total_Poultry %>% filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Total_Poultry <- Total_Poultry %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Total_Poultry <- Total_Poultry %>%
  pivot_longer(cols = -c(Country, AnimalType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Total_Poultry$Production <- as.numeric(Total_Poultry$Production)
imputed_data <- Total_Poultry %>%
  group_by(Country, AnimalType) %>%
  mutate(production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Example t-test to compare means
t.test(Total_Poultry$Production, imputed_data$production)

Total_Poultry <- Total_Poultry %>%
  group_by(Country, AnimalType) %>%
  mutate(Production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production)) %>% 
  mutate(AnimalType = "Poultry")

# Group by the "Country" column and then summarize the numerical columns by summing them
Total_Poultry <- Total_Poultry %>%
  group_by(Country, Year, AnimalType) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________

#Total_Livestock
# Combine the datasets and aggregate the Production values

Bovine_head <- Bovine_head %>% mutate(Year = as.double(Year))
Sheep_head <- Sheep_head %>% mutate(Year = as.double(Year))
Dairy_head <- Dairy_head %>% mutate(Year = as.double(Year))
Pig_head <- Pig_head %>% mutate(Year = as.double(Year))
Total_Poultry <- Total_Poultry %>% mutate(Year = as.double(Year))

combined_data <- bind_rows(Bovine_head, Sheep_head, Dairy_head, Pig_head, Total_Poultry) %>%
  group_by(AnimalType, Country, Year) %>%
  summarise(Production = sum(Production, na.rm = TRUE)) %>%
  ungroup()

#Rename values
combined_data <- combined_data %>%
  mutate(AnimalType = case_when(
    AnimalType == "A2000" ~ "Bovine",
    AnimalType == "A4100" ~ "Sheep",
    AnimalType == "A2300F" ~ "Dairy Cow",
    AnimalType == "A3100" ~ "Pig",
    TRUE ~ AnimalType
  ))

Total_livestock_per_animal <- combined_data

Total_livestock <- Total_livestock_per_animal[-1]
# Group by the "Country" column and then summarize the numerical columns by summing them
Total_livestock$Production <- as.numeric(Total_livestock$Production)
Total_livestock <- Total_livestock %>%
  group_by(Country, Year) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

# Group by the "Country" column and then summarize the numerical columns by summing them
Total_livestock_per_animal$Production <- as.numeric(Total_livestock_per_animal$Production)
Total_livestock_per_animal$Year <- as.numeric(Total_livestock_per_animal$Year)
Total_livestock_per_animal <- Total_livestock_per_animal %>%
  group_by(Country, Year, AnimalType) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

Total_livestock_per_animal <- Total_livestock_per_animal %>%
  group_by(Country, Year) %>%
  summarize()

#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________


Energy_productivity <- read_tsv("Energy productivity.tsv")

# Split the InfoColumn into three columns
Energy_productivity <- separate(data = Energy_productivity, col = `unit,geo\\time`, into = c("Energy", "Country"), sep = ",")

names(Energy_productivity)

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("Energy", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Energy_productivity <- Energy_productivity %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
Energy_productivity <- Energy_productivity %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Energy_productivity <- Energy_productivity %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))
Energy_productivity <- Energy_productivity %>%
  mutate(across(starts_with("20"), as.double))

Energy_productivity <- Energy_productivity %>%
  pivot_longer(cols = -c(Country, Energy), names_to = "Year", values_to = "Productivity")

Energy_productivity <- Energy_productivity[-1]

# Group by the "Country" column and then summarize the numerical columns by summing them
Energy_productivity <- Energy_productivity %>%
  group_by(Country, Year) %>%
  summarize_all(.funs = sum, na.rm = TRUE)


#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________


#Organic Livestock
Organic_livestock <- read_tsv("Organic livestock.tsv")

#Total_Crop_Production
class(Organic_livestock)
#Separate the first column into Crop, Structure of production, Country
# Split the InfoColumn into three columns
Organic_livestock <- separate(data = Organic_livestock, col = `animals,unit,geo\\time`, into = c("AnimalType", "StructureOfProduction", "Country"), sep = ",")

# Specify the columns you want to keep (change these to match your column names)
names(Organic_livestock)
columns_to_keep <- c("AnimalType", "Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Organic_livestock <- Organic_livestock %>%
  select(all_of(columns_to_keep))

Organic_livestock <- Organic_livestock %>%
  mutate(AnimalType = case_when(
    AnimalType == "A1100" ~ "Live horses",
    AnimalType == "A2000" ~ "Bovine",
    AnimalType == "A2000B" ~ "Bovine",
    AnimalType == "A2300F" ~ "Dairy Cows",
    AnimalType == "A2900" ~ "Bovine",
    AnimalType == "A3100" ~ "Pig",
    AnimalType == "A3120" ~ "Pig",
    AnimalType == "A3130" ~ "Pig",
    AnimalType == "A3132" ~ "Pig",
    AnimalType == "A4100" ~ "Sheep",
    AnimalType == "A4110" ~ "Sheep",
    AnimalType == "A4110K" ~ "Sheep",
    AnimalType == "A4120" ~ "Sheep",
    AnimalType == "A4200" ~ "Sheep",
    AnimalType == "A4210" ~ "Sheep",
    AnimalType == "A4220" ~ "Sheep",
    AnimalType == "A5000" ~ "Poultry",
    TRUE ~ Country
  ))


#Use filter to keep only Sweden, Poland and Spain
Organic_livestock <- Organic_livestock %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))
Organic_livestock <- Organic_livestock %>%  filter(AnimalType %in% c("Bovine","Sheep","Poultry","Pig", "Dairy Cows"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Organic_livestock <- Organic_livestock %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Organic_livestock <- Organic_livestock %>%
  pivot_longer(cols = -c(Country, AnimalType), names_to = "Year", values_to = "Production")

# Perform mean imputation for missing production values
Organic_livestock$Production <- as.numeric(Organic_livestock$Production)
imputed_data <- Organic_livestock %>%
  group_by(Country, AnimalType) %>%
  mutate(production = ifelse(is.na(Production), mean(Production, na.rm = TRUE), Production))

# Example t-test to compare means
t.test(Organic_livestock$Production, imputed_data$production)

# Group by the "Country" column and then summarize the numerical columns by summing them

Organic_livestock <- Organic_livestock %>%
  group_by(Country, Year, AnimalType) %>%
  summarize_all(.funs = sum, na.rm = TRUE)

Total_Organic_livestock <- Organic_livestock[c(1, 2, 4)]
# Group by the "Country" column and then summarize the numerical columns by summing them
Total_Organic_livestock$Production <- as.numeric(Total_Organic_livestock$Production)
Total_Organic_livestock <- Total_Organic_livestock %>%
  group_by(Country, Year) %>%
  summarize_all(.funs = sum, na.rm = TRUE)


#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________

#Organic Area - % of total utilised agricultural area (UAA)
Area_Organic <- read_tsv("Area under organic farming.tsv")

#Total_Crop_Production
class(Area_Organic)
#Separate the first column into Crop, Structure of production, Country
# Split the InfoColumn into three columns
Area_Organic <- separate(data = Area_Organic, col = `unit,crops,agprdmet,geo\\time`, into = c("Unit", "Crops", "Agricultural Production Methods", "Country"), sep = ",")

# Specify the columns you want to keep (change these to match your column names)
names(Area_Organic)
columns_to_keep <- c("Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Area_Organic <- Area_Organic %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
Area_Organic <- Area_Organic %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Area_Organic <- Area_Organic %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Area_Organic <- Area_Organic %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Area.Org.Share")


#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________



#GDP_capita
GDP_capita <- read_tsv("GDP.tsv")

#Total_Crop_Production
#Separate the first column into Crop, Structure of production, Country
# Split the InfoColumn into three columns
names(GDP_capita)
GDP_capita <- separate(data = GDP_capita, col = `na_item,unit,geo\\time`, into = c("National Account", "Unit", "Country"), sep = ",")

# Specify the columns you want to keep (change these to match your column names)
names(GDP_capita)
columns_to_keep <- c("Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
GDP_capita <- GDP_capita %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
GDP_capita <- GDP_capita %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
GDP_capita <- GDP_capita %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

GDP_capita <- GDP_capita %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP_capita")

GDP_capita <- GDP_capita %>% slice(1:279)


#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________


#Population
Population <- read_tsv("Population.tsv")

#Total_Crop_Production
#Separate the first column into Crop, Structure of production, Country
# Split the InfoColumn into three columns
names(Population)
Population <- separate(data = Population, col = `indic_de,geo\\time`, into = c("Unit", "Country"), sep = ",")

# Specify the columns you want to keep (change these to match your column names)
columns_to_keep <- c("Country", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")

# Use select and select to keep the specified columns
Population <- Population %>%
  select(all_of(columns_to_keep))

#Use filter to keep only Sweden, Poland and Spain
Population <- Population %>%  filter(Country %in% c("AL","AT","BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR", "HU" ,"IE" ,"IS", "IT", "LT", "LU", "LV", "ME", "MK", "MT", "NO", "PL","PT", "RO", "SE","SI","UK"))

# Rename "ES" to "Spain," "SE" to "Sweden," and "PL" to "Poland" in the "Country" column
Population <- Population %>%
  mutate(Country = case_when(
    Country == "AL" ~ "Albania",
    Country == "AT" ~ "Austria",
    Country == "BE" ~ "Belgium",
    Country == "BG" ~ "Bulgaria",
    Country == "CH" ~ "Switzerland",
    Country == "CY" ~ "Cyprus",
    Country == "CZ" ~ "Czech Republic",
    Country == "DE" ~ "Germany",
    Country == "DK" ~ "Denmark",
    Country == "EE" ~ "Estonia",
    Country == "ES" ~ "Spain",
    Country == "FI" ~ "Finland",
    Country == "FR" ~ "France",
    Country == "HR" ~ "Croatia",
    Country == "HU" ~ "Hungary",
    Country == "IE" ~ "Ireland",
    Country == "IS" ~ "Iceland",
    Country == "IT" ~ "Italy",
    Country == "LT" ~ "Lithuania",
    Country == "LU" ~ "Luxemburg",
    Country == "LV" ~ "Livonia",
    Country == "ME" ~ "Montenegro",
    Country == "MK" ~ "North Macedonia",
    Country == "MT" ~ "Malta",
    Country == "NO" ~ "Norway",
    Country == "PL" ~ "Poland",
    Country == "PT" ~ "Portugal",
    Country == "RO" ~ "Romania",
    Country == "SE" ~ "Sweden",
    Country == "SI" ~ "Slovenia",
    Country == "UK" ~ "United Kingdom",
    TRUE ~ Country
  ))

Population <- Population %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Population")

#________________________________________________________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________


#Merge All datasets together into one common dataset

#Ensure all of the dataframes have the same type
Final_dataset_list <- list(Total_Emissions, Total_Crop_production, Organic_Crop_production_by_crops, Total_livestock, Total_Organic_livestock, Energy_productivity, Area_Organic)
Final_dataset_list <- lapply(Final_dataset_list, function(x){
  x %>% mutate(Year = as.double(Year))
})

Total_Emissions <- Total_Emissions %>% mutate(Year = as.double(Year))
Total_Crop_production <- Total_Crop_production %>% mutate(Year = as.double(Year))
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>% mutate(Year = as.double(Year))
Total_livestock <- Total_livestock %>% mutate(Year = as.double(Year))
Total_Organic_livestock <- Total_Organic_livestock %>% mutate(Year = as.double(Year))
Energy_productivity <- Energy_productivity %>% mutate(Year = as.double(Year))
Area_Organic <- Area_Organic %>% mutate(Year = as.double(Year))
GDP_capita <- GDP_capita %>% mutate(Year = as.double(Year))
Population <- Population %>% mutate(Year = as.double(Year))

#Rename Production in production datasets
Total_Crop_production <- Total_Crop_production %>% rename(Crop.Prod.Total = Production)
Organic_Crop_production_by_crops <- Organic_Crop_production_by_crops %>% rename(Crop.Prod.Org = Production)
Total_livestock <- Total_livestock %>% rename(Liv.Prod.Total = Production)
Total_Organic_livestock <- Total_Organic_livestock %>% rename(Liv.Prod.Org = Production)

#Account for units
Total_Crop_production$Crop.Prod.Total = (Total_Crop_production$Crop.Prod.Total * 1000)
Total_livestock$Liv.Prod.Total = (Total_livestock$Liv.Prod.Total * 1000)
class(GDP_capita$GDP)
GDP_capita <- GDP_capita %>% mutate(GDP_capita = as.numeric(GDP_capita))
class(Population$Population)
Population <- Population %>% mutate(Population = as.numeric(Population))
GDP_capita<- GDP_capita %>% mutate(GDP = GDP_capita * Population$Population)

#Join Dataset
Final_dataset_Full <- Total_Emissions %>%
  full_join(Total_Crop_production, by = c("Year", "Country")) %>%
  full_join(Organic_Crop_production_by_crops, by = c("Year", "Country")) %>%
  full_join(Total_livestock, by = c("Year", "Country")) %>%
  full_join(Total_Organic_livestock, by = c("Year", "Country")) %>%
  full_join(Energy_productivity, by = c("Year", "Country")) %>%
  full_join(Area_Organic, by = c("Year", "Country")) %>% 
  full_join(GDP_capita, by = c("Year", "Country")) %>% 
  full_join(Population, by = c("Year", "Country"))
View(Final_dataset_Full)

