#Hypothesis testing and regression

#Descriptive statistics
library(tidyverse)
library(remotes)
library(ggplot2)
library(GGally)
library(psych)
library(pastecs)
library(stargazer)
library(MASS)
library(lmtest)

#Create the variables

#GHG growth
# Calculate the growth for each year from 2014 to 2020 and store it in a vector
Final_dataset_Full <- Final_dataset_Full %>%
  group_by(Country) %>%
  arrange(Year) %>%
  # mutate(GHG_Growth = (((Emissions - lag(Emissions)) / lag(Emissions)) * 100))
  mutate(
    GHG_Growth = (Emissions - lag(Emissions, order_by = Year)) / lag(Emissions, order_by = Year) * 100
  )

#Ditch countries that have no values in production
Final_dataset_Filtered <- Final_dataset_Full
Final_dataset_Filtered <- Final_dataset_Full %>%
  filter(!is.na(Crop.Prod.Total)) %>%
  filter(!is.na(Crop.Prod.Org)) %>%
  filter(!is.na(Liv.Prod.Total)) %>%
  filter(!is.na(Liv.Prod.Org))

# View(Final_dataset_Filtered)

#Filter emissions
Final_dataset_Filtered <- Final_dataset_Filtered %>%
  filter(!is.na(Emissions))

which(any(is.na(Final_dataset_Filtered)))
rows_with_missing_values <- which(is.na(Final_dataset_Filtered))
print(rows_with_missing_values) #No missing values

#Share of Organic Crop
Org.Crop.Share <- Final_dataset_Filtered$Crop.Prod.Org/Final_dataset_Filtered$Crop.Prod.Total

#Share of Organic Livestock
Org.Liv.Share <- Final_dataset_Filtered$Liv.Prod.Org/Final_dataset_Filtered$Liv.Prod.Total


#Describe Data
summary(Final_dataset_Filtered)
describe(Final_dataset_Filtered)

#Modify Dataset to take into account correaltion between variables
#Frst, make sure everything is in numeric
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(Crop.Prod.Total = as.numeric(Crop.Prod.Total))
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(Crop.Prod.Org = as.numeric(Crop.Prod.Org))
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(Area.Org.Share = as.numeric(Area.Org.Share))
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(Emissions = as.numeric(Emissions))
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(Liv.Prod.Total = as.numeric(Liv.Prod.Total))
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(GHG_Growth = as.numeric(GHG_Growth))
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(GDP_capita = as.numeric(GDP_capita))

#Then proceed
Final_dataset_Filtered <- Final_dataset_Filtered %>%
  mutate(Org.Crop.Share = (Crop.Prod.Org/Crop.Prod.Total)*100)
Final_dataset_Filtered <- Final_dataset_Filtered %>%
  mutate(Org.Liv.Share = (Liv.Prod.Org/Liv.Prod.Total)*100)

Final_dataset_Filtered <- Final_dataset_Filtered %>%
  mutate(Org.Prod = (Liv.Prod.Org/Liv.Prod.Total)*100)


Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(Org.Liv.Share = as.numeric(Org.Liv.Share))
Final_dataset_Filtered <- Final_dataset_Filtered %>% mutate(Org.Crop.Share = as.numeric(Org.Crop.Share))

#Test for Endogeneity

#Test for Heteroscedasticity
library(olsrr)
library(car)


# NA check
Final_dataset_FilteredL1 = Final_dataset_Filtered %>%
  na.omit()

#Check for Colinearity
ggcorr(Final_dataset_FilteredL1, method = c("pairwise", "pearson"),
       nbreaks = NULL, digits = 3,
       low = "blue", mid = "white", high = "red",
       geom = "tile",
       label = TRUE,
       label_alpha = FALSE)

# 표준화
Final_dataset_FilteredL2 = Final_dataset_FilteredL1 %>%
  mutate(across(where(is.numeric), scale)) %>%
  magrittr::set_colnames(c(colnames(Final_dataset_FilteredL1))) %>%
  na.omit()

allVar = names(Final_dataset_FilteredL2)
xVar <- setdiff(allVar, c("Emissions", "Country"))
formula <- paste("Emissions ~", paste(xVar, collapse = " + ")) %>% as.formula()

reg1 <- lm(formula, data = Final_dataset_FilteredL2)
summary(reg1)
ols_test_breusch_pagan(reg1) #There is no heteroscedasticity
ols_plot_resid_fit(reg1) # There seems to be no heteroscedasticity

lmFitStep = MASS::stepAIC(reg1, direction = "both")
summary(lmFitStep)
ols_test_breusch_pagan(lmFitStep) #There is heteroscedasticity
ols_plot_resid_fit(lmFitStep)

# #Do the regression accounting for heteroscedasticity
# robust_reg1 <- rlm(GHG_Growth ~ Org.Crop.Share + Org.Liv.Share + Productivity + GDP_capita + Area.Org.Share, data = Final_dataset_Filtered)
# robust_reg2 <- rlm(GHG_Growth ~ Liv.Prod.Org + Crop.Prod.Org + Productivity + GDP_capita + Area.Org.Share, data = Final_dataset_Filtered)
# #Do a series of test
# plot(robust_reg1$fitted.values, residuals(robust_reg1),
#      xlab = "Fitted Value",
#      ylab = "Residuals")
# bptest(robust_reg1, studentize = FALSE)
# plot(robust_reg1)
# summary(robust_reg1)
#
# #Other regressions
# summary(reg2)
#
# #Plot Data
# Final_dataset_Filtered %>% ggplot(aes(x = Liv.Prod.Org + Crop.Prod.Org + Productivity + GDP_capita + Area.Org.Share,
#                                       y = GHG_Growth))+
#   geom_point(aes(col = Country,
#                  size = Emissions))+
#   geom_smooth(method = lm)+
#   labs(x = "Share of organic production",
#        y = "Growth of GHG - Farm-Gate")
#
# names(Final_dataset_Filtered)
#
# #Check for Colinearity
# ggcorr(Regression_dataset, method = c("pairwise", "pearson"),
#        nbreaks = NULL, digits = 3,
#        low = "blue", mid = "white", high = "red",
#        geom = "tile",
#        label = TRUE,
#        label_alpha = FALSE)
#
# #Plot Data
# Final_dataset_Filtered %>% ggplot(aes(x = Crop.Prod.Total + Liv.Prod.Total + Liv.Prod.Org + Crop.Prod.Org + Productivity + GDP_capita + Area.Org.Share,
#                                       y = GHG_Growth))+
#   geom_point(aes(col = Country,
#                  size = Emissions))+
#   geom_smooth(method = lm)+
#   labs(x = "Growth of GHG - Farm-Gate",
#        y = "Share of organic production")
#
#
# write.xlsx(Final_dataset_Filtered, file = "Data.xlsx")
#
# head(Final_dataset)
# Final_dataset