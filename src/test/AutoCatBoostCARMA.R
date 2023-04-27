
# install.packages("remotes")
# remotes::install_github("AdrianAntico/RemixAutoML")

library(RemixAutoML)
library(data.table)

########################################
# Prepare data for AutoTS()----
########################################
# Load Walmart Data from Remix Institute's Box Account----
data <- data.table::fread("https://remixinstitute.box.com/shared/static/9kzyttje3kd7l41y1e14to0akwl9vuje.csv")

# Subset for Stores / Departments with Full Series Available: (143 time points each)----
data <- data[, Counts := .N, by = c("Store","Dept")][
  Counts == 143][, Counts := NULL]
# Subset Columns (remove IsHoliday column)----
keep <- c("Store","Dept","Date","Weekly_Sales")
data <- data[, ..keep]
# Group Concatenation----
data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c("Store","Dept")]
data[, c("Store","Dept") := NULL]
# Grab Unique List of GroupVar----
StoreDept <- unique(data[["GroupVar"]])
# AutoTS() Builds: Keep Run Times and AutoTS() Results----
# NOTES:
#   1. SkipModels: run everything
#   2. StepWise: runs way faster this way (cartesian check otherwise, but parallelized)
#   3. TSClean: smooth outliers and do time series imputation
#        over the data first
#   4. ModelFreq: algorithmically identify a series frequency to build
#        your ts data object
TimerList <- list()
OutputList <- list()
l <- 0
for(i in StoreDept) {
  l <- l + 1
  temp <- data[GroupVar == eval(i)]
  temp[, GroupVar := NULL]
  TimerList[[i]] <- system.time(
    OutputList[[i]] <- tryCatch({
      RemixAutoML::AutoTS(
        temp,
        TargetName       = "Weekly_Sales",
        DateName         = "Date",
        FCPeriods        = 52,
        HoldOutPeriods   = 30,
        EvaluationMetric = "MAPE",
        TimeUnit         = "week",
        Lags             = 25,
        SLags            = 1,
        NumCores         = 4,
        SkipModels       = NULL,
        StepWise         = TRUE,
        TSClean          = TRUE,
        ModelFreq        = TRUE,
        PrintUpdates     = FALSE)},
      error = function(x) "Error in AutoTS run"))
  print(l)
}
# Save Results When Done and Pull Them in After AutoCatBoostCARMA() Run----
save(TimerList, file = paste0(getwd(),"/TimerList.R"))
save(OutputList, file = paste0(getwd(),"/OutputList.R"))
########################################
# Prepare data for AutoCatBoostCARMA()----
########################################
# Load Walmart Data from Remix Institute's Box Account----
data <- data.table::fread("https://remixinstitute.box.com/shared/static/9kzyttje3kd7l41y1e14to0akwl9vuje.csv")
# Subset for Stores / Departments With Full Series (143 time points each)----
data <- data[, Counts := .N, by = c("Store","Dept")][
  Counts == 143][, Counts := NULL]
# Subset Columns (remove IsHoliday column)----
keep <- c("Store","Dept","Date","Weekly_Sales")
data <- data[, ..keep]
# Run AutoCatBoostCARMA()----
# NOTES:
#    1. GroupVariables get concatenated into a single column but returned back to normal
#    2. Lags and MA_Periods cover both regular and seasonal so mix it up!
#    3. CalendarVariables:
#           seconds, hour, wday, mday, yday, week, isoweek, month, quarter, year
#    4. TimeTrendVariable: 1:nrow(x) by group with 1 being the furthest back in time
#           no need for quadratic or beyond since catboost will fit nonlinear relationships
#    5. DataTruncate: TRUE to remove records with imputed values for NA's created by the
#           DT_GDL_Feature_Engineering
#    6. SplitRatios - written the way it is to ensure same ratio split as AutoTS()
#    7. TaskType - I use GPU but if you don't have one, set to CPU
#    8. I did not set GridTune to TRUE because I didn't want to wait
#    9. GridEvalMetric and ModelCount only matter if GridTune is TRUE
#   10. NTrees - Yes, I used 15k trees and I could have used more since the best model
#           performance utilized all trees (hit upper boundary)
#   11. PartitionType - "timeseries" allows time-based splits by groups IF you have equal sized
#           groups within each series ("random" is well, random; "time" is for transactional data)
#   12. Timer - Set to TRUE to get a print out of which forecasting step you are on when the
#           function hits that stage
#  *13. TargetTransformation is a new feature. Automatically choose the best transformation for
#           the target variable. Tries YeoJohnson, BoxCox, arcsinh, along with 
#           asin(sqrt(x)) and logit for proportion data
Results <- RemixAutoML::AutoCatBoostCARMA(
  data,
  TargetColumnName = "Weekly_Sales",
  DateColumnName = "Date",
  GroupVariables = c("Store","Dept"),
  FC_Periods = 52,
  TimeUnit = "week",
  TargetTransformation = TRUE,
  Lags = c(1:25, 51, 52, 53),
  MA_Periods = c(1:25, 51, 52, 53),
  CalendarVariables = TRUE,
  TimeTrendVariable = TRUE,
  DataTruncate = FALSE,
  SplitRatios = c(1 - 2*30/143, 30/143, 30/143),
  TaskType = "GPU",
  EvalMetric = "MAE",
  GridTune = FALSE,
  GridEvalMetric = "mae",
  ModelCount = 1,
  NTrees = 20000,
  PartitionType = "timeseries",
  Timer = TRUE)
# Plot aggregate sales forecast (Stores and Departments rolled up into Total)----
Results$TimeSeriesPlot
# Metrics for every store / dept. combo----
# NOTES:
#    1. Can also pull back other AutoCatBoostRegression() info such as
#         Variable Importance, Evaluation Plots / BoxPlots, Partial
#         Dependence Plots / BoxPlots, etc.
ML_Results <- Results$ModelInformation$EvaluationMetricsByGroup
# Transformation info:
# ColumnName = Variable Modified
# NethodName = Transformation Method
# Lambda = lambda value for YeoJohnson or BoxCox; NA otherwise
# NormalizedStatistic = pearson statistic
#   Note: value of 0.0000 is a filler value for prediction values
#         and it's included to show that the correct transformation was done
TransformInfo <- Results$TransformationDetail
#     ColumnName   MethodName  Lambda      NormalizedStatistics
# 1:  Weekly_Sales YeoJohnson  0.6341344   532.3125
# 2:  Predictions  YeoJohnson  0.6341344   0.0000
##################################################
# AutoTS() and AutoCatBoostCARMA() Comparison----
##################################################
# Load AutoTS outputs we saved earlier----
load(paste0(getwd(), "/TimerList.R"))
load(paste0(getwd(), "/OutputList.R"))
# Group Concatenation----
data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c("Store","Dept")]
data[, c("Store","Dept") := NULL]
# Grab unique list of GroupVar
StoreDept <- unique(data[["GroupVar"]])
# AutoTS: format results----
results <- list()
for(i in 1:2660) {
  results[[i]] <- tryCatch({
    OutputList[[i]]$EvaluationMetrics[1,]},
    error = function(x)
      return(data.table::data.table(
        ModelName = "NONE",
        MeanResid = NA,
        MeanPercError = NA,
        MAPE = NA,
        MAE = NA,
        MSE = NA,
        ID = 0)))
}
# AutoTS() Results----
Results <- data.table::rbindlist(results)
# AutoTS() Model Winners by Count----
print(
  data.table::setnames(
    Results[, .N, by = "ModelName"][order(-N)],
    "N",
    "Counts of Winners"))
# ModelName Counts of Winners
# 1:               TBATS               556
# 2:            TSLM_TSC               470
# 3:           TBATS_TSC               469
# 4:               ARIMA               187
# 5:           ARIMA_TSC               123
# 6:     TBATS_ModelFreq               117
# 7:              ARFIMA                86
# 8:                  NN                74
# 9:                 ETS                69
# 10:     ARIMA_ModelFreq                68
# 11:              NN_TSC                66
# 12:     NN_ModelFreqTSC                63
# 13:        NN_ModelFreq                60
# 14:          ARFIMA_TSC                52
# 15:       ETS_ModelFreq                51
# 16:  TBATS_ModelFreqTSC                38
# 17:   TSLM_ModelFreqTSC                29
# 18: ARFIMA_ModelFreqTSC                27
# 19:    ETS_ModelFreqTSC                23
# 20:  ARIMA_ModelFreqTSC                15
# 21:    ARFIMA_ModelFreq                11
# 22:                NONE                 6
# ModelName Counts of Winners
# AutoTS() Run Times----
User <- data.table::data.table(data.table::transpose(TimerList)[[1]])
data.table::setnames(User,"V1","User")
SystemT <- data.table::data.table(data.table::transpose(TimerList)[[2]])
data.table::setnames(SystemT,"V1","System")
Elapsed <- data.table::data.table(data.table::transpose(TimerList)[[3]])
data.table::setnames(Elapsed,"V1","Elapsed")
Times <- cbind(User, SystemT, Elapsed)
# AutoTS Run time Results----
MeanTimes <- Times[, .(User = sum(User),
                       System = sum(System),
                       Elapsed = sum(Elapsed))]
# AutoTS() Run Time In Hours----
print(MeanTimes/60/60)
#        User    System  Elapsed
# 1: 29.43282 0.3135111 33.24209
# AutoTS() Results Preparation----
Results <- cbind(StoreDept, Results)
GroupVariables <- c("Store","Dept")
Results[, eval(GroupVariables) := data.table::tstrsplit(StoreDept, " ")][
  , ':=' (StoreDept = NULL, ID = NULL)]
data.table::setcolorder(Results, c(7,8,1:6))
# Merge in AutoCatBoostCARMA() and AutoTS() Results----
FinalResults <- merge(ML_Results,
                      Results,
                      by = c("Store","Dept"),
                      all = FALSE)
# Add Indicator Column for AutoCatBoostCARMA() Wins----
FinalResults[, AutoCatBoostCARMA := ifelse(MAPE_Metric < MAPE, 1, 0)]
# Percentage of AutoCatBoostCARMA() Wins----
print(paste0("AutoCatBoostCARMA() performed better on MAPE values ",
             round(
               100 * FinalResults[!is.na(MAPE), mean(AutoCatBoostCARMA)],
               1),
             "% of the time vs. AutoTS()"))
# [1] "AutoCatBoostCARMA() performed better on MAPE values 41% of the time vs. AutoTS()"
# AutoCatBoostCARMA() Average MAPE by Store and Dept----
print(paste0("AutoCatBoostCARMA() Average MAPE of ",
             round(
               100 * FinalResults[!is.na(MAPE), mean(MAPE_Metric)],
               1),
             "%"))
# [1] "AutoCatBoostCARMA() Average MAPE of 14.1%"
# AutoTS() Average MAPE by Store and Dept----
print(paste0("AutoTS() Average MAPE of ",
             round(
               100 * FinalResults[!is.na(MAPE), mean(MAPE)],
               1),
             "%"))
# [1] "AutoTS() Average MAPE of 12%"
#################################################
# AutoTS() by top 100 Grossing Departments----
#################################################
temp <- data[, .(Weekly_Sales = sum(Weekly_Sales)), by = "GroupVar"][
  order(-Weekly_Sales)][1:100][, "GroupVar"]
GroupVariables <- c("Store","Dept")
temp[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][
  , ':=' (GroupVar = NULL, ID = NULL)]
temp1 <- merge(FinalResults, temp, by = c("Store","Dept"), all = FALSE)
# Percentage of AutoCatBoostCARMA() Wins----
print(paste0("AutoCatBoostCARMA() performed better on MAPE values ",
             round(
               100 * temp1[!is.na(MAPE), mean(AutoCatBoostCARMA)],
               1),
             "% of the time vs. AutoTS()"))
# [1] "AutoCatBoostCARMA() performed better than AutoTS() on MAPE values 47% of the time"
# AutoCatBoostCARMA() Average MAPE by Store and Dept----
print(paste0("AutoCatBoostCARMA() Average MAPE of ",
             round(
               100 * temp1[!is.na(MAPE), mean(MAPE_Metric)],
               1),
             "%"))
# [1] "AutoCatBoostCARMA() Average MAPE of 5.6%"
# AutoTS() Average MAPE by Store and Dept----
print(paste0("AutoTS() Average MAPE of ",
             round(
               100 * temp1[!is.na(MAPE), mean(MAPE)],
               1),
             "%"))
# [1] "AutoTS() Average MAPE of 5.6%"

















#==========================================================
# TEST
#========================================================== 
library(RemixAutoML) 
library(data.table) 
library(dplyr) 
library(magrittr) 
library(ggplot2) 
library(scales) 
library(magick) 
library(grid) 

# REMIX INSTITUTE BOX 계정에서 데이터 가져오기 -------- -- 

# 파일을 수동으로 다운로드할 수 있는 링크: https://remixinstitute.app.box.com/v/walmart-store-sales-data/ 
walmart_store_sales_data = data.table::fread("https://remixinstitute.box.com /shared/static/9kzyttje3kd7l41y1e14to0akwl9vuje.csv", header = T, stringsAsFactors = FALSE) 


# 최고 매출 매장 찾기 (dplyr 사용) ---------- 

# 그룹 매장별 합계 주간 매출 
top_grossing_store = walmart_store_sales_data %>% dplyr::group_by(., Store) %>%
  dplyr::summarize(., Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE)) 

# 최대 45개 매장의 매출 
max_sales = max(top_grossing_store$Weekly_Sales) 

# 최고 매출 매장 
찾기 top_grossing_store = top_grossing_store %>% dply ., Weekly_Sales == max_sales) 
top_grossing_store = top_grossing_store$Store %>% as.numeric(.) 

# 매출 상위 매장은? 
print(paste("매장 번호: ", top_grossing_store, sep = "")) 


# 최고 매출 매장의 주간 판매 데이터 찾기 (data.table 사용) ---------- 
top_store_weekly_sales <- walmart_store_sales_data[Store = = 평가(상위_grossing_store), 
                                                   .(Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE)),
                                                   by = "Date"] 


# AutoTS를 사용한 월마트 매장의 주간 판매 예측 ------ 

# 다음 16주 동안의 예측 - 기술적으로는 한 줄의 코드이지만 
# 각 인수는 프리젠테이션 목적으로 고유한 줄로 사용되었습니다 
week_forecast = RemixAutoML ::AutoTS( 
  데이터 = top_store_weekly_sales , 
  TargetName = "Weekly_Sales", 
  DateName = "Date", 
  FCPeriods = 16, 
  HoldOutPeriods = 12, 
  TimeUnit = "주"
) 


# VISUALIZE AutoTS FORECASTS ---------------- 

# 16주 예측 
보기 View(weekly_forecast$Forecast) 

# 모델 평가 메트릭 
보기 View(weekly_forecast$EvaluationMetrics) 

# 어떤 모델이 이겼습니까?
print(weekly_forecast$ChampionModel) 

# 예측 
플롯의 ggplot 참조 플롯 = Weekly_forecast$TimeSeriesPlot 
# y축을 통화 
플롯으로 변경 플롯 = 플롯 + ggplot2::scale_y_continuous(labels = scales::dollar) 
#RemixAutoML 브랜딩. 영감: https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html 
logo = magick::image_read("https://www.remixinstitute. com/wp-content/uploads/7b-Cheetah_Charcoal_Inline_No_Sub_No_BG.png") 
plot 
grid::grid.raster(logo, x = .73, y = 0.01, just = c('left', 'bottom'), width = 0.25 )