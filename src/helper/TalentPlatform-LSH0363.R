
#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#================================================
# 요구사항
#================================================
# R을 이용한 판매량 회귀분석 및 시계열 예측

# 1. Ad1, Ad2와 Prom에 의해 판매한 sales 얼마나 되는가? 각 마케팅 활동의 종류별로 sale에 얼마나 영향을 미치는가?
# 2. TV 광고는 200만 파운드, Prom는 매년 50만 파운드의 비용이 든다. 어느 것이 sales에 더 비용 효율적인가?
# 3. 마케팅 활동(ad1, ad2, prom) 외의 sales에 영향을 주는 요소를 제시하고 설명하시오.
# 4. 다음 달 매출에 대한 예측을 제공해주실 수 있나요? (만약 그렇다면, 모든 지역과 전체에 대해 설명하시오)
#
# On the dataset provided for your analysis: The columns in the file correspond (in order) to:
#
# • Row number -empty column header in the data file-
# • Sales (Number of units/packs of the product sold) : 판매한 유닛
# • Price (price in GBP per unit of product) : 유닛별 가격
# • Ad1 (TV advert in GRP -Gross rating points-) : TV 광고
# • Ad2 (Banner advert pressure, in views, transformed to GRP for comparability with the TV ad): 배너광고
# • Prom (percentage of stores with promotion on) : 가게 할인 프로모션
# • Time (monthly time index) : 시간
# • Product (type of unit) : 상품
# • Region (geographical region) : 지역
# • Month (month) : 월

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0363"

if (Sys.info()["sysname"] == "Windows") {
  contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")
} else {
  contextPath = ifelse(env == "local", ".", "/SYSTEMS/PROG/R/PyCharm")
}

if (env == "local") {
  globalVar = list(
    "inpPath" = contextPath
    , "figPath" = contextPath
    , "outPath" = contextPath
    , "tmpPath" = contextPath
    , "logPath" = contextPath
  )
} else {
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
}

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 설정
library(tidyverse)
library(lubridate)
library(openxlsx)
library(lm.beta)
library(forecast)

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Data_Icecream_3.csv"))
data = readr::read_csv(fileInfo, locale=locale(encoding="UTF-8")) %>%
  readr::type_convert() %>%
  na.omit()

# 선형회귀
lmFit = lm(sales ~ ad1 + ad2 + prom, data = data)

# 선형회귀 요약
summary(lmFit)
# Call:
# lm(formula = sales ~ ad1 + ad2 + prom, data = data)
#
# Residuals:
#       Min        1Q    Median        3Q       Max
# -91913.32 -30247.46 -14836.13  -2066.81 738577.68
#
# Coefficients:
#               Estimate Std. Error  t value  Pr(>|t|)
# (Intercept)  7101.7107 10197.7191  0.69640 0.4870959
# ad1          -344.3752  2029.7671 -0.16966 0.8654705
# ad2         11942.3819  6598.4034  1.80989 0.0720189 .
# prom         6943.7047  2212.2812  3.13871 0.0019902 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 83549.52 on 176 degrees of freedom
# Multiple R-squared:  0.07498984,	Adjusted R-squared:  0.05922262
# F-statistic:  4.75606 on 3 and 176 DF,  p-value: 0.003253876

# 베타회귀
lmBetaFit = lm.beta::lm.beta(lmFit)

# 베타회귀 요약
summary(lmBetaFit)
# Call:
# lm(formula = sales ~ ad1 + ad2 + prom, data = data)
#
# Residuals:
#       Min        1Q    Median        3Q       Max
# -91913.32 -30247.46 -14836.13  -2066.81 738577.68
#
# Coefficients:
#                  Estimate  Standardized    Std. Error  t value  Pr(>|t|)
# (Intercept)  7.101711e+03            NA  1.019772e+04  0.69640 0.4870959
# ad1         -3.443752e+02 -1.293483e-02  2.029767e+03 -0.16966 0.8654705
# ad2          1.194238e+04  1.383283e-01  6.598403e+03  1.80989 0.0720189 .
# prom         6.943705e+03  2.282392e-01  2.212281e+03  3.13871 0.0019902 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 83549.52 on 176 degrees of freedom
# Multiple R-squared:  0.07498984,	Adjusted R-squared:  0.05922262
# F-statistic:  4.75606 on 3 and 176 DF,  p-value: 0.003253876

# 1. Ad1, Ad2와 Prom에 의해 판매한 sales 얼마나 되는가? 각 마케팅 활동의 종류별로 sale에 얼마나 영향을 미치는가?
# 예측 판매량
prdVal = predict(lmFit, newdata = data)

# 예측 판매량 총계 : 5,277,691
sum(prdVal, na.rm = TRUE)

# • Ad1 (TV advert in GRP -Gross rating points-) : TV 광고
# • Ad2 (Banner advert pressure, in views, transformed to GRP for comparability with the TV ad): 배너광고
# • Prom (percentage of stores with promotion on) : 가게 할인 프로모션

# 2. TV 광고는 200만 파운드, Prom는 매년 50만 파운드의 비용이 든다. 어느 것이 sales에 더 비용 효율적인가?

# 베타회귀에서 표준화 계수를 통해 ad2보다 Prom 효율이 가장 좋음
lmBetaFit$standardized.coefficients %>% round(2)
# (Intercept)         ad1         ad2        prom
#          NA       -0.01        0.14        0.23



# 3. 마케팅 활동(ad1, ad2, prom) 외의 sales에 영향을 주는 요소를 제시하고 설명하시오.

dataL1 = data
# 카테고리형을 숫자형으로 변환
monthData = 1:12
names(monthData) = stringr::str_sub(month.name, 1, 3)
dataL1$month = monthData[dataL1$month]

dataL1$region = factor(dataL1$region)

# 속성 1개 제외
# data$product = factor(data$product)

# 선형회귀
# lmFit = lm(sales ~ price + ad1 + ad2 + prom + time + region + month + revenue + year, data = data)
lmFit = lm(sales ~ price + ad1 + ad2 + prom + time + region + month, data = dataL1)

# 베타회귀
lmBetaFit = lm.beta::lm.beta(lmFit)

# 베타회귀 요약
summary(lmBetaFit)
# Call:
# lm(formula = sales ~ price + ad1 + ad2 + prom + time + region +
#     month, data = dataL1)
#
# Residuals:
#        Min         1Q     Median         3Q        Max
# -119083.58  -27862.16     490.32   17372.32  602994.35
#
# Coefficients:
#                     Estimate  Standardized    Std. Error  t value   Pr(>|t|)
# (Intercept)     1.748463e+05            NA  2.779664e+04  6.29020 2.6201e-09
# price          -4.757116e+02 -2.338215e-01  1.178425e+02 -4.03684 8.1987e-05
# ad1            -6.718143e+02 -2.523353e-02  1.690302e+03 -0.39745   0.691536
# ad2             1.338920e+04  1.550867e-01  5.288032e+03  2.53198   0.012254
# prom            7.326513e+03  2.408220e-01  1.752935e+03  4.17957 4.6763e-05
# time            3.278986e+02  3.965455e-02  5.074856e+02  0.64612   0.519075
# regionEast     -1.169908e+05 -5.447795e-01  1.565579e+04 -7.47268 4.0307e-12
# regionTheNorth -1.280484e+05 -5.962704e-01  1.554017e+04 -8.23983 4.5613e-14
# regionTheSouth -1.240625e+05 -5.777099e-01  1.555271e+04 -7.97691 2.1645e-13
# regionWest     -1.285841e+05 -5.987648e-01  1.554092e+04 -8.27390 3.7223e-14
# month           2.012135e+03  8.086186e-02  1.575802e+03  1.27690   0.203391
#
# (Intercept)    ***
# price          ***
# ad1
# ad2            *
# prom           ***
# time
# regionEast     ***
# regionTheNorth ***
# regionTheSouth ***
# regionWest     ***
# month
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 65793.22 on 169 degrees of freedom
# Multiple R-squared:  0.4491988,	Adjusted R-squared:  0.416607
# F-statistic: 13.78258 on 10 and 169 DF,  p-value: < 2.2204e-16

# 베타회귀에서 표준화 계수를 통해 price, region에서 -0.23, -0.54~-0.60 순으로 판매량과 반비례 경향을 보임
lmBetaFit$standardized.coefficients %>% round(2)
 # (Intercept)          price            ad1            ad2           prom
 #          NA          -0.23          -0.03           0.16           0.24
 #        time     regionEast regionTheNorth regionTheSouth     regionWest
 #        0.04          -0.54          -0.60          -0.58          -0.60
 #       month
 #        0.08


# 4. 다음 달 매출에 대한 예측을 제공해주실 수 있나요? (만약 그렇다면, 모든 지역과 전체에 대해 설명하시오)
dataL2 = dataL1 %>%
  dplyr::select(region, month, year, sales)

# lmFit = lm(sales ~ region + month + year, data = dataL2)
# summary(lmFit)

regionList = dataL2$region %>% unique() %>% sort()
yearList = c(2010, 2011, 2012)
monthList = dataL2$month %>% unique() %>% sort()

# 학습 데이터 : 2010.01 ~ 2012.12 (총 36개)
# 검증 데이터 : 2013.01
# 학습 데이터에 대한 보간 (없을 시 0)
for (regionInfo in regionList) {
  for (yearInfo in yearList) {
    for (monthInfo in monthList) {

      selData = dataL2 %>%
        dplyr::filter(
          region == regionInfo
          , year == yearInfo
          , month == monthInfo
        )

      # 데이터 보간
      if (nrow(selData) < 1) {
        tmpData = data.frame(year = yearInfo, month = monthInfo, region = regionInfo)
        # tmpData$sales = predict(lmFit, newdata = tmpData)
        tmpData$sales = 0
        dataL2 = dplyr::bind_rows(dataL2, tmpData)

        next
      }
    }
  }
}


# region에 따른 시계열 모형 학습/시계열
akimaDataL1 = tibble()
for (regionInfo in regionList) {

  dataL3 = dataL2 %>%
    dplyr::filter(
      region == regionInfo
      , year %in% yearList
    ) %>%
    dplyr::arrange(region, year, month)

  tsData = ts(dataL3$sales, start=c(2010,1), end=c(2012,12), frequency = 12)

  # 시계열로부터 최종선택한 모형
  bestModel = forecast::auto.arima(tsData, trace = TRUE)
  # ARIMA(0,1,4)(1,1,2)[12]
  # 비계절 : AR(0), 1차 차분, MA(4)
  # 계절 : AR(1), 2차 차분 (24개월), MA(2)

  # 모형의 통계적 유의성 검정
  # X-squared = 0.071471882, df = 1, p-value = 0.7892057
  # Box.test(bestModel$residuals, lag = 1, type = "Ljung")

  # 최종선택한 모형을 사용하여 구한 예측값(월별자료는 향후 1개월)을 나타내는 그래프
  akimaData = forecast::forecast(bestModel, h = 1)

  tmpData = data.frame(akimaData)
  tmpDataL1 = tibble(
    dtDate =  row.names(tmpData)
    , region =regionInfo
    , tmpData
  )

  akimaDataL1 = dplyr::bind_rows(akimaDataL1, tmpDataL1)

  # 성능 비교
  # accuracy(akimaData)

  mainTitle = sprintf("2010-2012년 월별 %s 판매량 시계열 + 향후 ARIMA 1개월 예측", regionInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

  makePlot = autoplot(akimaData) +
    geom_smooth(method = 'lm', se = TRUE) +
    ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
    ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 5) +
    labs(
      x = "연도"
      , y = "판매량"
      , color = NULL
      , fill = NULL
      , subtitle = mainTitle
    ) +
    theme(
      text = element_text(size = 16)
      , legend.position = "bottom"
    )
    ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
}

print(akimaDataL1)
# # A tibble: 5 × 7
#   dtDate   region   Point.Forecast    Lo.80   Hi.80     Lo.95   Hi.95
#   <chr>    <chr>             <dbl>    <dbl>   <dbl>     <dbl>   <dbl>
# 1 Jan 2013 Capital        123612.  -82306.  329531. -191312.  438537.
# 2 Jan 2013 East            62459.   36826.   88093.   23256.  101663.
# 3 Jan 2013 TheNorth          471.     -81.8   1024.    -375.    1317.
# 4 Jan 2013 TheSouth         4955.   -4310.   14219.   -9214.   19123.
# 5 Jan 2013 West               54.3    -32.6    141.     -78.5    187.