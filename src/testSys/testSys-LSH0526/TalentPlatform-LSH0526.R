#================================================
# 요구사항
#================================================
# R을 이용한 매력도 및 직장 만족도의 시각화 및 회귀분석

# ================================================
# 비즈니스 로직 수행
# ================================================
# 라이브러리 읽기
library(tidyverse)
library(modelr)

# 데이터 읽기
attData = read.csv('attract.dat', sep = "\t") %>% 
  as.tibble()

colnames(attData)

satData = read.csv('satis.dat', sep = "\t") %>% 
  as.tibble()

colnames(satData)

# ==============================================================================
# 11. Attract.dat 데이터 파일에 관한 문제 
# ==============================================================================
# 11.1
ggplot(attData, aes(x=face, y=attract)) +
  geom_point() +
  geom_smooth(method='lm', level=0.99, color='blue', size=2, fill='#FF69B4', alpha=0.1) +
  ggtitle('외모평가에 따른 매력도의 산포도 및 추세선') +
  xlab('외모평가') +
  ylab('매력도')

ggplot(attData, aes(x=humor, y=attract)) +
  geom_point() +
  geom_smooth(method='lm', level=0.99, color='blue', size=2, fill='#FF69B4', alpha=0.1) +
  ggtitle('유머능력에 따른 매력도의 산포도 및 추세선') +
  xlab('유머능력') +
  ylab('매력도')


ggplot(attData, aes(x=charact, y=attract)) +
  geom_point() +
  geom_smooth(method='lm', level=0.99, color='blue', size=2, fill='#FF69B4', alpha=0.1) +
  ggtitle('성격에 따른 매력도의 산포도 및 추세선') +
  xlab('성격') +
  ylab('매력도')


ggplot(attData, aes(x=iqscore, y=attract)) +
  geom_point() +
  geom_smooth(method='lm', level=0.99, color='blue', size=2, fill='#FF69B4', alpha=0.1) +
  ggtitle('지능에 따른 매력도의 산포도 및 추세선') +
  xlab('지능') +
  ylab('매력도')

# 11.2
ggplot(attData, aes(x=humor, y=attract, color=sex)) +
  geom_point() +
  geom_smooth(aes(color=sex), method='lm', level=0.99, size=2, se=TRUE, fill='#FF69B4', alpha=0.1) +
  scale_color_manual(values=c("blue", "red")) +
  ggtitle('유머능력에 따른 매력도의 산포도 및 추세선') +
  xlab('유머능력') +
  ylab('매력도')

# 11.3
ggplot(attData, aes(x = sex, y=iqscore, color = sex)) +
  geom_boxplot() +
  ggtitle('성별에 따른 지능의 상자그림') +
  xlab('성별') +
  ylab('지능')

# ==============================================================================
# 12. Satis.dat 데이터 파일에 관한 문제 
# ==============================================================================
# "id"     "area"   "satis"  "motiv"  "envir"  "salary" "stress"

# 12.1
satData$area = factor(satData$area, levels = c(1, 2, 3), labels = c('서울', '지방', '해외'))
satData$area

# 12.2 
# P값은 0.2318로서 유의수준 0.05보다 크기 떄문에 귀무가설 기각하여 정규분포를 따름
shaTest = shapiro.test(satData$salary)
shaTest


ggplot(satData, aes(x=salary)) + 
  geom_histogram(aes(y=..density..), fill="blue", color="black", alpha = 0.1) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("임금의 히스토그램") +
  xlab('임금') +
  ylab('밀도 함수')


# 12.3 
satData$salaryLog = log(satData$salary)

ggplot(satData, aes(x=satData$salaryLog)) +
  geom_histogram(bins=30, fill='blue', color='black', alpha=0.7) +
  ggtitle("log 임금의 히스토그램") +
  xlab("log 임금") +
  ylab("빈도 분포")


# 12.4
ggplot(satData, aes(x=salary, y=satis)) +
  geom_point() +
  geom_smooth(method='lm', level=0.99, color='blue', size=2, fill='#FF69B4', alpha=0.1) +
  ggtitle('임금에 따른 직장 만족도의 산포도 및 추세선') +
  xlab('임금') +
  ylab('직장 만족도')

ggplot(satData, aes(x=salaryLog, y=satis)) +
  geom_point() +
  geom_smooth(method='lm', level=0.99, color='blue', size=2, fill='#FF69B4', alpha=0.1) +
  ggtitle('log 임금에 따른 직장 만족도의 산포도 및 추세선') +
  xlab('log 임금') +
  ylab('직장 만족도')

# ==============================================================================
# 13. Satis.dat 데이터 파일에 관한 문제 
# ==============================================================================
satDataL1 = satData
satDataL1$area = as.numeric(satDataL1$area)

# 13.1
satDataL1 %>% 
  dplyr::select(satis, motiv, envir, salary, stress, area) %>% 
  cor()

# 13.2
satDataL2 = satData %>% 
  dplyr::select(satis, motiv, envir, salary, stress, area) 

lmFit = lm(satis ~ ., data = satDataL2)

prdData = satDataL2 %>% 
  add_predictions(lmFit, var = "pred") %>%
  add_residuals(lmFit, var = "resid")
head(prdData)

# 선형성 검정
# 앞서 상관계수에서도와 같이 선형성 만족
prdData %>%
  ggplot(aes(x = pred, y = satis)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Observed vs Predicted Scatterplot",
    x = "Predicted Values",
    y = "Observed Values"
  )

# 등분산성 검정
# 잔차를 기준으로 불규칙하게 분포되어 등분산 X
prdData %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Residual vs. Predicted Values Scatterplot",
    x = "Predicted Values",
    y = "Residuals"
  )

# 정규성 검정
# QQ plot에서 고르게 만족하고 있어 정규성 만족함
prdData %>%
  ggplot(aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of resid",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# 13.3
# 종속 변수 (직장 만족도)를 예측하기 위해서 독립변수 5종을 통해 선형회귀 모형을 수행함
# 모형 결과 수정된 결정계수는 0.3373로서 유의수준 0.05 이하에서 통계적으로 유의미함 (P값 참조)
# 회귀계수에서는 임금, 스트레스, 직무동기, 지역(해외)인 경우를 제외하고 대부분 통계적으로 유의미하지 못함
summary(lmFit)
