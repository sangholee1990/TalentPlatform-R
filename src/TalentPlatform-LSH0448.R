# ===============================================================================================
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
# R을 이용한 키와 몸무게 간의 산점도, 학년별 수학점수 막대그래프 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0448"

if (Sys.info()[["sysname"]] == "Windows") {
  contextPath = ifelse(env == "local", ".", "C:/SYSTEMS/PROG/R/TalentPlatform-R")
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
  # source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
  source(file.path(contextPath, "src", "InitConfig.R"))
}

# ================================================
# 비즈니스 로직 수행
# ================================================
# 라이브러리 읽기
library(tidyverse)
library(lubridate)
library(fs)

# 파일 조회
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "Data+for+Quiz+3.csv"))

# 데이터 읽기
data = readr::read_csv(fileList)

# 데이터 병합

# 1. 아래와 같이 wt (몸무게) 와 ht (키) 변수를 나타내는 산점도를 그리세요 (50점)
# 성별 (남, 여)을 기준으로 몸무게와 키에 대한 산점도를 시각화하였습니다.
# 그 결과 몸무게와 키의 관계는 선형적으로 증가하는 경향이 나타남
# 특히 남자의 경우 평균 몸무게/키 (71.90/173.61)은 여자 (55.21/160.74)보다 높은 값을 보임

mainTitle = "키와 몸무게 간의 산점도"
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data, aes(x = ht, y = wt, color = sex)) +
  geom_point() +
  theme_classic() +
  labs(x = "Height", y = "Weight", color = "Sex", title = "Average Height and Weight") +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "darkblue", hjust = 0.5)
    , axis.text = element_text(size = 14)
    , axis.title = element_text(size = 16, face = "bold", color = "black")
    , legend.text = element_text(size = 14)
    , legend.title = element_text(size = 16, face = "bold", color = "black")
    , legend.position = c(0.075, 0.925)
  ) +
  geom_hline(yintercept = mean(subset(data, sex %in% c("Female"))$wt, na.rm = TRUE), linetype = "dashed", color = "#F8766D") +
  geom_vline(xintercept = mean(subset(data, sex %in% c("Female"))$ht, na.rm = TRUE), linetype = "dashed", color = "#F8766D") +
  geom_hline(yintercept = mean(subset(data, sex %in% c("Male"))$wt, na.rm = TRUE), linetype = "dashed", color = "#00BFC4") +
  geom_vline(xintercept = mean(subset(data, sex %in% c("Male"))$ht, na.rm = TRUE), linetype = "dashed", color = "#00BFC4") +
  annotate("text", label = "What can we tell from this plot?", x = 180, y= 30, size = 5, fontface = "italic", color = "orangered") +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 2. 학년과 수학 점수를 나타내는 막대그래프를 그리세요 (50점)
# 학년 (1, 2, 3, 4)을 기준으로 수학 등급 (A, B, C, D or Below)에 따라 개수를 막대그래프로 시각화하였습니다.
# 그 결과 B와 C 수학 등급은 타 등급 (A, D or Below)보다 많은 빈도수를 보임
# 특히 C 수학 등급 내에서도 4학년은 최대 빈도수를 나타냄

mainTitle = "학년별 수학점수 막대그래프"
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data, aes(x = math_grade, fill = as.factor(class))) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(x = "Grade", y = "Number of Students", fill = "Class", subtitle = "... any difference?", title = "Math Exam Results (by Class)") +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5)
    , plot.subtitle = element_text(size = 10, color = "black", hjust = 0.5)
    , axis.text = element_text(size = 14)
    , axis.title = element_text(size = 16, face = "bold", color = "black")
    , legend.text = element_text(size = 14)
    , legend.title = element_text(size = 16, face = "bold", color = "black")
    , legend.position = c(0.9, 0.45)
    , legend.background = element_rect(fill = "white", color = "black")
  ) +
  annotate("text", x = 1, y= 100, size = 5, label = sprintf("Overall Mean is %s", round(mean(data$math, na.rm = TRUE), 1)), fontface = "italic", color = "grey") +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
# 3. 성별과 영어점수를 나타내는 막대그래프를그리세요. 
# 둘중 하나 (왼쪽 proportion stacked vs 오른쪽 stacked) 선택해서 그리시면 됩니다
# 성별 (남, 여)을 기준으로 영어 등급 (A, B, C, D or Below)에 따라 개수를 막대그래프로 시각화하였습니다.
# 그 결과 B와 C 수학 등급은 타 등급 (A, D or Below)보다 많은 빈도수를 보임
# 특히 C 영어 등급 내에서도 B학년은 최대 빈도수를 나타냄

mainTitle = "학년별 영어점수 막대그래프"
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data, aes(x = eng_grade, fill = sex)) +
  geom_bar(position = "stack") +
  theme_classic() +
  labs(x = "Grade", y = "Number of Students", fill = "Sex", title = "English Exam Results (by Sex)") +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5)
    , axis.text = element_text(size = 14)
    , axis.title = element_text(size = 16, face = "bold", color = "black")
    , legend.text = element_text(size = 14)
    , legend.title = element_text(size = 16, face = "bold", color = "black")
    , legend.position = c(0.075, 0.925)
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
