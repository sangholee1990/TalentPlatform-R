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
# R을 이용한 한전 파워플래너 로그인 및 일별요금 CSV 파일 적재

# 로그인 ID (고객번호: 0422206954) , 패스워드(yusan2f6931)는 개발 시작시 제공
# 년월을 지정하고 , 프로그램을 가동하면 , 
# 데이터가 .csv 파일에 저장될 수 있으면 좋겠습니다.
# 
# 3. 예)  
# 검색기간:  2022년 1월 ~  2022년 05월
# 
# 실행 엔터
# 
# 지정된 폴더에 자동으로 고객번호 끝 4자리와 조합해서
# 6954.2022.01.csv 
# 6954.2022.02.csv
# 6954.2022.03.csv
# 6954.2022.04.csv
# 6954.2022.05.csv 
# 
# 저장할 수 있으면 좋겠습니다.


# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0451"

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
library(RSelenium)
library(wdman)
library(webdriver)


sysOpt = list(
  "userId" = "0422206954"
  , "userPw" = "yusan2f6931"
)

# RSelenium::checkForServer()
# RSelenium::startServer()
# java -jar selenium-server-standalone-x.xx.x.jar

driver <- rsDriver(browser = "chrome")
remote_driver <- driver[["client"]]



# 1안)
# https://github.com/rstudio/webdriver

# 설치 방법
# webdriver::install_phantomjs()
pjs = webdriver::run_phantomjs()
pjs


# https://pp.kepco.co.kr/ 에 id/pwd 를 입력한후
# https://pp.kepco.co.kr/re/re0103N.do?menu_id=O010406 일별요금에서

ses = Session$new(port = pjs$port)

# 화면 이동
ses$go("https://pp.kepco.co.kr/intro.do")
ses$getUrl()


# 사용자 이름과 비밀번호 입력 필드를 찾습니다.
# username_element <- ses$findElement(using = 'xpath', '//*[@id="RSA_USER_ID"]')
# password_element <- ses$findElement(using = 'xpath', "//input[@id='password']")

ses$executeScript("$('#RSA_USER_ID').val(arguments[0]);", list(sysOpt$userId))
# ses$takeScreenshot()

ses$executeScript("$('#RSA_USER_PWD').val(arguments[0]);", list(sysOpt$userPw))
# ses$takeScreenshot()

# 자바스크립트를 사용하여 로그인 버튼을 클릭합니다.
ses$executeScript("$('#intro_form > form > fieldset > input.intro_btn').click();", list())

# 스크린샷을 찍습니다.
ses$takeScreenshot()


# 사용자 이름과 비밀번호를 입력합니다.
username_element$sendKeysToElement(list("your_username"))
password_element$sendKeysToElement(list("your_password"))

# 로그인 버튼을 클릭합니다.
# login_button <- ses$findElement(using = 'xpath', '//*[@id="intro_form"]/form/fieldset/input[1]')
# login_button$clickElement()

# 페이지의 제목을 확인합니다.
ses$getTitle()

# 스크린샷을 찍습니다.
ses$takeScreenshot()


# pjs <- wdman::phantomjs(port = 4567L)
# remDr <- remoteDriver(browserName = "phantomjs", port = pjs$port)
# remDr$open()






# 2안)
# cd /c/selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

# remDr = remoteDriver(
#   remoteServerAddr = "localhost"
#   , port = 5000L
#   , browserName = "chrome"
# )





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
