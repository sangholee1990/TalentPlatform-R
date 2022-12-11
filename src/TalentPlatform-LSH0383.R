
# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0383"

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
# install.packages("stringr")
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)


# df <- read.csv(file="C:/Users/User/Desktop/kdramalist.csv", header=T, fileEncoding='euc-kr')

# 2022.12.12
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "kdramalist.csv"))
df = readr::read_csv(file = fileInfo) %>%
  readr::type_convert() %>%
  dplyr::rename(
    "Aired.On" = "Aired On"
    , "Original.Network"  = "Original Network"
  ) %>%
  mutate_at(c("score", "Episodes", "Duration", "Watchers", "Popularity", "Aired.On"), as.numeric)

# df$`Original Network`
#############전처리$############
sum(df$Original.Network=='N/A')
df <- subset (df, df$drama_name!="N/A")
df <- subset (df, df$Genres!="N/A")
df <- subset (df, df$Episodes!="N/A")
df <- subset (df, df$Aired.On!="N/A")
df <- subset (df, df$Original.Network!="N/A")
df <- subset (df, df$Duration!="N/A")
df <- subset (df, df$score!="N/A")
df <- subset (df, df$Popularity!="N/A")
df <- subset (df, df$Watchers!="N/A")
df <- subset (df, df$platforms!="N/A")
df <- subset (df, df$platforms!="[]")

summary(df)

# df$score <- as.numeric(df[,'score'])
# df$Episodes <- as.numeric(df[,'Episodes'])
# df$Duration <- as.numeric(df[,'Duration'])
# df$Watchers <- as.numeric(df[,'Watchers'])
# df$Popularity <- as.numeric(df[,'Popularity'])
# df$Aired.On <- as.numeric(df[,'Aired.On'])
df$Original.Network <- str_sub(df$Original.Network,1,regexpr(",",df$Original.Network))
df$Original.Network <- gsub("[[:punct:]]", "", df$Original.Network)
df$Genres <- str_sub(df$Genres,1,regexpr(",",df$Genres))
df$Genres <- gsub("[[:punct:]]", "", df$Genres)
df$platforms <- str_sub(df$platforms,1,regexpr(",",df$platforms))
# df$platforms <- gsub("[[:punct:]]", "", df$plat)
head(df)
summary(df)

ggplot(df, aes(x = Original.Network)) +
  geom_bar(colour = "#FF9999") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

ggplot(df, aes(x = Genres)) +
  geom_bar(colour = "#FF9999") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

ggplot(df, aes(x = platforms)) +
  geom_bar(colour = "#FF9999") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

################범주형 데이터 수치형 변환################
select<-sapply(df,function(x) {is.character(x)})
select
df[,select]<-lapply(df[,select],factor)
df

df$drama_name <- as.numeric(df$drama_name)
df$Original.Network <- as.numeric(df$Original.Network)
df$Genres <- as.numeric(df$Genres)
df$platforms <- as.numeric(df$platforms)
df
###Episodes###
plot(df$Episodes,df$score,col='red',pch=20,cex=1.5,
     main='Episodes',xlab='Episodes',ylab='df$score')
cor(df$Episodes,df$score)
epi_lm <- lm(df$score~df$Episodes,data=df)
epi_lm$coefficients
abline(coef(epi_lm))
summary(epi_lm)

###Duration###
plot(df$Duration,df$score,col='red',pch=20,cex=1.5,
     main='Duration',xlab='Duration',ylab='df$score')
cor(df$Duration,df$score)
dur_lm <- lm(df$score~df$Duration,data=df)
dur_lm$coefficients
abline(coef(dur_lm))
summary(dur_lm)

###Watchers###
plot(df$Watchers,df$score,col='red',pch=20,cex=1.5,
     main='Watchers',xlab='Duration',ylab='df$score')
cor(df$Watchers,df$score)
wat_lm <- lm(df$score~df$Watchers,data=df)
wat_lm$coefficients
abline(coef(wat_lm))
summary(wat_lm)

###Popularity###
plot(df$Popularity,df$score,col='red',pch=20,cex=1.5,
     main='Popularity',xlab='Popularity',ylab='df$score')
cor(df$Popularity,df$score)
pop_lm <- lm(df$score~df$Popularity,data=df)
pop_lm$coefficients
abline(coef(pop_lm))
summary(pop_lm)

###Aired_on###
plot(df$Aired.On,df$score,col='red',pch=20,cex=1.5,
     main='Aired_on',xlab='aired_ons',ylab='df$score')
cor(df$Aired.On,df$score)
aired_on_lm <- lm(df$score~df$Aired.On,data=df)
aired_on_lm$coefficients
abline(coef(aired_on_lm))
summary(aired_on_lm)

###Original Network###
plot(df$Original.Network,df$score,col='red',pch=20,cex=1.5,
     main='Original Network',xlab='Popularity',ylab='df$score')
cor(df$Original.Network,df$score)
ori_net_lm <- lm(df$score~df$Original.Network,data=df)
ori_net_lm$coefficients
abline(coef(ori_net_lm))
summary(ori_net_lm)

###Genres###
plot(df$Genres,df$score,col='red',pch=20,cex=1.5,
     main='Popularity',xlab='Popularity',ylab='df$score')
cor(df$Genres,df$score)
gen_lm <- lm(df$score~df$Genres,data=df)
gen_lm$coefficients
abline(coef(gen_lm))
summary(gen_lm)

###Platforms###
plot(df$platforms,df$score,col='red',pch=20,cex=1.5,
     main='Popularity',xlab='Popularity',ylab='df$score')
cor(df$platforms,df$score)
plat_lm <- lm(df$score~df$platforms,data=df)
plat_lm$coefficients
abline(coef(plat_lm))
summary(plat_lm)






#########협업필터링###########
# install.packages("SnowballC") #tf-idf를 구성하기 위한 패키지
# install.packages("class") #KNN 분석을 위한 패키지
# install.packages("dbscan") #KNN 분석을 위한 패키지
# install.packages("proxy") #코사인 유사도, 거리를 계산하기 위한 패키지
# install.packages("recommenderlab") #추천 시스템을 위한 패키지
# install.packages("dplyr") #데이터 프레임을 처리하는 함수 패키지
# install.packages("tm") #tf-idf matrix를 구성하기 위한 패키지
# install.packages("caTools")

library(recommenderlab)
library(dplyr)
library(tm)
library(SnowballC)
library(class)
library(dbscan)
library(proxy)
library(caTools)
data <- df

data.frame2matrix = function(data, rowtitle, coltitle, datatitle, 
                             rowdecreasing = FALSE, coldecreasing = FALSE,
                             default_value = NA) {
  ## 열 이름 존재하는지 확인
  if ( (!(rowtitle%in%names(data))) 
       || (!(coltitle%in%names(data))) 
       || (!(datatitle%in%names(data))) ) {
    stop('data.frame2matrix: bad row-, col-, or datatitle.')
  }
  
  ## 열 개수
  ndata = dim(data)[1]
  
  ## 행 및 열 이름
  rownames = sort(unique(data[[rowtitle]]), decreasing = rowdecreasing)
  nrows = length(rownames)
  colnames = sort(unique(data[[coltitle]]), decreasing = coldecreasing)
  ncols = length(colnames)
  
  ## matrix 초기화
  out_matrix = matrix(NA, 
                      nrow = nrows, ncol = ncols,
                      dimnames=list(rownames, colnames))
  
  ## 데이터의 행 반복
  for (i1 in 1:ndata) {
    ## 현재 데이터 행에 대한 행렬-행 및 행렬-열 색인 가져오기
    iR = which(rownames==data[[rowtitle]][i1])
    iC = which(colnames==data[[coltitle]][i1])
    
    ## matrix 항목 (iR, iC)가 공백이 아니라면 에러 발생
    if (!is.na(out_matrix[iR, iC])) stop('data.frame2matrix: double entry in data.frame')
    out_matrix[iR, iC] = data[[datatitle]][i1]
  }
  
  ## matrix 결측치에 default 값 삽입하기
  out_matrix[is.na(out_matrix)] = default_value
  
  # return matrix
  return(out_matrix)
  
}
# colnames(data) <- c('user_id','item_id','rating','timestamp')

summary(df)

pre_data = data.frame2matrix(data, 'drama_name', 'Genres', 'score')
target_data <- as(as.matrix(pre_data), "realRatingMatrix")


# Train:Test = 7:3
# 2022.12.12
# spl <- sample.split(data$rating,0.7)
spl <- sample.split(data$score, 0.7)

train <- subset(data,spl==TRUE)
test <- subset(data,spl==FALSE)
