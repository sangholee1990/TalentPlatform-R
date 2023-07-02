
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
# R을 이용한 트위터 데이터 전처리 및 통계 분석 (트윗수, 백분율, 특정 키워드 분석, PCA, n-gram, 2-gram, 선형회귀)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0314"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

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
library(tidyverse)
library(lubridate)
library(openxlsx)
library(dplyr)
library(readxl)
library(ggcorrplot)
library(NbClust)
library(tidyverse)
library(psych)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(dplyr)
library(fitdistrplus)
library(stats)
library(gutenbergr)
library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(forcats)
library(ggraph)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(textdata)
library(ggraph)
library(igraph)
library(ggnet)
library(networkD3)
library(GGally)
library(gutenbergr)
library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(forcats)
library(ggraph)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(textdata)
library(ggraph)
library(wordcloud2)
library(htmlwidgets)
library(webshot)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

# devtools::install_github("briatte/ggnet")

# install.packages("tidyverse")
# install.packages("reshape")
# install.packages("NbClust")
# install.packages("gmodels")

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "data_a2.csv"))
All_Data = readr::read_csv(fileInfo, locale=locale(encoding="UTF-8"))
# All_Data = readr::read_csv("./data_a2.csv", locale=locale(encoding="UTF-8"))
# n은 tweet, sumVal은 리트윗

# 데이터에서 '([^\'A-Za-z0-9@#_]+)' 삭제

# preg_replace("([^\'A-Za-z0-9@#_+])","",$All_Data);

All_Data$text2 = All_Data$text %>% 
  str_replace_all(pattern = "([^\'A-Za-z0-9@#_+])", replacement = " ") 

Time_All_Data <-
  All_Data %>%
  group_by(created_at) %>%
  summarise(
    sumVal = sum(retweet_count, na.rm = TRUE)
    , n = n()
  )

Max_Data = Time_All_Data %>% 
  dplyr::filter(n == max(n, na.rm = TRUE))

ggplot(data = Time_All_Data,
       mapping = aes(x = created_at, y= n)) + 
  geom_line() +
  geom_point(data = Max_Data, mapping = aes(x = created_at, y= n), color = "red")

# geom_pint(pch = c(2,4,6)[All_Data$Max], size = c(.8,.9,1)[All_Data$Max], 
# col=c('red','blue','black)[All_Data$Max)

# dplyr 방법에서 group_by를 summarise 통계 계산
# ggplot2으로 x축, y축, 컬럼값을 지정해서 시각화 

# Question 1 (20pt, 5pt each subquestion)
# As we have seen in , crawling data from the internet comes with several issues. One example is how text is encoded. To deal with that problem, please load the dataset using “utf-8” character encoding. 
# Tweets have several characters that are not easily dealt programmatically. For instance, emoticons have several representations depending on the encoding used. We want to simplify the dataset for our analysis. Please, remove any character that matches the following regular expression (regex, https://en.wikipedia.org/wiki/Regular_expression): '([^\'A-Za-z0-9@#_ ]+)'. 
# 1) Plot the number of tweets over time. 


# a) Do you see any major peaks in the number of tweets? 
#   b) If so, when is that?
#   a) 
# b) Date: 2020-02-01


# 2)	

# a) Plot the emotional distribution of our dataset over time. In other words, plot the percentage of tweets by emotion over time. 
# anger,disgust,fear,joy,optimism

# optimism
All_Data_Optimism = All_Data %>%
  group_by(created_at) %>%
  summarise(
    sumVal = sum(optimism, na.rm = TRUE)
    , n = n()
  ) %>% 
  dplyr::mutate(
    ratio = sumVal / n * 100.0
  )

ggplot(data = All_Data_Optimism,
       mapping = aes(x = created_at, y=ratio)) + 
  geom_line()
# geom_point(data = Max_Data, mapping = aes(x = created_at, y= ratio), color = "red")

# anger
All_Data_Anger = All_Data %>%
  group_by(created_at) %>%
  summarise(
    sumVal = sum(anger, na.rm = TRUE)
    , n = n()
  ) %>% 
  dplyr::mutate(
    ratio = sumVal / n * 100.0
  )
ggplot(data = All_Data_Anger,
       mapping = aes(x = created_at, y=ratio)) + 
  geom_line()



# disgust
All_Data_Disgust = All_Data %>%
  group_by(created_at) %>%
  summarise(
    sumVal = sum(disgust, na.rm = TRUE)
    , n = n()
  ) %>% 
  dplyr::mutate(
    ratio = sumVal / n * 100.0
  )
ggplot(data = All_Data_Disgust,
       mapping = aes(x = created_at, y=ratio)) + 
  geom_line()


# fear
All_Data_Fear = All_Data %>%
  group_by(created_at) %>%
  summarise(
    sumVal = sum(fear, na.rm = TRUE)
    , n = n()
  ) %>% 
  dplyr::mutate(
    ratio = sumVal / n * 100.0
  )
ggplot(data = All_Data_Fear,
       mapping = aes(x = created_at, y=ratio)) + 
  geom_line()


# joy 
All_Data_Joy =  All_Data %>%
  group_by(created_at) %>%
  summarise(
    sumVal = sum(joy, na.rm = TRUE)
    , n = n()
  ) %>% 
  dplyr::mutate(
    ratio = sumVal / n * 100.0
  )
ggplot(data = All_Data_Joy,
       mapping = aes(x = created_at, y=ratio)) + 
  geom_line()


# All Together Visualization
Data = dplyr::bind_rows(
  data.frame(All_Data_Optimism, type = "optimism")
  ,  data.frame(All_Data_Joy, type = "joy")
  , data.frame(All_Data_Anger, type = "anger")
  , data.frame(All_Data_Disgust, type = "disgust")
  , data.frame(All_Data_Fear, type = "fear"))

ggplot(data = Data, aes(x = created_at, y=ratio, color = type)) + 
  geom_line()


# b) Which emotion dominates our dataset?
# -> Joy
#   c) Which emotion is the least prevalent in our dataset? 
# -> Fear
#   d) Do you see any significant shifts in emotions in the dataset? If so, when is that?
# -> In between middle of December and beginning of January

# 3)	Please, convert all the characters in the dataset to lower case. Use this lower case dataset 
# in all subsequent analyses, unless explicitly stated otherwise. 

All_Data$text = tolower(All_Data$text)
All_Data$text

# Identify COVID-related tweets using the keywords “covid” and “corona”.
# Make it so that any tweet before December 31, 2019 that was identified 
# as COVID-related is not classified as such. This is because WHO confirmed the 
# first COVID-19 case on December 31, 2019. #covid, corona (시간2019년 12월 31일부터 시작만)

All_DataL1 = All_Data %>% 
  dplyr::mutate(
    isYn = dplyr::case_when(
      stringr::str_detect(text2, regex("#covid|corona")) ~ "Y"
      , TRUE ~ "N"
    )
  ) %>% 
  dplyr::filter(
    created_at >= as.Date("2019-12-31")
    , isYn == "Y"
  )
All_DataL1

# a) How many tweets in our dataset are COVID-19 related?
length(All_DataL1$text2)
# -> 9824

#   b) Plot the number of COVID-related tweets over time.
All_DataL1_OT = All_DataL1 %>%
  group_by(created_at) %>%
  summarise(
    sumVal = sum(optimism, na.rm = TRUE)
    , n = n()
  ) %>% 
  dplyr::mutate(
    ratio = sumVal / n * 100.0
  )

ggplot(data = All_DataL1_OT,
       mapping = aes(x = created_at, y=sumVal)) + 
  geom_line()


# c) Are COVID-related tweets evenly distributed throughout our dataset?
# -> The tweets are not evenly distributed as it can be seen in the graph
# In January, it has the highest percentage or the COVID-related tweets, and
# goes up and down, and since end of 2020 January the distribution becomes quite
# flat again.

###################여기까지 완료 ####################################
###################여기 이후부터 개발 ###############################
##############################################################
##############################################################
##############################################################
##############################################################

# 4)	a) Plot the emotional distribution of COVID-related tweets over time.
#해석: 코로나에 관련된(covid,corona) 트위터정보를 감정의 시간에 따른 변화를 시각화하라
All_DataL2 = All_DataL1 %>% 
  dplyr::select(created_at, anger, disgust, fear, joy, optimism) %>% 
  tidyr::gather(-created_at, key = "key", value = "val") %>% 
  dplyr::group_by(created_at, key) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  )

ggplot(data = All_DataL2, aes(x = created_at, y=sumVal, color = key)) + 
  geom_line()


# b) Do you see any unexpected patterns in early January? If so, why do you think that’s the case?
# 해석: 1월 초에 특이한 패턴이 보이는가? 이유는 무엇일까?

# 1월 초에서는 모든 감정이 낮은 빈도룰 보였으나 2월 1일부터 높은 빈도로 증가함

#   c) What are the predominant emotions in COVID-related tweets from February 2020?
# 해석: 2020년 2월부터 코로나에 관련이 가장 높은 감정은 무엇인가?

# disgust 감정
All_DataL2 %>% 
  dplyr::filter(
    created_at >= as.Date("2020-02-01")
    ) %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    maxVal = max(sumVal, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(desc(maxVal))


#   d) What are the least prevalent emotions in COVID-related tweets from February 2020?
#  해석: 2020년 2월부터 코로나에 관련이 가장 낮은 감정은 무엇인가?

# joy, optimism 감정
All_DataL2 %>% 
  dplyr::filter(
    created_at >= as.Date("2020-02-01")
  ) %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    minVal = min(sumVal, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(minVal)

#   e) Discuss the differences between the emotions expressed 
# in all tweets in our dataset and COVID-related tweets.
# 해석: 전체 데이터랑(All_Data), 2019년 12월 31일부터 코로나 관련된 트위터랑 
# 차이점을 유의미한 통계방법으로 설명 하시오.

# 전체 데이터의 경우 앞선 코로나 키워드에 따른 시계열과 달리 다양한 증가/감소 폭을 보여주며
# 특히 2월 1일에서는 상당히 높은 빈도수를 나타냄

All_DataL2 = All_Data %>% 
  dplyr::select(created_at, anger, disgust, fear, joy, optimism) %>% 
  dplyr::filter(
    created_at >= as.Date("2019-12-31")
  ) %>% 
  tidyr::gather(-created_at, key = "key", value = "val") %>% 
  dplyr::group_by(created_at, key) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  )

ggplot(data = All_DataL2, aes(x = created_at, y=sumVal, color = key)) + 
  geom_line()

# Question 2 (10pt, 5pt each subquestion)
# We want to test if the emotional distribution of COVID-related tweets 
# is different from that of our complete dataset.
# 1)	Disregarding the temporal aspect of our data, calculate the emotional distribution of COVID-related tweets and our complete dataset. Report the number of tweets, not the percentage.
# Anger	Disgust	Fear	Joy	Optimist
# All Tweets					
# COVID-Related					
# 
# 2)	Using a chi-square test, is there a difference in the emotions expressed in COVID-related tweets in comparison to our complete dataset? Report the appropriate statistics.
# Answer: 
#   
#   Question 3 (10pt, 2.5pt each item)
# It is hard to visualize the emotional distribution of tweets because it is high dimensional. We want to reduce it by using Principal Component Analysis (PCA). Consider that a tweet can be identified as a 5-dimensional vector indicating the presence (or absence) of each emotion. Use all tweets for this question.
# 1)	a) Draw a scree plot to identify how many components should be retained. Use the rule of thumb taught in class. How many components should we retain?
#   b) Run PCA using the number of components you identified above. Explain each component with respect to the 5 emotions. Note that writing the formula will not be considered an explanation. Write down your interpretation. Focus on loadings that are greater than 0.50.
# c) Plot a biplot of these two components. 
# d) Is the number of components you have chosen in (a) appropriate after seeing the biplot? Are we losing some significant information by reducing it to this number of components?
#   
# a) 
# b) 
# c) 
# d) 
# 
# Question 4 (10pt, 2.5pt each subquestion)
# We now want to run the same analysis as in Question 3, but restricting our analysis to COVID-related tweets. 
# 1)	a) Draw a scree plot to identify how many components should be retained. Use the rule of thumb taught in class. How many components should we retain?
#   b) Run PCA using the number of components you identified above. Explain each component with respect to the 5 emotions. Note that writing the formula will not be considered an explanation. Write down your interpretation. Focus on loadings that are greater than 0.50.
# c) Do you see any significant differences between the PCA analyses in Q3 and Q4?
#   d) Is the number of components you have chosen in (a) appropriate? Are we losing some significant information by reducing it to this number of components? If so, what can we do about it?
#   
#   a) 
# b) 
# c) 
# d)
# Question 5 (10pt, 5pt each item)
# We now want to run the same analysis as in Question 3, but at the user level. Consider that a user U can be identified by a 5-dimensional vector containing the number of tweets shared by U classified as each emotion. Scale each dimension of users’ vector representations to mean = 0 and standard deviation = 1. Use all tweets for this question.
# 1)	a) Draw a scree plot to identify how many components should be retained. Use the rule of thumb taught in class. How many components should we retain?
#   b) Run PCA using the number of components you identified above. Explain each component with respect to the 5 emotions. Note that writing the formula will not be considered an explanation. Write down your interpretation. Focus on loadings that are greater than 0.50.
# 
# a)	
# b)	
# 
# Note: For Q6 and Q7, focus on the tweets posted in 2020. In other words, keep only the tweets that were posted on Jan 1st, 2020 and after (including Jan 1st, 2020).
# Question 6 (10pt, 2pt-4pt-4pt)
# Let’s use n-grams and graphs to understand the relationship between words in our dataset. Use the complete dataset (i.e., not the COVID-related one) from now on.
# 1)	What are the 5 most frequent 2-grams in our dataset? Remove any 2-grams that contain stop words identified by the stop_words object in the TidyText library. Remove any 2-grams that contain the NA tag.
# Rank	Word 1	Word 2
# 1		
# 2		
# 3		
# 4		
# 5		
# 2)	Create an unweighted graph with 2-grams that appear more than twice.
# a) How many nodes are there in this graph?
#   b) How many edges are there in this graph?
#   c) Calculate the betweenness and degree of each node in this graph. What are the 6 nodes with the highest betweenness? What is their betweenness and degree?
#   a) 
# b) 
# c) 
# Rank	Word	Betweenness	Degree
# 1			
# 2			
# 3			
# 4			
# 5			
# 6			
# 3)	Create a weighted graph with 2-grams that appear more than twice.
# a) Calculate the betweenness of each node in this graph. What are the 6 nodes with the highest betweenness? What is their betweenness?
#   a) 
# Rank	Word	Betweenness
# 1		
# 2		
# 3		
# 4		
# 5		
# 6		
# 
# Question 7 (35pt, 5pt-5pt-5pt-10pt-10pt)
# We want to see how users are connected on Twitter depending on the hashtags they use and the users they mention in their tweets. Do not forget to use lower case tweets and our complete dataset.
# 1)	a) Extract the first hashtag included in each tweet using regex (‘#([A-Za-z0-9_]+)’). Note that we want only the first hashtag. How many tweets contain at least one hashtag?
#                                                                      b) Extract the first mention included in each tweet using regex (‘@([A-Za-z0-9_]+)’). Note that we want only the first mention. How many tweets contain at least one mention?
#   a) 
# b) 
# 2)	a) How many unique hashtags did you identify in (1a)?
#   b) How many unique mentions did you identify in (1b)?
#   a)  
# b) 
# 3)	What are the 10 most frequent hashtags and mentions?
#   Hashtags: 
#   Mentions: 
#   4)	a) Create an unweighted and undirected graph where users are connected if they mentioned the same person in the time period covered by our dataset. Do not include self-loops. Consider only the top-10 Twitter handles identified in (3). 
# - How many nodes and edges are there in this graph?
#   b) Calculate the betweenness, the closeness, and the eigencentrality of each node in this graph. 
# - What are the top-3 nodes according to each of these three metrics? Present the userID. 
# - Are there differences between three centrality measures according to the top-3 nodes? If so, explain why that is so.
# c) Use the cluster_fast_greedy function to partition the graph.
# - How many clusters are there?
#   - How big is each cluster?
#   a) 
# b) 
# Rank	Betweenness	Closeness	Eigencentrality
# 1			
# 2			
# 3			
# c) 
# 5)	a) Create a weighted and undirected graph where users are connected if they used the same hashtag in the time period covered by our dataset. Do not include self-loops. Consider only the top-10 hashtags identified in (3). The weight of each edge is the number of hashtags that are shared by users (i.e., nodes). 
# - How many nodes and edges are there in this graph?
#   - Plot the edge weight distribution. Describe the distribution.
# - What’s the maximum edge weight?
#   b) Calculate the betweenness and the eigencentrality of each node in this graph. 
# - Find the three highest values of betweenness and centrality in this graph. What are the top nodes according to each of these metrics? Present the userID. 
# - Are there differences between the two centrality measures according to the top nodes? If so, explain why that is so.
# - Plot the CDF of betweenness and eigentrality values. How is their distribution different?
#   c) Use the cluster_fast_greedy function to partition the graph.
# - How many clusters are there?
#   - How big is each cluster?
#   a) 
# b) 
# c) 

# Question 8 (10pt) 리그레션 (간단한 선형회귀 사용)
# The last question of this assignment is open-ended. We want you to think about a research question you would like to answer using the dataset you were provided. Please formulate a question that you can test using the tools you have learned in CS564 until now. Your answer should include at least one topic that you learned after the midterm. Use the dataset you have after completing the whole assignment.
# a)	Please formulate your research question.
# b)	Which tool you have learned in is useful in answering this question?
#   c)	What are your results?
#   d)	Explain your results.
# 
# a)	
# b)	
# c)	
# 
# 
# 

############################### 해석 ###############################
############################### 해석 ###############################
############################### 해석 ###############################
############################### 해석 ###############################
# Q2부터 해석

# ==============================================================================
# 질문 2 
# ==============================================================================
# 코로나 관련 트윗의 감정 분포 여부를 테스트하고 싶습니다.
# 전체 데이터세트와 다릅니다.
# 1) 데이터의 시간적 측면을 무시하고 COVID 관련 트윗과 전체 데이터 세트의 감정 분포를 계산합니다. 
# 단, 백분율이 아니라 트윗 수를 보고하십시오.
# 칼럼:                	Anger	    Disgust  	Fear	 Joy	 Optimist
# 모든 트윗에 관하여: 

# 코로나 데이터 시기 트윗수

# # A tibble: 5 x 2
# key      sumVal
# <chr>     <dbl>
#   1 anger      1856
# 2 disgust    2307
# 3 fear       2689
# 4 joy         792
# 5 optimism    830 

Covid_Data_Info = All_Data %>% 
  dplyr::mutate(
    isYn = dplyr::case_when(
      stringr::str_detect(text2, regex("#covid|corona")) ~ "Y"
      , TRUE ~ "N"
    )
  ) %>% 
  dplyr::filter(
    created_at >= as.Date("2019-12-31")
    , isYn == "Y"
  ) %>% 
  dplyr::select(anger, disgust, fear, joy, optimism) %>% 
  tidyr::gather(key = "key", value = "val") %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  )


# 전체 데이터 시기 트윗수

# > All_Data_Info
# # A tibble: 5 x 2
# key      sumVal
# <chr>     <dbl>
#   1 anger    111189
# 2 disgust  109669
# 3 fear      20092
# 4 joy      200632
# 5 optimism 148874

All_Data_Info = All_Data %>% 
  dplyr::filter(
    created_at >= as.Date("2019-12-31")
  ) %>% 
  dplyr::select(anger, disgust, fear, joy, optimism) %>% 
  tidyr::gather(key = "key", value = "val") %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  )

# 2) 카이 제곱 테스트를 사용하여 전체 데이터 세트와 
# 비교하여 COVID 관련 트윗에 표현된 감정에 차이가 있습니까? 
# 적절한 통계를 보고합니다.
# 대답:

library(gmodels)

Covid_Data_Info = All_Data %>% 
  dplyr::mutate(
    isYn = dplyr::case_when(
      stringr::str_detect(text2, regex("#covid|corona")) ~ "Y"
      , TRUE ~ "N"
    )
  ) %>% 
  dplyr::filter(
    created_at >= as.Date("2019-12-31")
    , isYn == "Y"
  ) %>% 
  dplyr::select(anger, disgust, fear, joy, optimism) %>% 
  tidyr::gather(key = "key", value = "val") 

gmodels::CrossTable(Covid_Data_Info$key, Covid_Data_Info$val, chisq = TRUE)
# 카이 제곱 분석 결과 p값이 0.0로 나타났다.
# 따라서 유의수준이 0.05보다 낮기 때문에 감정별로 트윗수의 차이는 있다.
  
# ==============================================================================
# 질문 3 (10pt, 2.5pt 각 항목)
# ==============================================================================
# 트윗의 감정 분포는 고차원이기 때문에 시각화하기 어렵습니다. 
# PCA (Principal Component Analysis)를 사용하여 이를 줄이고자 합니다. 
# 트윗이 각 감정의 존재(또는 부재)를 나타내는 5차원 벡터로 식별될 수 있다고 생각하십시오.
# 이 질문에 대한 모든 트윗을 사용하십시오.

library(factoextra)

All_Data_Info = All_Data %>% 
  dplyr::filter(
    created_at >= as.Date("2019-12-31")
  ) %>% 
  dplyr::select(anger, disgust, fear, joy, optimism)

pcaRes = prcomp(All_Data_Info, center = FALSE, scale = FALSE)

# 요약
summary(pcaRes)


# 1) a) 얼마나 많은 구성 요소를 유지해야 하는지 식별하기 위해 스크리 플롯을 그립니다. 
# 얼마나 많은 구성 요소를 유지해야 합니까?
factoextra::fviz_eig(pcaRes)

# b) 위에서 식별한 구성 요소 수를 사용하여 PCA를 실행합니다. 5가지 감정에 대한 각 구성 요소를 설명합니다. 
# 당신의 해석을 쓰십시오. 0.50보다 큰 점수에 중점을 둡니다.
# 1~4 주성분의 경우 전체를 기준으로 98%을 설명할 수 있음
# 즉 각각 1번째 주성분 52%, 2번째 주성분 35%, 3번째 주성분 6%, 4번쨰 주성분 3%를 나타냄

# c) 이 두 성분의 biplot을 플로팅합니다.
# 시간 오래 걸림
factoextra::fviz_pca_biplot(
  pcaRes
  , repel = TRUE
  , col.var = "#2E9FDF" # Variables color
  , col.ind = "#696969"  # Individuals color
)

# d) (a)에서 선택한 성분의 수는 biplot을 본 후 적절한가?
# 이 구성 요소 수로 줄이면 중요한 정보가 손실됩니까?
# 1번째 주성분 (Dim1)에서 주로 disgust, anger (양의 영향), optimism, joy (음의 영향)을 주로 영향을 끼침
# 반면에 2번째 주성분에서는 fear 영향도 끼침 

# ==============================================================================
# 질문 4 
# ==============================================================================
# 이제 질문 3과 동일한 분석을 실행하고 싶지만 분석을 COVID 관련 트윗으로 제한합니다.

library(ggbiplot)

Covid_Data_Info = All_Data %>% 
  dplyr::mutate(
    isYn = dplyr::case_when(
      stringr::str_detect(text2, regex("#covid|corona")) ~ "Y"
      , TRUE ~ "N"
    )
  ) %>% 
  dplyr::filter(
    created_at >= as.Date("2019-12-31")
    , isYn == "Y"
  ) %>% 
  dplyr::select(anger, disgust, fear, joy, optimism) 

pcaRes = prcomp(Covid_Data_Info, center = FALSE, scale = FALSE)

summary(pcaRes)

# 1) a) 얼마나 많은 구성 요소를 유지해야 하는지 식별하기 위해 스크리 플롯을 그립니다. 
# 수업 시간에 배운 경험 법칙을 사용하십시오. 얼마나 많은 구성 요소를 유지해야 합니까?
factoextra::fviz_eig(pcaRes)

# b) 위에서 식별한 구성 요소 수를 사용하여 PCA를 실행합니다. 
# 5가지 감정에 대한 각 구성 요소를 설명합니다.
# 공식을 작성하는 것은 설명으로 간주되지 않습니다.
# 당신의 해석을 쓰십시오. 0.50보다 큰 하중에 중점을 둡니다.

factoextra::fviz_eig(pcaRes)

# 1~4 주성분의 경우 전체를 기준으로 96%을 설명할 수 있음
# 즉 각각 1번째 주성분 53%, 2번째 주성분 23%, 3번째 주성분 15%, 4번쨰 주성분 3%를 나타냄

# c) Q3와 Q4의 PCA 분석 사이에 유의미한 차이가 있습니까?
factoextra::fviz_pca_biplot(
  pcaRes
  , repel = TRUE
  , col.var = "#2E9FDF" # Variables color
  , col.ind = "#696969"  # Individuals color
)

# 1번째 주성분 (Dim1)에서 주로 disgust, anger, optimism, joy, fear (음의 영향)을 주로 영향을 끼침
# 특히 disgust에서 가장 큰 영양도 끼침

# d) (a)에서 선택한 구성 요소의 수는 적절합니까? 이 구성 요소 수로 줄이면 중요한 정보가 손실됩니까?
# 그렇다면 우리는 그것에 대해 무엇을 할 수 있습니까?
# 전체보다 코로나 시기에 따라 사람들의 트윗 감정을 파악할 수 있으며
# 특히 대면 시대에서 비대면으로 바뀌는 사회 구조적 환경으로 인해 disgust (싫음, 혐오감)에 대한 
# 반감이 생기는 것으로 파악됨

# ==============================================================================
# 질문 5 (10pt, 각 항목 5pt)
# ==============================================================================
# 이제 질문 3과 동일한 분석을 사용자 수준에서 실행하려고 합니다. 
# 각 감정으로 분류된 U가 공유한 트윗의 수를 포함하는 5차원 벡터로 사용자 U를 식별할 수 있다고 가정합니다.
# 사용자 벡터 표현의 각 차원을 평균 = 0 및 표준 편차 = 1로 조정합니다. 이 질문에 대한 모든 트윗을 사용합니다.

pcaRes = prcomp(Covid_Data_Info, center = TRUE, scale = TRUE)

summary(pcaRes)

# a) 얼마나 많은 구성 요소를 유지해야 하는지 식별하기 위해 스크리 플롯을 그립니다.
# 수업 시간에 배운 경험 법칙을 사용하십시오. 얼마나 많은 구성 요소를 유지해야 합니까?

factoextra::fviz_eig(pcaRes)

# 1~4 주성분의 경우 전체를 기준으로 96%을 설명할 수 있음
# 즉 각각 1번째 주성분 41%, 2번째 주성분 27%, 3번째 주성분 18%, 4번쨰 주성분 8%를 나타냄

# b) 위에서 식별한 구성 요소 수를 사용하여 PCA를 실행합니다. 5가지 감정에 대한 각 구성 요소를 설명합니다. 공식을 작성하는 것은 설명으로 간주되지 않습니다.
# 당신의 해석을 쓰십시오. 
# 0.50보다 큰 하중에 중점을 둡니다.
factoextra::fviz_pca_biplot(
  pcaRes
  , repel = TRUE
  , col.var = "#2E9FDF" # Variables color
  , col.ind = "#696969"  # Individuals color
)

# 1번째 주성분 (Dim1)에서 주로 disgust, anger (양의 영향), optimism, joy (음의 영향)을 주로 영향을 끼침
# 반면에 2번째 주성분에서는 fear 영향도 끼침

# 참고: 6분기와 7분기는 2020년 트윗에 집중합니다.
# 즉, 2020년 1월 1일 이후(2020년 1월 1일 포함)에 게시된 트윗만 유지합니다.

# ==============================================================================
# 질문 6 
# ==============================================================================
# n-gram과 그래프를 사용하여 데이터 세트의 단어 간의 관계를 이해합시다. 
# 지금부터 완전한 데이터 세트(COVID 관련 데이터가 아닌)를 사용하십시오.
# 1) 데이터 세트에서 가장 자주 사용되는 2-그램 5개는 무엇입니까?
# TidyText 라이브러리에서 stop_words 개체로 식별되는 
# 중지 단어를 포함하는 2-그램을 제거합니다. NA 태그가 포함된 2그램을 제거합니다.
# 순위      단어 1 단어 2 (컬럼이름임)
# 1
# 2
# 3
# 4
# 5
# 

# 자료 개수가 많아서 2019-12-31일 이후 10,000개 데이터 제한
All_Data_Info = All_Data %>% 
  dplyr::mutate(
    text2 = str_replace_all(text, pattern = "([^\'A-Za-z0-9@#_+])", replacement = " ")
  ) %>% dplyr::filter(
    created_at >= as.Date("2019-12-31")
  ) %>% 
  slice(1:1000)

All_Data_Info_L2 = All_Data_Info %>% 
  dplyr::select(text2) %>% 
  unnest_tokens(bigram, text2, token = "ngrams", n = 5) %>% 
  separate(bigram, c("Word1", "Word2"), sep = " ") %>%
  filter(
    ! is.na(Word1)
    , ! is.na(Word2)
    , Word1 != ""
    , Word2 != ""
    , ! Word1 %in% stop_words$word
    , ! Word2 %in% stop_words$word
  ) %>% 
  dplyr::count(Word1, Word2, sort = TRUE)


# 2) 2번 이상 나타나는 2-gram으로 무가중 그래프 (unweighted graph)를 생성합니다.
All_Data_Info_L3 = All_Data_Info_L2 %>% 
  graph_from_data_frame()

All_Data_Info_L3 %>%
  ggraph(layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_with=n), edge_colour="cyan4") +
  geom_node_point(color="navyblue", size=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()


# a) 이 그래프에는 몇 개의 노드가 있습니까?
summary(All_Data_Info_L3)
# 1772개


# b) 이 그래프에는 몇 개의 간선이 있습니까?
summary(All_Data_Info_L3)
# 1460개

# c) 이 그래프에서 각 노드의 사이(betweeness)와 정도(degree)를 계산합니다. 
# 가장 높은 간극을 가진 6개의 노드는 무엇입니까? 그들의 사이와 정도는 무엇입니까?

# betweenness와 degree는 각각 몇입니까?
# betweenness : 상위 10개, 전체 평균 61.72
All_Data_Info_L3 %>% 
  igraph::betweenness() %>% 
  head(n = 10)

All_Data_Info_L3 %>% 
  igraph::betweenness() %>% 
  mean(na.rm = TRUE)

# degree : 상위 10개, 전체 평균 1.65
All_Data_Info_L3 %>% 
  igraph::degree() %>%
  head(n = 10)


All_Data_Info_L3 %>% 
  igraph::degree() %>% 
  mean(na.rm = TRUE)

# 3) 2회 이상 나타나는 2그램으로 가중 그래프 (weighted graph)를 생성합니다.
# a) 이 그래프에서 각 노드의 사이를 계산합니다.
# 가장 높은 간극을 가진 6개의 노드는 무엇입니까?
# 그들의 사이는 무엇입니까?
# 순위 단어 사이

All_Data_Info_L4 = All_Data_Info %>% 
  dplyr::select(text2) %>% 
  unnest_tokens(bigram, text2, token = "ngrams", n = 2) %>% 
  separate(bigram, c("Word1", "Word2"), sep = " ") %>%
  filter(
    ! is.na(Word1)
    , ! is.na(Word2)
    , Word1 != ""
    , Word2 != ""
    , ! Word1 %in% stop_words$word
    , ! Word2 %in% stop_words$word
  ) %>% 
  dplyr::count(Word1, Word2, sort = TRUE) %>% 
  graph_from_data_frame()

# betweenness : 상위 10개, 전체 평균 389.33
All_Data_Info_L4 %>% 
  igraph::betweenness() %>% 
  head(n = 10)

All_Data_Info_L4 %>% 
  igraph::betweenness() %>% 
  mean(na.rm = TRUE)

# degree : 상위 10개, 전체 평균 1.76
All_Data_Info_L4 %>% 
  igraph::degree() %>%
  head(n = 10)

All_Data_Info_L4 %>% 
  igraph::degree() %>%
  mean(na.rm = TRUE)


# ==============================================================================
# 질문 7
# ==============================================================================
# 사용자가 사용하는 해시태그와 트윗에서 언급한 사용자에 따라 사용자가 
# Twitter에서 연결되는 방식을 보고 싶습니다. 
# 소문자 트윗과 전체 데이터 세트를 사용하는 것을 잊지 마십시오.
# 1) a) 각 트윗에 포함된 첫 번째 해시태그를 정규식('#([A-Za-z0-9_]+)')을 사용하여 추출합니다. 
# 첫 번째 해시태그만 필요합니다. 하나 이상의 해시태그가 포함된 트윗은 몇 개입니까?

All_Data_Info_L4 = All_Data %>% 
  dplyr::filter(
    stringr::str_detect(text, regex("#([A-Za-z0-9_]+)"))
  ) %>% 
  dplyr::mutate(
    keyword = str_extract_all(text, pattern = "#([A-Za-z0-9_]+)")
  )

# 총 129,532개
length(All_Data_Info_L4$keyword)

# b) 정규식('@([A-Za-z0-9_]+)')을 사용하여 각 트윗에 포함된 첫 번째 멘션을 추출합니다. 
# 첫 번째 멘션만 원한다는 점에 유의하십시오. 하나 이상의 멘션이 포함된 트윗은 몇 개입니까?

All_Data_Info_L5 = All_Data %>% 
  dplyr::filter(
    stringr::str_detect(text, regex("@([A-Za-z0-9_]+)"))
  ) %>% 
  dplyr::mutate(
    keyword = str_extract_all(text, pattern = "#([A-Za-z0-9_]+)")
  )

# 총 519,462개
length(All_Data_Info_L5$keyword)

# 나)
# 2) a) (1a)에서 몇 개의 고유한 해시태그를 식별했습니까?
All_Data_Info_L4 %>% 
  tidyr::unnest() %>% 
  dplyr::distinct(keyword)

# 60,998개

# b) (1b)에서 몇 개의 고유한 멘션을 식별했습니까?
All_Data_Info_L5 %>% 
  tidyr::unnest() %>% 
  dplyr::distinct(keyword)

# 24,636개

# 나)

dplyr::count(Word1, Word2, sort = TRUE)


# 2) 2번 이상 나타나는 2-gram으로 무가중 그래프 (unweighted graph)를 생성합니다.
All_Data_Info_L3 = All_Data_Info_L2 %>% 
  graph_from_data_frame()

All_Data_Info_L3 %>%
  ggraph(layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_with=n), edge_colour="cyan4") +
  geom_node_point(color="navyblue", size=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()



# 3) 가장 자주 사용되는 해시태그와 멘션 10개는 무엇인가요?
# 해시태그:
All_Data_Info_L4 %>% 
  tidyr::unnest() %>% 
  dplyr::select(keyword) %>% 
  dplyr::count(keyword, sort = TRUE) %>% 
  head(n = 10)

# A tibble: 10 x 2
# keyword          n
# <chr>        <int>
#   1 #coronavirus  6239
# 2 #covid19      1648
# 3 #china        1257
# 4 #ai            718
# 5 #iran          713
# 6 #trump         687
# 7 #wuhan         681
# 8 #health        672
# 9 #2019ncov      662
# 10 #1             661

# 멘션:
All_Data_Info_L5 %>% 
  tidyr::unnest() %>% 
  dplyr::select(keyword) %>% 
  dplyr::count(keyword, sort = TRUE) %>% 
  head(n = 10)

# # A tibble: 10 x 2
# keyword          n
# <chr>        <int>
#   1 #coronavirus  1126
# 2 #1             332
# 3 #yanggang      319
# 4 #wbz           308
# 5 #covid19       306
# 6 #iran          282
# 7 #ai            262
# 8 #trump         226
# 9 #hongkong      205
# 10 #china         201

# 4) a) 사용자가 데이터 세트가 포함된 기간에 동일한 사람을 언급한 경우 
# 연결되는 가중치가 없는 무방향 그래프를 만듭니다. 
# 자체 루프(self-loops) 를 포함하지 마십시오. 

# (3)에서 식별된 상위 10개 Twitter 핸들만 고려하십시오.
# - 이 그래프에는 몇 개의 노드와 에지가 있습니까?
# 해시태그를 기준으로 처리
All_Data_Info_L6 = All_Data_Info_L4 %>% 
  tidyr::unnest() %>% 
  # dplyr::count(keyword, sort = TRUE) %>% 
  dplyr::count(userID, keyword, sort = TRUE) %>%
  graph_from_data_frame()

# All_Data_Info_L6 %>%
#   ggraph(layout="fr") +
#   geom_edge_link(aes(edge_alpha=n, edge_with=n), edge_colour="cyan4") +
#   geom_node_point(color="navyblue", size=2) +
#   geom_node_text(aes(label=name), repel=TRUE, point.padding = unit(0.2, "lines")) +
#   theme_void()

summary(All_Data_Info_L6)
# 노드 : 67280
# 에지 : 127776

# b) 이 그래프에서 각 노드의 매개성, 근접성 및 고유중심성을 계산합니다.
# (참고: betweenness, the closeness, and the eigencentrality)
# - 이 세 가지 메트릭 각각에 따른 상위 3개 노드는 무엇입니까? 사용자 ID를 제시하십시오.
# - 상위 3개 노드에 따른 3개의 중심성 측정값의 차이가 있습니까? 그렇다면 왜 그런지 설명하십시오.

# betweenness : 상위 10개, 전체 평균 0.0
All_Data_Info_L6 %>% 
  igraph::betweenness() %>% 
  head(n = 10)

All_Data_Info_L6 %>% 
  igraph::betweenness() %>% 
  mean(na.rm = TRUE)

# closeness : 상위 10개, 전체 평균 0.31
All_Data_Info_L6 %>% 
  igraph::closeness() %>% 
  head(n = 10)

All_Data_Info_L6 %>% 
  igraph::closeness() %>% 
  mean(na.rm = TRUE)

# eigen_centrality : 상위 10개 고유벡터, 고유근 51.05
eigCenData = All_Data_Info_L6 %>% 
  igraph::eigen_centrality()

# 고유벡터
eigCenData$vector %>% 
  head(10)

# 고유근
eigCenData$value



# ******************************************************************
# 에러 발생
# ******************************************************************
# c) cluster_fast_greedy 함수를 사용하여 그래프를 분할합니다.
# - 클러스터는 몇 개입니까?
# - 각 클러스터의 크기는 얼마입니까?

All_Data_Info_L6 = All_Data_Info_L4 %>% 
  tidyr::unnest() %>%
  dplyr::count(userID, keyword, sort = TRUE) %>%
  graph_from_data_frame()

cluster_fast_greedy(All_Data_Info_L6)

# 
# 나)
# Rank	Betweenness	Closeness	Eigencentrality   (컬럼임)
# 1
# 2
# 3

# ==============================================================================
# 질문 8 (10pt) 간단한 선형회귀로 풀기
# ==============================================================================
# 개방형문제입니다. 제공된 데이터 세트를 사용하여 답변하고 싶은 연구 질문에 대해 생각해 보시기 바랍니다.
# 지금까지 CS564에서 학습한 도구를 사용하여 테스트할 수 있는 질문을 공식화하십시오.
# 답에는 중간고사 후에 배운 주제가 하나 이상 포함되어야 합니다.
# 전체 할당을 완료한 후 보유한 데이터세트를 사용합니다.

#) 연구 질문을 공식화하십시오.
# b) 이 질문에 답하는 데 유용한 도구는 무엇입니까? 선형회귀로 해보기
# 자주 사용하는 키워드
# 조건별 추출 (빈도수 2 초과, 2글자 초관)
# 해시태그 및 맨션 워드클라우드

# c) 결과는 어떻습니까? RMSE, R^2 Score
# d) 간단한 결과해석을 하기
# 해시태그의 경우 코로나 관련 키워드 (coronavirus, covid19, china, 2019ncov, coronavirusoutbreak) 외
# 인공지능 (ai) 및 정치 (iran, trump) 키워드가 주를 이루었다.
# 특히 코로나19 시대인 만큼 코로나에 대한 관심이 두드러지게 나타남 (전체 대비 90%)

# 멘션의 경우 앞선 해시태그와 유사하게 코로나 및 인공지능, 정치에 관심이 크긴하나 
# 예외적인 키워드 (yanggang, wbz)도 보인다.

# 가장 자주 사용하는 해시태그
Vis_Data_L4 = All_Data_Info_L4 %>% 
  tidyr::unnest() %>%
  dplyr::count(keyword, sort = TRUE) %>% 
  dplyr::mutate(
    len = stringr::str_length(keyword)
    ) %>% 
  filter(
    n > 2
    , len > 2
  )


fig = wordcloud2::wordcloud2(data = Vis_Data_L4)

# html 저장
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# html에서 png로 저장
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "워드클라우드_해시태그")
webshot::webshot("fig.html", saveImg, vwidth = 800, vheight = 600, delay = 10)


# 가장 자주 사용하는 맨션
Vis_Data_L5 = All_Data_Info_L5 %>% 
  tidyr::unnest() %>%
  dplyr::count(keyword, sort = TRUE) %>% 
  dplyr::mutate(
    len = stringr::str_length(keyword)
  ) %>% 
  filter(
    n > 2
    , len > 2
  )

fig = wordcloud2::wordcloud2(data = Vis_Data_L5)

# html 저장
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# html에서 png로 저장
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "워드클라우드_맨션")
webshot::webshot("fig.html", saveImg, vwidth = 800, vheight = 600, delay = 10)

