
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
# R을 이용한 bigram 텍스트 전처리 및 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0311"
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

#1-a
# gutenberg package의 책 번호 730. 단어 2개가 X,Y 연달아 나오기 때문에, 2개의 컬럼으로 나누기 (Word1, Word2 컬럼)
# Stop Word 모두 제거
# Bi-gram에서 가장 많이 등장하는 10개 단어 추출

# showtext::showtext_opts(dpi = 100)

All_Data <- gutenberg_download(11)

All_Data_New <- All_Data %>%
`  separate(text, c("Word1", "Word2"), sep=" ") %>%`
  filter(
    ! Word1 %in% stop_words$word
    , ! Word2 %in% stop_words$word
  )

# 가장 많이 등장하는 10개 단어 추출
All_Data_New %>% 
  dplyr::count(Word1, Word2, sort = TRUE) %>% 
  dplyr::filter(
    ! is.na(Word1)
    , ! is.na(Word2)
    , Word1 != ""
    , Word2 != ""
  ) %>% 
  slice_head(n=10)


#1-b
# Bi-gram에서 적어도 4번 이상 동시에 등장하는것을 시각화
# 엣지를 동시에 등장하는 단어로 설정

# Graph_BG
# All_Data_New

# ===================================================
# Word1에 대한 결과
# ===================================================
Filter_All_Data_New = All_Data %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 4) %>% 
  separate(bigram, c("Word1", "Word2"), sep = " ") %>%
  filter(
    ! Word1 %in% stop_words$word
    , ! Word2 %in% stop_words$word
  )

Filter_All_Data_New %>% 
  dplyr::count(Word1, Word2, sort = TRUE) %>% 
  dplyr::filter(
    ! is.na(Word1)
    , ! is.na(Word2)
    , Word1 != ""
    , Word2 != ""
  ) %>%
  graph_from_data_frame() %>%
  ggraph(layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_with=n), edge_colour="cyan4") +
  geom_node_point(color="navyblue", size=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding = unit(0.2, "lines")) +
  theme_void()


# 1-c: 
# Word1 칼럼에서 만약 "no", "not", "never", "neither"와 같은 부정동사가 나오면 Word2에서 제거하기
# joining과 counting을 사용한 후 ggplot을 사용하여 bigram에서 얼마나 자주 단어가 등장하나 확인해보기.
# (mutate, group_by, ggplot, geom_bar, facet_wrap 등을 사용해보기)

# 감정 분석을 위한 데이터
AFINN = tidytext::get_sentiments("afinn")
AFINN

# 데이터 전처리
Filter_All_Data_New2 = All_Data %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 4) %>% 
  separate(bigram, c("Word1", "Word2"), sep = " ") %>% 
  dplyr::filter(
    ! is.na(Word1)
    , ! is.na(Word2)
    , Word1 != ""
    , Word2 != ""
    , Word1 %in% c("no", "not", "never", "neither")
  ) 
  

# 감정분석 및 상위20개 막대그래프 시각화
Filter_All_Data_New2 %>% 
  inner_join(AFINN, by = c("Word2" = "word")) %>%
  count(Word1, Word2, value, sort = TRUE) %>% 
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(Word2 = reorder(Word2, contribution)) %>%
  ggplot(aes(n * value, Word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Word2 List") +
  facet_wrap(~Word1, scales = "free_y")
  