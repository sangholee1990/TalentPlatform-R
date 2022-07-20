#1-a
# gutenberg package의 책 번호 730. 단어 2개가 X,Y 연달아 나오기 때문에, 2개의 컬럼으로 나누기 
# (Word1, Word2 컬럼)
# Stop Word 모두 제거
# Bi-gram에서 가장 많이 등장하는 10개 단어 추출

library(gutenbergr)
library(dplyr)
library(tidytext)

All_Data <- gutenberg_download(11)
library(tidyr)
All_Data_New <- All_Data %>%
  dplyr::rename(word = text) %>% 
	separate(word, c("Word1", "Word2"), sep=" ")
head(All_Data_New)

Filter_All_Data_New <- All_Data_New %>%
	filter(!Word1 %in% stop_words$word) %>%
	filter(!Word2 %in% stop_words$word)

Filter_All_Data_New %>% slice_head(n=10)


#1-b
# Bi-gram에서 적어도 4번 이상 동시에 등장하는것을 시각화
# 엣지를 동시에 등장하는 단어로 설정

library(igraph)
library(ggraph)

# Graph_BG
Graph_BG = Filter_All_Data_New

Graph_BG %>%
	# filter (n>4) %>%
	graph_from_data_frame() %>%
	ggraph(layout="fr") +
	geom_edge_link(aes(edge_alpha=n, edge_with=n), edge_colour="cyan4")+
	geom_node_point(color="navyblue", size=2) +
	geom_node_text(aes(label=name), repel=TRUE,
		point.padding = unit(0.2, "lines")) +
theme_void()

# Graph_BGC

# 1-c: 
# Word1 칼럼에서 만약 "no", "not", "never", "neither"와 같은 부정동사가 나오면 Word2에서 제거하기.
# joining과 counting을 사용한 후 ggplot을 사용하여 bigram에서 얼마나 자주 단어가 등장하나 확인해보기.
# (mutate, group_by, ggplot, geom_bar, facet_wrap 등을 사용해보기)


Bi_Data <- All_Data_New$Word1 %>%
	unnest_tokens(word, text, token="ngram", n=2)
Pos_Neg <- get_sentiments("no", "not", "never", "neither"), sep = " ") %>%
	inner_join(Pos_Neg, by= c(word1="word")) %>%
	group_by(no, not, never, neither) %>%
	tally(sort=TRUE)


Ranked_Words <- senti %>%
	group_by(sentiment) %>%
	mutate(rank = order(n, decreasing=TRUE)) %>%
	unite("words", word1: word3, sep=" ", na.rm=TRUE)
Ranked_Words

library(forcats)
Top_Words <- Ranked_Words[Ranked_Words$rank <= 5,] %>%
	arrange(rank) %>%
	mutate(words=fct_reorder(words, rank))
Top_Words

ggplot(Top_Words)
