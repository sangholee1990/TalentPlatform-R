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
# R을 이용한 비지도 학습 (주성분 분석, 군집 분석, Moran EigenVector Spatial Filtering)

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0232"
contextPath = ifelse(env == "local", ".", getwd())

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
# 라이브러리 읽기
library(tidyverse)
library(readr)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(ggcorrplot)
library(GGally)
library(factoextra)

# 파일 찾기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Seoul_grid.shp"))

# shp 파일 읽기
mapData = sf::st_read(fileInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(CRS("+proj=longlat"))

mapDataL1 = mapData %>% 
  as.tibble() %>% 
  dplyr::select(-c("grid_id", "geometry"))

# ******************************************************************************
# 1. 모든 변수 간의 상관관계 행렬(correlation matrix)를 계산하시오.
# 특히 높은 상관관계를 보이는 변수쌍(절댓값 > 0.3)을 굵게 표시하고, 
# 해당 변수들 간의 관계를 간략히 설명하시오. 
# ******************************************************************************
# 상관관계 행렬에서 아파트 단위면적당 매매가를 기준으로
# 음의 관계 (지하철역 접근성, 노후 연수, 대학 진학률)을 보인 반면
# 특목/자립고 진학 비율에서는 양의 관계를 보인다.
# 특히 주요 편의 시설 관련 변수 (병원-공원-문화시설-공원시설 접근성)들은 서로 간의 상관성이 높음을 수 확인할 수 있다.

# 상관계수 (1)
corRes = cor(mapDataL1)
pvalRes = ggcorrplot::cor_pmat(mapDataL1)    
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "상관계수 행렬 (1)")

ggcorrplot::ggcorrplot(
  corRes
  , outline.col = "white"
  , lab = TRUE
  , p.mat = pvalRes
  , sig.level = 0.05
  , colors = c("#6D9EC1", "white", "#E46726")
) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# 상관계수 (2)
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "상관계수 행렬 (2)")

GGally::ggcorr(corRes, geom = "blank", label = TRUE, hjust = 0.75, label_size = 5) +
  geom_point(size = 14, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.3)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ******************************************************************************
# 2. 주성분분석을 실행하시오. 전체 변수의 분산을 적절히 요약하기 위해 
# 적합한 수의 주성분을 선택하고, 선정 이유를 설명하시오. 
# 더불어 각 주성분이 전체 분산에서 설명하는 비율을 나타내시오.
# ******************************************************************************
# 변수 표준화 과정없이 주성분분석을 수행한 결과 제1성분 및 제2성분만으로도 충분히 설명 가능할 것으로 판단된다.
# 즉 1-2 성분에 대한 누적 분산은 0.9978로서 전체 분산의 99.78%를 설명할 수 임을 알 수 있다.

# 표준화 O
mapDataL1Scale = scale(mapDataL1)

# 표준화 X + 상관계수 X
# pcaRes = princomp(mapDataL1, cor = FALSE)

# 표준화 O + 상관계수 O
pcaRes = princomp(mapDataL1Scale, cor = TRUE)

summary(pcaRes)

pcaResVar = factoextra::get_eig(pcaRes)$variance.percent
pcaResCumVar = factoextra::get_eig(pcaRes)$cumulative.variance.percent
cnt = nrow(factoextra::get_eig(pcaRes))

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주성분에 따른 설명력")

factoextra::fviz_screeplot(pcaRes, addlabels = TRUE, title=  NULL, x="Principal  Components", y="Percentage  of  Variance  [%]", ncp = cnt) +
  geom_point(aes(y=pcaResCumVar), col='red') + 
  geom_line(aes(y=pcaResCumVar), col='red') +
  theme_bw(base_size = 18) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# ******************************************************************************
# 3. 성분 부하량(loading)을 이용하여 선택된 주성분과 변수 간의 관계를 설명하시오. 
# ******************************************************************************
pcaResLoading = pcaRes$loadings

# 각 성분에 따라 주요 영향을 미치는 변수 특성을 파악할 수 있다.
# 즉 제1성분의 경우 아파트 단위면적당 매매가 영향이 큼을 확인할 수 있고
# 또한 제2성분에서는 아파트 매매건수가 주요 영향임을 알수 있다.
# 이는 앞서 언급한 바와 같이 데이터 표준화를 처리하지 않아서 
# 데이터 큰 범위에 주성분분석에 영향을 끼치는 것으로 파악된다.


# ******************************************************************************
# 4. 두 번째 주성분의 성분 점수(scores)를 지도화하고, 그 패턴을 앞의 
# 성분 부하량과 연관지어 해석하시오. 
# Hint: 지도화를 위하여 MESF.R에 있는 mapping.seq함수를 이용할 수 있다
# (필요 패키지: library(RColorBrewer); library(classInt)).
# ******************************************************************************

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주성분에 따른 분산 기여도")

# Contributions of variables to PC1
fviz_contrib(pcaRes, choice = "var", axes = 2, top = 10) +
  theme_bw(base_size = 18) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주성분에 따른 성분부하량")

fviz_pca_biplot(pcaRes, repel = TRUE, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_bw(base_size = 18) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주성분에 따른 성분부하량 (2)")

fviz_pca_ind(pcaRes, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
) +
  theme_bw(base_size = 18) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)






# EOF = princomp(mapDataL1, cor = TRUE)
EOF
## scale = FALSE , center = FALSE : 공분산 행렬   (아노말리 미고려)
## scale = FALSE , center = TRUR  : 공분산 행렬   (아노말리 고려)
## scale = TRUE  , center = FALSE : 상관계수 행렬 (아노말리 미고려)
## scale = TRUE  , center = TRUE  : 상관계수 행렬 (아노말리 고려)
names(EOF)
Eigen_value = EOF$sdev^2      ;  Eigen_value         # 고유근
Eigen_vector = EOF$loadings   ;  Eigen_vector        # 고유벡터
EOF_data = data.frame(EOF$scores)  ;  EOF_data            # Principal Components
summary(EOF)

screeplot(EOF, type = "l")   # 떨어지는 각도가 완만해지는 2까지 주성분으로 선택.

## Eigen_value Test
sum(diag(COV))        # 공분산의 대각선 합
sum(Eigen_value)      # 고유근의 합
PC_var = get_eig(EOF)$variance.percent
pcaResCumVar = get_eig(EOF)$cumulative.variance.percent

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주성분에 따른 설명력")
# 
# factoextra::fviz_screeplot(EOF, addlabels = TRUE, title=  NULL, x="Principal  Components", y="Percentage  of  Variance  [%]", ncp = nrow(get_eig(EOF))) +
#   geom_point(aes(y=pcaResCumVar), col='red') + 
#   geom_line(aes(y=pcaResCumVar), col='red') +
#   theme_bw(base_size = 18) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# 성분 부하량
Eigen_vector

# Eigenvalues
eig.val <- factoextra::get_eigenvalue(EOF)
eig.val

# Results for Variables
res.var <- factoextra::get_pca_var(EOF)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
res.var$cor

# Results for individuals
res.ind <- factoextra::get_pca_ind(EOF)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# fviz_ca_row(EOF, repel = TRUE)

# Color by cos2 values: quality on the factor map
# fviz_ca_row(EOF, col.row = "cos2",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE)

# 분산 기여도
fviz_pca_var(EOF, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(EOF, choice = "var", axes = 2, top = 10)




# fviz_pca_biplot(EOF, repel = TRUE)

# fviz_pca_ind(EOF, repel = TRUE)

# res.ca <- CA(housetasks, graph = FALSE)
# fviz_pca_ind(EOF,
#              label = "none", # hide individual labels
#              # habillage = iris$Species, # color by groups
#              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#              addEllipses = TRUE # Concentration ellipses
# )

# biplot(EOF,cex=0.8)

library(classInt)  


coords <- as.matrix(cbind(SPDF$x, SPDF$y))
scaled.spdf <- SpatialPointsDataFrame(coords, as.data.frame(data.scaled ))

bw.gw.pca <- bw.gwpca(scaled.spdf, 
                      vars = colnames(scaled.spdf@data),
                      k = 5,
                      robust = FALSE, 
                      adaptive = TRUE)





#=================================================
# Part 2: Cluster analysis
#=================================================

mapDataL2 = mapDataL1 %>% 
  dplyr::select(-grid_id, -geometry)

# 중심 위/경도 반환
posData = sf::st_centroid(mapDataL1$geometry) %>% 
  sf::st_coordinates() %>% 
  as.tibble() %>% 
  dplyr::rename(
    "lon" = "X"
    , "lat" = "Y"
  )

# ****************************************************************
# kmeans 클러스터링 (데이터 표준화 X)
# ****************************************************************
mapDataL3 = dplyr::bind_cols(posData, mapDataL2)

kcluModel = mapDataL3 %>% 
  purrr::keep(is.numeric) %>% 
  kmeans(centers = 8, iter.max = 10, nstart = 5)

# 원시 데이터+ 클러스터링 결과
pointAssignments = broom::augment(kcluModel, mapDataL3)
pointAssignments

# 클러스터링 결과
clusterInfo = broom::tidy(kcluModel)
clusterInfo

# 클러스터링 통계 결과 (amap::Kmean 라이브러리 이용 시 불가)
modelStats = broom::glance(kcluModel)
modelStats

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Kmeans-Cluster")

ggplot() +
  geom_sf(data = mapData, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "grey50", fill = NA) +
  geom_point(data = pointAssignments, aes(x = lon , y = lat, color = .cluster), shape = 15, size=5, alpha = 0.5) +
  # geom_point(data = pointAssignments, aes(x = lon , y = lat, color = .cluster), shape = 16, size=5, alpha = 0.5) +
  geom_label(data = clusterInfo, aes(x = lon , y = lat, label = cluster, fill = factor(cluster)), size = 8, colour = "white", fontface = "bold", show.legend = FALSE) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
    , axis.line = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
    , plot.margin = unit(c(0, 0, 0, 0), 'lines')
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)

# ****************************************************************
# kmeans 클러스터링 (데이터 표준화 O)
# ****************************************************************
mapDataL3 = dplyr::bind_cols(posData, mapDataL2) %>% 
  dplyr::mutate_each(
    funs(scale)
    , vars = c(colnames(mapDataL2))
  )

kcluModel = mapDataL3 %>% 
  purrr::keep(is.numeric) %>% 
  kmeans(centers = 8, iter.max = 10, nstart = 5)


# 원시 데이터+ 클러스터링 결과
pointAssignments = broom::augment(kcluModel, mapDataL3) %>% 
  dplyr::mutate_each_(
    funs(grt::unscale)
    , vars = colnames(mapDataL3)
  )
pointAssignments

# 클러스터링 결과
clusterInfo = broom::tidy(kcluModel) %>% 
  dplyr::mutate_each_(
    funs(grt::unscale)
    , vars = colnames(mapDataL3)
  )
clusterInfo

# 클러스터링 통계 결과 (amap::Kmean 라이브러리 이용 시 불가)
modelStats = broom::glance(kcluModel)
modelStats

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Kmeans-Cluster-Nomal")

ggplot() +
  geom_sf(data = mapData, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "grey50", fill = NA) +
  geom_point(data = pointAssignments, aes(x = lon , y = lat, color = .cluster), shape = 15, size=5, alpha = 0.5) +
  # geom_point(data = pointAssignments, aes(x = lon , y = lat, color = .cluster), shape = 16, size=5, alpha = 0.5) +
  geom_label(data = clusterInfo, aes(x = lon , y = lat, label = cluster, fill = factor(cluster)), size = 8, colour = "white", fontface = "bold", show.legend = FALSE) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
    , axis.line = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
    , plot.margin = unit(c(0, 0, 0, 0), 'lines')
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


# ****************************************************************
# kmeans 다중 클러스터링 (데이터 표준화 O)
# ****************************************************************
kcluModelList = dplyr::tibble(nClu = 1:12) %>%
  dplyr::mutate(
    kcluModel = purrr::map(
      nClu
      , ~ kmeans(mapDataL3, centers = .x)
    )
    , augmented = purrr::map(kcluModel, broom::augment, mapDataL3)
    , tidied = purrr::map(kcluModel, broom::tidy)
    # 클러스터링 통계 결과 (amap::Kmean 라이브러리 이용 시 불가)
    , tot.withinss = purrr::map(kcluModel, ~ sum(.x$withinss, na.rm = TRUE))
  ) 

modelStats = kcluModelList %>%
  dplyr::select(nClu, tot.withinss) %>%
  tidyr::unnest(tot.withinss)

# ****************************************************************
# 클러스터링 오차 시각화
# ****************************************************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Kmeans-Cluster-ElbowChart")

ggplot(data = modelStats, aes(nClu, tot.withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, 1)) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


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
# R을 활용한 유튜브 크롤링 결과 기반으로 명사 추출 및 워드 클라우드

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0239"
contextPath = ifelse(env == "local", ".", getwd())

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
# 라이브러리 읽기
library(tidyverse)
library(readr)
library(RColorBrewer)
library(tidyverse)
library(readr)
library(stringr)
library(wordcloud2)
library(RcppMeCab)
library(RmecabKo)

# 명사 추출을 위한 메타 정보
RmecabKo::install_mecab("c:/mecab")

# *************************************************************
# 유튜브 댓글 수집
# *************************************************************
# install.packages("multilinguer")
# 
# library(multilinguer)
# 
# install_jdk()
# 
# install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
# 
# install.packages("remotes")
# 
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# 
# #cmd열기
# 
# #C:\r-selenium
# #java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445
# 
# install.packages("httr")
# install.packages("RSelenium")
# install.packages("rvest")
# 
# library(httr)
# library(RSelenium)
# library(rvest)
# 
# remD<- remoteDriver(remoteServerAddr = 'localhost',
#                     port = 4445L, #포트번호 입력
#                     browserName = "chrome")
# 
# remD$open() #빈페이지 열기 
# remD$navigate("https://www.youtube.com/watch?v=CitIMlaa8To&list=PL4Lb93R1nd7ctttoHCmCXxEZcGy_KUYpY&index=4") #홈페이지 이동
# 
# btn <- remD$findElement(using = "css selector",
#                         value = ".html5-main-video")
# btn$clickElement() #동영상 일시정지 
# 
# remD$executeScript("window.scrollTo(0,5000)")
# remD$executeScript("window.scrollTo(5000,10000)")
# remD$executeScript("window.scrollTo(10000,50000)")
# remD$executeScript("window.scrollTo(50000,100000)")
# remD$executeScript("window.scrollTo(100000,200000)")
# remD$executeScript("window.scrollTo(200000,300000)")
# remD$executeScript("window.scrollTo(300000,400000)")
# remD$executeScript("window.scrollTo(400000,500000)")
# remD$executeScript("window.scrollTo(500000,600000)")
# remD$executeScript("window.scrollTo(600000,700000)")
# remD$executeScript("window.scrollTo(700000,900000)")
# remD$executeScript("window.scrollTo(900000,1000000)")
# remD$executeScript("window.scrollTo(1000000,1300000)")
# remD$executeScript("window.scrollTo(1300000,1600000)")
# remD$executeScript("window.scrollTo(1600000,1900000)")
# remD$executeScript("window.scrollTo(1900000,2300000)")
# remD$executeScript("window.scrollTo(2300000,2600000)")
# remD$executeScript("window.scrollTo(2600000,2900000)")
# remD$executeScript("window.scrollTo(2900000,3300000)")
# remD$executeScript("window.scrollTo(3300000,3600000)")
# remD$executeScript("window.scrollTo(3600000,3900000)")
# remD$executeScript("window.scrollTo(3900000,4300000)")
# remD$executeScript("window.scrollTo(4300000,4600000)")
# remD$executeScript("window.scrollTo(4600000,4900000)")
# remD$executeScript("window.scrollTo(4900000,5300000)")
# remD$executeScript("window.scrollTo(5300000,5600000)")
# remD$executeScript("window.scrollTo(5600000,5900000)")
# remD$executeScript("window.scrollTo(6300000,6600000)")
# remD$executeScript("window.scrollTo(6600000,6900000)")
# remD$executeScript("window.scrollTo(6900000,7300000)")
# remD$executeScript("window.scrollTo(7300000,7600000)")
# remD$executeScript("window.scrollTo(7600000,7900000)")
# remD$executeScript("window.scrollTo(7900000,8300000)")
# remD$executeScript("window.scrollTo(8300000,8600000)")
# remD$executeScript("window.scrollTo(8600000,8900000)")
# remD$executeScript("window.scrollTo(8900000,9300000)")
# remD$executeScript("window.scrollTo(9300000,9600000)")
# remD$executeScript("window.scrollTo(9600000,9900000)")
# remD$executeScript("window.scrollTo(9900000,10300000)")
# remD$executeScript("window.scrollTo(10300000,10600000)")
# remD$executeScript("window.scrollTo(10600000,10900000)")
# remD$executeScript("window.scrollTo(10900000,11300000)")
# remD$executeScript("window.scrollTo(11300000,11600000)")
# 
# 
# html <- remD$getPageSource()[[1]]
# html <- read_html(html)  # 페이지 소스 읽어오기
# ytube_comments <- html %>% html_nodes("#content-text") %>% html_text()
# #SelectorGadget 설치. 댓글의 CSS 찾기
# 
# head(ytube_comments)
# 
# ytube_comments <- gsub("\n", "", ytube_comments)
# ytube_comments <- trimws(ytube_comments)
# 
# ytube_comments
# # / 와 \ 헷갈리지 않기 
# write.table(ytube_comments,
#             file = "C:/r-selenium/결과물/youtube_comments1.txt",
#             sep=".",
#             row.names=FALSE,
#             quote = FALSE)


# *************************************************************
# 명사 추출 및 시각화 (1) : 신규
# *************************************************************
# 명사 추출
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0239_youtube_comments1.txt"))
data = readr::read_delim(fileInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"))

dataL1 = data.frame()
for (i in 1:nrow(data)) {
  tmpData = RcppMeCab::pos(utf8::as_utf8(data$x[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)
  
  dataL1 = dplyr::bind_rows(tmpData, dataL1)
}

# 키워드 빈도에 따른 시각화
# 빈도수 2 이상 및 2글자 이상
keywordData = dataL1 %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::mutate(len = stringr::str_length(token)) %>% 
  dplyr::filter(
    freq >= 2
    , len >= 2
  ) %>% 
  dplyr::arrange(desc(freq))

fig = wordcloud2::wordcloud2(data = keywordData)

# html 저장
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# html에서 png로 저장
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "워드클라우드")
webshot::webshot("fig.html", saveImg, vwidth = 800, vheight = 600, delay = 10)


# *************************************************************
# 명사 추출 및 시각화 (2) : 기존
# *************************************************************

#=====================================================
# KoNLP 라이브러리 (국문) 설치
#=====================================================
# 1. R Stduido 작업환경 설정
# - 바탕화면에서 Test 작업환경 구축
#
# 2. Java 설치 확인
# - cmd 창에서 java 버전 확인 (java -version)
#
# 3. Java 환경변수 확인
# - 현재 미 설정
# - 일반적으로 KoNLP 라이브러리 경우 java 1.8에서 수행 가능
# - 따라서 Java 다운로드 수행
#
# 4. Java 1.8 다운로드
# - https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html
# - jdk-8u261-windows-x64.exe
# - Open JRE가 아니라 Oracle Java 사용 권장
# - 관리자 권한으로 실행
# - JRE 및 JDK를 순차적으로 설치
# - cmd 창에서 java 버전 확인 (java -version)
# C:\Users\inu>java -version
# java version "1.8.0_261"
# Java(TM) SE Runtime Environment (build 1.8.0_261-b12)
# Java HotSpot(TM) 64-Bit Server VM (build 25.261-b12, mixed mode)

# 5. Java 환경변수 설정
# - JAVA_HOME : C:\Program Files\Java\jdk1.8.0_261
# - PATH : C:\Program Files\Java\jdk1.8.0_261\bin
# - cmd창에서 환경변수 확인 (echo %JAVA_HOME%)

# 6. Github에서 설치할 수 있는 라이브러리 설치 및 읽기
# - # install.packages("remotes")
# - library(remotes)

# 7. 의존성 라이브러리 설치
# - install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
# - # install.packages("remotes")
# - library(remotes)
# - library(devtools)
# - # install.packages("rmarkdown")
# - library(rmarkdown)
# - # install.packages("RSQLite")
# - library(RSQLite)

# 8. rJava 읽기
# - library(rJava)

# 9. KoNLP 설치 및 읽기
# - # remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# - library(KoNLP)

# 10. useNIADic() 읽기
# - # devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE, force = TRUE)
# - useNIADic()

# 11. 설치 완료

#=====================================================
# KoNLP 라이브러리 (국문) 설치
#=====================================================
# 7-11번 에러 발생 시 조치 방법
# 1) 7-10행 1회 수행
# 2) Rstudio 종료 후 Test.Rproj 재 실행
# 3) 7-11행까지 정상적으로 수행

# install.packages ( "tm") # 텍스트 처리를 할 수있게 도와주는 패키지
# install.packages ( "stringr") # 문자열 처리를 할 수있게 도와주는 패키지
# install.packages ( "wordcloud") # 문자들을 제공 할 수있게 도와주는 패키지
# install.packages ( "NLP") # 영어 텍스트에 특화된 패키지
# install.packages("KoNLP") # 한글 텍스트에  특화된 패키지
# install.packages ( "RColorBrewer") # 색상을 다양하게 표현할 수있게 도와주는 패키지
# install.packages("sp")
# # 
library(sp)
library(stringr)
library(wordcloud)
library(tm)
library(KoNLP)
library(tm)
# 
# #옆에 페키지 KoLNP 체크하기
# 
# getwd()
# 
# setwd("C:/r-selenium/결과물") #경로가 맞지 않을 경우 변경

# readLines("youtube_comments1.txt") #텍스트문서를 읽는 함수
# 
# myEtext<-readLines("youtube_comments1.txt")
# 
# head(myEtext) #저장된거 확인 
# 
# paste(readLines("youtube_comments1.txt"),collapse = " ")#7개의 라인을 1개로 만듦
# 
# myEtext2<-paste(readLines("youtube_comments1.txt"),collapse = " ")

fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0239_youtube_comments1.txt"))
data = readr::read_delim(fileInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"))


myEtext2 = paste(data$x, collapse = " ")

myEtext3<-gsub(pattern = "\n", replace=" ",myEtext2) #gsub(찾을것, 바꿀것, 해당열), \W은 영어구두점을 의미하는 것임

# myEtext3

# tolower(myEtext3) #모두 소문자로 처리

myEtext4<-tolower(myEtext3)

# myEtext4


# stopwords() #텍스트내의 불용어
# removeWords(myEtext4,stopwords())
myEtext5<-removeWords(myEtext4, tm::stopwords()) #불용어 삭제

# stripWhitespace(myEtext5)
myEtext6<-stripWhitespace(myEtext5) #쓸데 없는 공란 제거
# myEtext6

myEtext6 <- gsub("\\^", "", myEtext6)
myEtext6 <- gsub("ㅎ", "", myEtext6)
myEtext6 <- gsub("ㅋ", "", myEtext6)
myEtext6 <- gsub("ㅠ", "", myEtext6)
myEtext6 <- gsub("ㅜ", "", myEtext6)
myEtext6 <- gsub("진짜", "", myEtext6)
myEtext6 <- gsub("너무", "", myEtext6)
myEtext6 <- gsub("구독", "", myEtext6)
myEtext6 <- gsub("님", "", myEtext6)
myEtext6 <- gsub("언니", "", myEtext6)
myEtext6 <- gsub("누나", "", myEtext6)
myEtext6 <- gsub("영상", "", myEtext6)
myEtext6 <- gsub("여", "", myEtext6)
myEtext6 <- gsub("요", "", myEtext6)
myEtext6 <- gsub("저", "", myEtext6)
myEtext6 <- gsub("것", "", myEtext6)
myEtext6 <- gsub("1", "", myEtext6)
myEtext6 <- gsub("2", "", myEtext6)
myEtext6 <- gsub("3", "", myEtext6)
myEtext6 <- gsub("4", "", myEtext6)
myEtext6 <- gsub("5", "", myEtext6)
myEtext6 <- gsub("6", "", myEtext6)
myEtext6 <- gsub("7", "", myEtext6)
myEtext6 <- gsub("8", "", myEtext6)
myEtext6 <- gsub("9", "", myEtext6)
myEtext6 <- gsub("0", "", myEtext6)
myEtext6 <- gsub("한", "", myEtext6)
myEtext6 <- gsub("이", "", myEtext6)
myEtext6 <- gsub("스", "", myEtext6)
myEtext6 <- gsub("개", "", myEtext6)
myEtext6 <- gsub("분", "", myEtext6)
myEtext6 <- gsub("부", "", myEtext6)
myEtext6 <- gsub("뭐", "", myEtext6)
myEtext6 <- gsub("분", "", myEtext6)
myEtext6 <- gsub("세", "", myEtext6)
myEtext6 <- gsub("뭐", "", myEtext6)
myEtext6 <- gsub("수", "", myEtext6)
myEtext6 <- gsub("번", "", myEtext6)
myEtext6 <- gsub("들", "", myEtext6)
myEtext6 <- gsub("것", "", myEtext6)
myEtext6 <- gsub("렇게", "", myEtext6)
myEtext6 <- gsub("적", "", myEtext6)
myEtext6 <- gsub("데", "", myEtext6)
myEtext6 <- gsub("국", "", myEtext6)
myEtext6 <- gsub("기", "", myEtext6)
myEtext6 <- gsub("진", "", myEtext6)
myEtext6 <- gsub("제", "", myEtext6)
myEtext6 <- gsub("라", "", myEtext6)
myEtext6 <- gsub("을", "", myEtext6)
myEtext6 <- gsub("미", "", myEtext6)
myEtext6 <- gsub("전", "", myEtext6)
myEtext6 <- gsub("보", "", myEtext6)
myEtext6 <- gsub("때", "", myEtext6)
myEtext6 <- gsub("도", "", myEtext6)
myEtext6 <- gsub("듯", "", myEtext6)
myEtext6 <- gsub("은", "", myEtext6)
myEtext6 <- gsub("저", "", myEtext6)
myEtext6 <- gsub("지", "", myEtext6)
myEtext6 <- gsub("나", "", myEtext6)
myEtext6 <- gsub("많", "", myEtext6)
myEtext6 <- gsub("같", "", myEtext6)
myEtext6 <- gsub("의", "", myEtext6)
myEtext6 <- gsub("일", "", myEtext6)
myEtext6 <- gsub("u", "", myEtext6)
myEtext6 <- gsub("f", "", myEtext6)
myEtext6 <- gsub("d", "", myEtext6)
myEtext6 <- gsub("\\+", "", myEtext6)
myEtext6 <- gsub("\\<", "", myEtext6)
myEtext6 <- gsub("\\>", "", myEtext6)


myEtext7 <- extractNoun(myEtext6)
myEtext7

# 텍스트 추출
sent = myEtext7 %>%
  str_replace_all("[0-9]+", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("http[a-zA-Z0-9]+", "") %>%
  str_replace_all('\n', ' ') %>%
  str_replace_all('\t', ' ') %>%
  str_replace_all("[[:punct:]]", '') %>%
  str_replace_all("[\\$,]", '') %>%
  str_replace_all('<b>', '') %>%
  str_replace_all('</b>', '') %>%
  str_replace_all('\\(', '') %>%
  str_replace_all('\\)', '') %>%
  str_replace_all('\\)', '') %>%
  str_replace_all('shuttlekakaomobilitycomshuttles', '') %>%
  str_replace_all('zip', '')

# 별도로 처리
# myEtext8 <- myEtext7[nchar(myEtext7)<5]
# myEtext8 <- myEtext8[nchar(myEtext8)>1]
# myEtext8 <- sort(table(myEtext7), decreasing = T)
# myEtext8
# myEtext9 <- head(myEtext8, 300)
# myEtextbag<-unlist(myEtext8) #리스트를 벡터로 바꿔주는 함수
# class(myEtextbag)
# myEtextbag
# str(myEtextbag)

# install.packages("wordcloud2")
library(wordcloud2)

# 텍스트 분석을 위한 말뭉치 (Corpus) 생성
text = tm::Corpus(VectorSource(sent)) #

# TermDocumentMatrix (용어-문서 행렬) 생성
tdm = tm::TermDocumentMatrix(text)

# 키워드별 등장 빈도
data = data.frame(
  word = rownames(as.matrix(tdm))
  , freq = rowSums(as.matrix(tdm))
)

# 상위 100개
keywordData = data %>%
  dplyr::mutate(len = stringr::str_length(word)) %>%
  dplyr::filter(
    dplyr::between(len, 2, 4)
  ) %>% 
  dplyr::arrange(desc(freq)) %>%
  dplyr::top_n(100)
  
# 워드 클라우드
fig = wordcloud2::wordcloud2(data = keywordData)

# html 저장
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# html에서 png로 저장
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "워드클라우드_KoNLP")
webshot::webshot("fig.html", saveImg, vwidth = 800, vheight = 600, delay = 10)

