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
# R을 이용한 선거 데이터 (서울특별시 강서구, 충청남도 아산시) 3단계 시각화 및 도표 삽입

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0214"
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
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(openxlsx)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(ggmap)
library(ggcharts)
library(scales)
library(raster)
library(cowplot)
library(patchwork)
library(scatterpie)

# 선거 데이터 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0214_선거분석(강서병).xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 3)

dataL1 = data %>%
  as.tibble() %>%
  na.omit() %>%
  readr::type_convert()

dataL2 = dataL1 %>% 
  tidyr::gather(-c(투표구, 종류), key = "key", value = "val") %>% 
  dplyr::group_by(투표구, key) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(key = "key", value = "meanVal")

dataL3 = dataL2 %>% 
  rowwise(투표구) %>% 
  dplyr::mutate(
    sumVal = sum(더불어민주당, 자유한국당, 기타야당, na.rm = TRUE)
    , maxVal = max(더불어민주당, 자유한국당, 기타야당, na.rm = TRUE)
    , meanVal = (더불어민주당 / sumVal) * 100
    , meanVal2 = (자유한국당 / sumVal) * 100
    , meanVal3 = (기타야당 / sumVal) * 100
    , val = dplyr::case_when(
      자유한국당 == maxVal ~ 1
      , 더불어민주당 == maxVal ~ 2
      , 기타야당 == maxVal ~ 3
    )
  )

dataL4 = dataL3 %>%
  dplyr::select(-c(더불어민주당, 자유한국당, 기타야당, sumVal, val, maxVal)) %>%
  # dplyr::select(-c(meanVal, meanVal2, meanVal3, sumVal, val, maxVal)) %>%
  dplyr::rename(
    더불어민주당 = meanVal
    , 자유한국당 = meanVal2
    , 기타야당 = meanVal3
  ) %>% 
  tidyr::gather(-c(투표구), key = "key", value = "val")

# 정당에 따른 정렬
dataL4$key = forcats::fct_relevel(dataL4$key, c("자유한국당", "더불어민주당", "기타야당"))

selData = dataL1 %>% dplyr::filter(종류 == "광역단체장")
dataL4$투표구 = forcats::fct_relevel(dataL4$투표구, rev(selData$투표구))

# ************************************************
# 선거 빈도분포
# ************************************************
saveImg1 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "서울특별시_강서구_선거_빈도분포")

ggplot(dataL4, aes(x = 투표구, y = val, fill = key, group = key, label = round(val, 0))) +
  # geom_bar(position = "dodge", stat = "identity") +
  geom_bar(position = position_stack(), stat = "identity") +
  # geom_text(size = 5, vjust = 1.6, hjust = 0.5, color = "white") +
  geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  labs(x = "투표구", y = "비율", fill = NULL, subtitle = "서울특별시 강서구 선거 빈도분포") +
  # scale_x_continuous(breaks = seq(1, 11, 1)) +
  theme(
    text = element_text(size = 16)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  # facet_wrap(~종류, scale = "free", ncol = 3) +
  ggsave(filename = saveImg1, width = 12, height = 10, dpi = 600)


# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))
# mapInfo = Sys.glob(file.path(globalVar$mapPath, "LSMD_ADM_SECT_UMD_서울/LSMD_ADM_SECT_UMD_11.shp"))
# mapInfo = Sys.glob(file.path(globalVar$mapPath, "LSMD_CONT_LDREG_서울_강서구/LSMD_CONT_LDREG_11500.shp"))
# mapInfo = Sys.glob(file.path(globalVar$mapPath, "읍면동/EMD_202101/TL_SCCO_EMD.shp"))

# shp 파일 읽기 (1)
# mapData = raster::shapefile(mapInfo, encoding = "UTF-8")
# geoData = sp::spTransform(mapData, CRS("+proj=longlat"))
# mapGeoData = ggplot2::fortify(geoData, region = "EMD_CD", region2 = "EMD_KOR_NM")

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(CRS("+proj=longlat"))

st_crs(mapGlobal)

# 법정동 코드 읽기 (1)
# codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/법정동코드_전체자료.txt"))
# codeData = utils::read.table(codeInfo, sep = "\t", header = TRUE, colClasses = "character", fileEncoding = "EUC-KR") %>%
#   as.tibble() %>%
#   magrittr::set_colnames(c("EMD_CD", "addr", "isUse"))

# codeDataL1 = codeData %>%
#   tidyr::separate(col = "addr", into = c("addr1", "addr2", "addr3", "addr4", "addr5"), sep = " ") %>%
#   # dplyr::select(-addr4, -addr5) %>%
#   dplyr::filter(
#     stringr::str_detect(addr1, regex("서울특별시"))
#     , stringr::str_detect(addr2, regex("강서구"))
#     # , isUse == "존재"
#   ) %>%
#   # dplyr::filter(
#   #   !is.na(addr1)
#   #   , !is.na(addr2)
#   #   , !is.na(addr3)
#   # ) %>%
#   dplyr::mutate(
#     id = stringr::str_sub(EMD_CD, 1, 8)
#   )


# 법정도 코드 읽기 (2)
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

codeDataL1 = codeData %>%
  dplyr::select("시도코드", "시도명칭", "시군구코드", "시군구명칭", "읍면동코드", "읍면동명칭") %>% 
  dplyr::filter(
    stringr::str_detect(시도명칭, regex("서울특별시"))
    , stringr::str_detect(시군구명칭, regex("강서구"))
  ) 


# 통합 데이터셋
dataL5 = mapGlobal %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
  dplyr::left_join(dataL3, by = c("adm_dr_nm" = "투표구"))
# dplyr::inner_join(codeDataL1, by = c("EMD_CD" = "id")) # %>%

# 서울 강서구
# mapData = ggmap::get_map(
#   location = c(lon = 126.822838, lat = 37.560797)
#   , zoom = 13
#   , maptype = "hybrid"
# )

# ************************************************
# 선거 주제도
# ************************************************
saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "서울특별시_강서구_선거_주제도")
plotSubTitle = sprintf("%s", "서울특별시 강서구 선거 주제도")

ggplotDefaultColor = hue_pal()(3)

ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL4, aes(fill = factor(val)), inherit.aes = FALSE) +
  geom_sf_text(data = dataL4, aes(label = 읍면동명칭)) +
  # ggrepel::geom_label_repel(data = dataL4, aes(label = 읍면동명칭)) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[2], "3" = ggplotDefaultColor[3])
    , labels = c("자유한국당", "더불어민주당", "기타야당")
  ) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
  theme(
    text = element_text(size = 16)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , plot.subtitle = element_text(hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg2, width = 12, height = 8, dpi = 600)



# ************************************************
# 스토리 보드
# ************************************************
# 테이블
ggTable = dataL3 %>% 
  dplyr::select(투표구, 자유한국당, 더불어민주당, 기타야당) %>%
  dplyr::mutate(
    자유한국당 = scales::comma(자유한국당)
    , 더불어민주당 = scales::comma(더불어민주당)
    , 기타야당 = scales::comma(기타야당)
  ) %>% 
  dplyr::arrange(factor(투표구, levels = selData$투표구))

ggTableL1 = ggpubr::ggtexttable(ggTable, rows = NULL)

# 빈도분포
ggFreqPlot = ggplot(dataL4, aes(x = 투표구, y = val, fill = key, group = key, label = round(val, 0))) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = "선거 빈도분포") +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  )

# 지도
ggMapPlot = ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE) +
  geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
  # ggrepel::geom_text_repel(data = dataL5, aes(label = 읍면동명칭, geometry = geometry), stat = "sf_coordinates") +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[2], "3" = ggplotDefaultColor[3])
    , labels = c("자유한국당", "더불어민주당", "기타야당")
  ) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL)

ggMapPlotTheme = theme(
  text = element_text(size = 14)
  , panel.grid.major.x = element_blank()
  , panel.grid.major.y = element_blank()
  , panel.grid.minor.x = element_blank()
  , panel.grid.minor.y = element_blank()
  , axis.text.x = element_blank()
  , axis.ticks.x = element_blank()
  , axis.title.x = element_blank()
  , axis.text.y = element_blank()
  , axis.ticks.y = element_blank()
  , axis.title.y = element_blank()
  , plot.subtitle = element_text(hjust = 1)
  , legend.position = "none"
)

saveImgMerge = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "서울특별시_강서구_선거_통합")
plotSubTitle = sprintf("%s", "[서울특별시 강서구] 선거 주제도")

(ggMapPlot & ggMapPlotTheme ) / (ggFreqPlot | ggTableL1) +
  patchwork::plot_layout(heights = c(3, 1)) +
  ggsave(filename = saveImgMerge, width = 10, height = 10, dpi = 600)


#=================================================
# 충청남도 아산시 인구현황
#=================================================
# 선거 데이터 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0214_선거분석(강서병).xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 5)

dataL1 = data %>%
  as.tibble() %>%
  na.omit() %>%
  readr::type_convert()

statData = dataL1 %>% 
  dplyr::group_by(투표구) %>% 
  dplyr::summarise(
    sumVal = sum(투표수, na.rm = TRUE) 
  )

dataL2 = dataL1 %>% 
  dplyr::left_join(statData, by = c("투표구" = "투표구")) %>% 
  tidyr::spread(key = "나이", value = "투표수")

# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(CRS("+proj=longlat"))

# 법정동 코드 읽기 (2)
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

codeDataL1 = codeData %>%
  dplyr::filter(
    stringr::str_detect(시도명칭, regex("충청남도"))
    , stringr::str_detect(시군구명칭, regex("아산시"))
  ) 


# 통합 데이터셋
dataL5 = mapGlobal %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
  dplyr::left_join(dataL2, by = c("adm_dr_nm" = "투표구")) 

# 중심 위/경도 반환
posData = sf::st_centroid(dataL5$geometry) %>% 
  sf::st_coordinates() %>% 
  as.tibble() %>% 
  dplyr::rename(
    "lon" = "X"
    , "lat" = "Y"
  )

dataL6 = dplyr::bind_cols(dataL5, posData)

# ************************************************
# 선거 주제도
# ************************************************
dataL7 = na.omit(dataL6)

dataL8 = dataL7 %>% 
  as.tibble() %>% 
  dplyr::mutate(
    geometry = NULL
  )

plotSubTitle = sprintf("%s", "충청남도 아산시 전체 인구현황")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL6, fill = NA, inherit.aes = FALSE) +
  geom_sf_text(data = dataL6, aes(label = 읍면동명칭)) +
  scatterpie::geom_scatterpie(
    aes(x = lon, y = lat, group = factor(읍면동명칭), r = 0.015)
    , cols=c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
    , data = dataL8, color = NA, alpha = 0.75
  ) +
  scatterpie::geom_scatterpie_legend(dataL8$sumVal/1500000, x = 126.85, y = 36.67) +
  labs(
    x = NULL
    , y = NULL
    , color = NULL
    , fill = NULL
    , subtitle = plotSubTitle
  ) +
  theme(
    text = element_text(size = 14)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , legend.position = "top"
    , legend.box = "horizontal"
    , plot.margin = unit(c(0, 0, 0, 0), 'lines')
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)



plotSubTitle2 = sprintf("%s", "충청남도 아산시 일부 인구현황 (크기 비율 X)")
plotSubTitle2 = sprintf("%s", "충청남도 아산시 일부 인구현황 (크기 비율 O)")
saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle2)

ggplot() +
  theme_bw() +
  # coord_fixed(ratio) +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL7, fill = NA, inherit.aes = FALSE) +
  geom_sf_text(data = dataL7, aes(label = 읍면동명칭)) +
  # scatterpie::geom_scatterpie(
  #   aes(x = lon, y = lat, group = factor(읍면동명칭), r = 0.01)
  #   , cols=c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
  #   , data = dataL8, color = NA, alpha = 0.75
  #   ) +
  scatterpie::geom_scatterpie(
    aes(x = lon, y = lat, group = factor(읍면동명칭), r = sumVal/2000000)
    , cols=c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
    , data = dataL8, color = NA, alpha = 0.75
  ) +
  scatterpie::geom_scatterpie_legend(dataL8$sumVal/1500000, x = 126.84, y = 36.72) +
  labs(
    x = NULL
    , y = NULL
    , color = NULL
    , fill = NULL
    , subtitle = plotSubTitle2
  ) +
  theme(
    text = element_text(size = 14)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , legend.position = "top"
    , legend.box = "horizontal"
    , plot.margin = unit(c(0, 0, 0, 0), 'lines')
  ) +
  ggsave(filename = saveImg2, width = 12, height = 8, dpi = 600)


#=================================================
# 충청남도 아산시 세부 주제도
#=================================================
# 선거 데이터 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0214_선거분석(강서병).xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 6)
dataGeo = openxlsx::read.xlsx(fileInfo, sheet = 7)

# 세부 투표구에 대한 위/경도 반환
dataGeoL1 = dataGeo %>% 
  dplyr::mutate(
    addr = stringr::str_c(주소, 건물명, sep = " ")
  )

addrList = dataGeoL1$addr%>% unique %>% sort %>%
  as.tibble()

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "충남 아산시 투표구 정보")

# 각 주소에 따라 위/경도 반환
# for (i in 1:nrow(addrList)) {
#   
#   # 구글 API 하루 제한
#   addrData = ggmap::mutate_geocode(addrList[i, 'value'], value, source = "google")
# 
#   if (nrow(addrData) < 1) { next }
# 
#   readr::write_csv(x = addrData, file = saveFile, append = TRUE)
# }

addrData = readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

dataGeoL2 = dataGeoL1 %>% 
  dplyr::left_join(addrData, by = c("addr" = "value"))

# summary(dataGeoL2)

dataL1 = data %>%
  as.tibble() %>%
  na.omit() %>%
  readr::type_convert()

dataL2 = dataL1 %>% 
  tidyr::gather(-c(투표구, 세부투표구, 종류), key = "key", value = "val") %>% 
  dplyr::group_by(투표구, key) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(key = "key", value = "meanVal")

dataL3 = dataL2 %>% 
  rowwise(투표구) %>% 
  dplyr::mutate(
    sumVal = sum(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , maxVal = max(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , meanVal = (더불어민주당 / sumVal) * 100
    , meanVal2 = (자유한국당 / sumVal) * 100
    , meanVal3 = (중도층 / sumVal) * 100
    , val = dplyr::case_when(
      자유한국당 == maxVal ~ 1
      , 더불어민주당 == maxVal ~ 2
      , 중도층 == maxVal ~ 3
    )
  )

dataDtlL2 = dataL1 %>% 
  tidyr::gather(-c(투표구, 세부투표구, 종류), key = "key", value = "val") %>% 
  dplyr::group_by(세부투표구, key) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(key = "key", value = "meanVal")

dataDtlL3 = dataDtlL2 %>% 
  rowwise(세부투표구) %>% 
  dplyr::mutate(
    sumVal = sum(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , maxVal = max(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , meanVal = (더불어민주당 / sumVal) * 100
    , meanVal2 = (자유한국당 / sumVal) * 100
    , meanVal3 = (중도층 / sumVal) * 100
    , val = dplyr::case_when(
      자유한국당 == maxVal ~ 1
      , 더불어민주당 == maxVal ~ 2
      , 중도층 == maxVal ~ 3
    )
  ) %>% 
  dplyr::left_join(dataGeoL2, by = c("세부투표구" = "세부투표구")) %>% 
  dplyr::mutate(
    label = str_match_all(세부투표구, "제[[:digit:]]+투") %>% unlist()
  )

# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(CRS("+proj=longlat"))

# 법정동 코드 읽기 (2)
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

codeDataL1 = codeData %>%
  dplyr::filter(
    stringr::str_detect(시도명칭, regex("충청남도"))
    , stringr::str_detect(시군구명칭, regex("아산시"))
  ) 

# 통합 데이터셋
dataL5 = mapGlobal %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
  dplyr::left_join(dataL3, by = c("adm_dr_nm" = "투표구")) 


# ************************************************
# 선거 주제도
# ************************************************
plotSubTitle = sprintf("%s", "충청남도 아산시 선거 주제도")
saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplotDefaultColor = hue_pal()(3)

ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 0.3) +
  geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
  # geom_point(data = dataDtlL3, aes(x = lon, y = lat, color = factor(val)), shape = 16, show.legend = FALSE) +
  # ggrepel::geom_label_repel(
  #   data = dataDtlL3
  #   , aes(x = lon, y = lat, fill = factor(val), label = label)
  #   , color = "white"
  #   , segment.color = "black"
  #   , show.legend = FALSE
  #   , segment.size = 0.2
  #   , size = 3
  # ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  scale_color_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "기타야당")
  ) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
  theme(
    text = element_text(size = 16)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , plot.subtitle = element_text(hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg2, width = 8, height = 10, dpi = 600)


dataDtlL4 = dataDtlL3 %>%
  dplyr::select(-c(더불어민주당, 자유한국당, 중도층, sumVal, val, maxVal, 건물명, 주소, addr, lon, lat, label)) %>%
  dplyr::rename(
    더불어민주당 = meanVal
    , 자유한국당 = meanVal2
    , 중도층 = meanVal3
  ) %>% 
  tidyr::gather(-c(세부투표구), key = "key", value = "val") %>% 
  dplyr::mutate(
    label = str_match_all(세부투표구, "제[[:digit:]]+투") %>% unlist()
    , 투표구 = str_replace_all(세부투표구, pattern = "제[[:digit:]]+투", replacement = "")
  ) %>% 
  dplyr::na_if(0)

# 정당에 따른 정렬
dataDtlL4$key = forcats::fct_relevel(dataDtlL4$key, rev(c("자유한국당", "더불어민주당", "중도층")))

selLabel = paste0("제", c(1:99), "투")
dataDtlL4$label = forcats::fct_relevel(dataDtlL4$label, selLabel)
# dataDtlL4$label = forcats::fct_relevel(dataDtlL4$label, rev(selLabel))

# ************************************************
# 선거 빈도분포
# ************************************************
plotSubTitle = sprintf("%s", "충청남도 아산시 선거 빈도분포")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(dataDtlL4, aes(x = label, y = val, fill = key, group = key, label = round(val, 0))) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white") +
  coord_flip() +
  labs(x = "세부 투표구", y = "비율", fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 14)
    , legend.position = "top"
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
  ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("자유한국당" = ggplotDefaultColor[1], "더불어민주당" = ggplotDefaultColor[3], "중도층" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  # facet_wrap(~투표구, scale = "free", ncol = 3) +
  facet_wrap(~투표구, scale = "free", ncol = 5) +
  # facet_grid(~ 투표구, space = "free") +
  # facet_wrap(~투표구, scale = "free", space = "free", ncol = 5) +
  # scale_x_discrete(drop = FALSE) +
  ggsave(filename = saveImg, width = 16, height = 12, dpi = 600)


# ************************************************
# 스토리 보드
# ************************************************
# 빈도분포
ggFreqPlot = ggplot(dataDtlL4, aes(x = label, y = val, fill = key, group = key, label = round(val, 0))) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_flip() +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = NULL) +
  theme(
    text = element_text(size = 9)
    , legend.position = "none"
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
  ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("자유한국당" = ggplotDefaultColor[1], "더불어민주당" = ggplotDefaultColor[3], "중도층" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  facet_wrap(~투표구, scale = "free", ncol = 4)


# 지도
ggMapPlot = ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 0.3) +
  geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
  # geom_point(data = dataDtlL3, aes(x = lon, y = lat, color = factor(val)), shape = 16, show.legend = FALSE) +
  # ggrepel::geom_label_repel(
  #   data = dataDtlL3
  #   , aes(x = lon, y = lat, fill = factor(val), label = label)
  #   , color = "white"
  #   , segment.color = "black"
  #   , show.legend = FALSE
  #   , segment.size = 0.2
  #   , size = 3
  # ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  scale_color_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "기타야당")
  ) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL)

ggMapPlotTheme = theme(
  text = element_text(size = 16)
  , panel.grid.major.x = element_blank()
  , panel.grid.major.y = element_blank()
  , panel.grid.minor.x = element_blank()
  , panel.grid.minor.y = element_blank()
  , axis.text.x = element_blank()
  , axis.ticks.x = element_blank()
  , axis.title.x = element_blank()
  , axis.text.y = element_blank()
  , axis.ticks.y = element_blank()
  , axis.title.y = element_blank()
  , plot.subtitle = element_text(hjust = 1)
  , legend.position = "top"
)


plotSubTitle = sprintf("%s", "충청남도 아산시 선거 통합도")
saveImgMerge = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

(ggMapPlot & ggMapPlotTheme ) / (ggFreqPlot) +
  patchwork::plot_layout(heights = c(2, 1)) +
  ggsave(filename = saveImgMerge, width = 10, height = 20, dpi = 600)


#=================================================
# 서울시 강서구 세부 주제도
#=================================================
# 선거 데이터 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0214_선거분석(강서병).xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 8)
dataGeo = openxlsx::read.xlsx(fileInfo, sheet = 10)

# 세부 투표구에 대한 위/경도 반환
dataGeoL1 = dataGeo %>% 
  dplyr::mutate(
    addr = stringr::str_c(주소, 건물명, sep = " ")
  )

addrList = dataGeoL1$addr%>% unique %>% sort %>%
  as.tibble()

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "서울시 강서구 투표구 정보")

# 각 주소에 따라 위/경도 반환
# for (i in 1:nrow(addrList)) {
# 
#   # 구글 API 하루 제한
#   addrData = ggmap::mutate_geocode(addrList[i, 'value'], value, source = "google")
# 
#   if (nrow(addrData) < 1) { next }
# 
#   readr::write_csv(x = addrData, file = saveFile, append = TRUE)
# }

addrData = readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

dataGeoL2 = dataGeoL1 %>% 
  dplyr::left_join(addrData, by = c("addr" = "value"))

# summary(dataGeoL2)

dataL1 = data %>%
  as.tibble() %>%
  readr::type_convert()

dataL2 = dataL1 %>% 
  dplyr::filter(세부투표구 %in% c("소계")) %>% 
  rowwise() %>% 
  dplyr::mutate(
    중도층 = sum(dplyr::c_across(matches("중도층")), na.rm = TRUE)
  ) %>% 
  dplyr::select(-tidyselect::matches("중도층[0-9]")) %>% 
  dplyr::select(-c(종류)) %>% 
  tidyr::gather(-c(투표구, 세부투표구), key = "key", value = "val") %>% 
  dplyr::group_by(투표구, key) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(key = "key", value = "meanVal")


dataL3 = dataL2 %>% 
  rowwise(투표구) %>% 
  dplyr::mutate(
    sumVal = sum(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , maxVal = max(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , meanVal = (더불어민주당 / sumVal) * 100
    , meanVal2 = (자유한국당 / sumVal) * 100
    , meanVal3 = (중도층 / sumVal) * 100
    , val = dplyr::case_when(
      자유한국당 == maxVal ~ 1
      , 더불어민주당 == maxVal ~ 2
      , 중도층 == maxVal ~ 3
    )
    , 투표구2 = dplyr::case_when(
        stringr::str_detect(투표구, regex("가양제1동")) ~ "가양1동"
        , stringr::str_detect(투표구, regex("가양제2동")) ~ "가양2동"
        , stringr::str_detect(투표구, regex("가양제3동")) ~ "가양3동"
        , stringr::str_detect(투표구, regex("등촌제1동")) ~ "등촌1동"
        , stringr::str_detect(투표구, regex("등촌제2동")) ~ "등촌2동"
        , stringr::str_detect(투표구, regex("등촌제3동")) ~ "등촌3동"
        , stringr::str_detect(투표구, regex("발산제1동")) ~ "발산1동"
        , stringr::str_detect(투표구, regex("방화제1동")) ~ "방화1동"
        , stringr::str_detect(투표구, regex("방화제2동")) ~ "방화2동"
        , stringr::str_detect(투표구, regex("방화제3동")) ~ "방화3동"
        , stringr::str_detect(투표구, regex("화곡제1동")) ~ "화곡1동"
        , stringr::str_detect(투표구, regex("화곡제2동")) ~ "화곡2동"
        , stringr::str_detect(투표구, regex("화곡제3동")) ~ "화곡3동"
        , stringr::str_detect(투표구, regex("화곡제4동")) ~ "화곡4동"
        , stringr::str_detect(투표구, regex("화곡제5동")) ~ "화곡5동"
        , stringr::str_detect(투표구, regex("화곡제6동")) ~ "화곡6동"
        , stringr::str_detect(투표구, regex("화곡제8동")) ~ "화곡8동"
        , stringr::str_detect(투표구, regex("염창동")) ~ "염창동"
        , stringr::str_detect(투표구, regex("화곡본동")) ~ "화곡본동"
        , stringr::str_detect(투표구, regex("공항동")) ~ "공항동"
        , stringr::str_detect(투표구, regex("우장산동")) ~ "우장산동"
      )
  )

# ******************************************************************************
# 출력 데이터
# ******************************************************************************
writeData = dataL1 %>% 
  dplyr::filter(세부투표구 %in% c("소계")) %>% 
  rowwise() %>% 
  dplyr::mutate(
    중도층 = sum(dplyr::c_across(matches("중도층")), na.rm = TRUE)
  ) %>% 
  dplyr::select(-tidyselect::matches("중도층[0-9]")) %>% 
  dplyr::mutate(
    sumVal = sum(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , maxVal = max(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , meanVal = (더불어민주당 / sumVal) * 100
    , meanVal2 = (자유한국당 / sumVal) * 100
    , meanVal3 = (중도층 / sumVal) * 100
  ) %>% 
  dplyr::rename(
    "합계" = sumVal
    , "%더불어민주당" = meanVal
    , "%자유한국당" = meanVal2
    , "%중도층" = meanVal3
  ) %>%
  dplyr::select(-c(maxVal, 세부투표구)) %>% 
  dplyr::arrange(종류, 투표구)
    

writeDataL2 = dataL3 %>%
  dplyr::rename(
    "합계" = sumVal
    , "평균 더불어민주당" = 더불어민주당
    , "평균 자유한국당" = 자유한국당
    , "평균 중도층" = 중도층
    , "%평균 더불어민주당" = meanVal
    , "%평균 자유한국당" = meanVal2
    , "%평균 중도층" = meanVal3
  ) %>%
  dplyr::select(-c(maxVal, val, 투표구2))


typeList = writeData$종류 %>% unique() %>% sort()

# typeInfo = typeList[1]

saveXlsxFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "[1단계] 서울특별시 강서구 선거 데이터")
wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "선거 데이터")
openxlsx::writeData(wb, "선거 데이터", writeDataL2, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)

for (typeInfo in typeList) {
  
  writeDataL1 = writeData %>% 
    dplyr::filter(종류 == typeInfo)
  
  openxlsx::addWorksheet(wb, typeInfo)
  openxlsx::writeData(wb, typeInfo, writeDataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
}

openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)


dataDtlL2 = dataL1 %>% 
  dplyr::filter(! 세부투표구 %in% c("소계", "관내사전투표", "선거일투표")) %>% 
  rowwise() %>%
  dplyr::mutate(
    중도층 = sum(dplyr::c_across(matches("중도층")), na.rm = TRUE)
  ) %>% 
  dplyr::select(-tidyselect::matches("중도층[0-9]")) %>% 
  dplyr::select(-c(종류)) %>%
  tidyr::gather(-c(투표구, 세부투표구), key = "key", value = "val") %>% 
  dplyr::group_by(세부투표구, key) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(key = "key", value = "meanVal")

dataDtlL3 = dataDtlL2 %>% 
  rowwise(세부투표구) %>% 
  dplyr::mutate(
    sumVal = sum(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , maxVal = max(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , meanVal = (더불어민주당 / sumVal) * 100
    , meanVal2 = (자유한국당 / sumVal) * 100
    , meanVal3 = (중도층 / sumVal) * 100
    , val = dplyr::case_when(
      자유한국당 == maxVal ~ 1
      , 더불어민주당 == maxVal ~ 2
      , 중도층 == maxVal ~ 3
      
    )
  ) %>% 
  dplyr::left_join(dataGeoL2, by = c("세부투표구" = "세부투표구")) %>% 
  dplyr::mutate(
    label = str_match_all(세부투표구, "제[[:digit:]]+투") %>% unlist()
  )

# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(CRS("+proj=longlat"))

# 법정동 코드 읽기 (2)
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

codeDataL1 = codeData %>%
  dplyr::filter(
    stringr::str_detect(시도명칭, regex("서울특별시"))
    , stringr::str_detect(시군구명칭, regex("강서구"))
  ) 

# 통합 데이터셋
dataL5 = mapGlobal %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
  dplyr::left_join(dataL3, by = c("adm_dr_nm" = "투표구2")) 


# ************************************************
# 선거 주제도
# ************************************************
plotSubTitle = sprintf("%s", "서울특별시 강서구 선거 주제도")
saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplotDefaultColor = hue_pal()(3)

ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 0.3) +
  geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
  # geom_point(data = dataDtlL3, aes(x = lon, y = lat, color = factor(val)), shape = 16, show.legend = FALSE) +
  # ggrepel::geom_label_repel(
  #   data = dataDtlL3
  #   , aes(x = lon, y = lat, fill = factor(val), label = label)
  #   , color = "white"
  #   , segment.color = "black"
  #   , show.legend = FALSE
  #   , segment.size = 0.2
  #   , size = 3
  # ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  scale_color_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "기타야당")
  ) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
  theme(
    text = element_text(size = 16)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , plot.subtitle = element_text(hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg2, width = 8, height = 10, dpi = 600)

dataDtlL4 = dataDtlL3 %>%
  dplyr::select(-c(더불어민주당, 자유한국당, 중도층, sumVal, val, maxVal, 건물명, 주소, addr, lon, lat, label)) %>%
  dplyr::rename(
    더불어민주당 = meanVal
    , 자유한국당 = meanVal2
    , 중도층 = meanVal3
  ) %>% 
  tidyr::gather(-c(세부투표구), key = "key", value = "val") %>% 
  dplyr::mutate(
    label = str_match_all(세부투표구, "제[[:digit:]]+투") %>% unlist()
    , 투표구 = str_replace_all(세부투표구, pattern = "제[[:digit:]]+투", replacement = "")
    , 투표구2 = dplyr::case_when(
      stringr::str_detect(투표구, regex("가양제1동")) ~ "가양1동"
      , stringr::str_detect(투표구, regex("가양제2동")) ~ "가양2동"
      , stringr::str_detect(투표구, regex("가양제3동")) ~ "가양3동"
      , stringr::str_detect(투표구, regex("등촌제1동")) ~ "등촌1동"
      , stringr::str_detect(투표구, regex("등촌제2동")) ~ "등촌2동"
      , stringr::str_detect(투표구, regex("등촌제3동")) ~ "등촌3동"
      , stringr::str_detect(투표구, regex("발산제1동")) ~ "발산1동"
      , stringr::str_detect(투표구, regex("방화제1동")) ~ "방화1동"
      , stringr::str_detect(투표구, regex("방화제2동")) ~ "방화2동"
      , stringr::str_detect(투표구, regex("방화제3동")) ~ "방화3동"
      , stringr::str_detect(투표구, regex("화곡제1동")) ~ "화곡1동"
      , stringr::str_detect(투표구, regex("화곡제2동")) ~ "화곡2동"
      , stringr::str_detect(투표구, regex("화곡제3동")) ~ "화곡3동"
      , stringr::str_detect(투표구, regex("화곡제4동")) ~ "화곡4동"
      , stringr::str_detect(투표구, regex("화곡제5동")) ~ "화곡5동"
      , stringr::str_detect(투표구, regex("화곡제6동")) ~ "화곡6동"
      , stringr::str_detect(투표구, regex("화곡제8동")) ~ "화곡8동"
      , stringr::str_detect(투표구, regex("염창동")) ~ "염창동"
      , stringr::str_detect(투표구, regex("화곡본동")) ~ "화곡본동"
      , stringr::str_detect(투표구, regex("공항동")) ~ "공항동"
      , stringr::str_detect(투표구, regex("우장산동")) ~ "우장산동"
    )
  ) %>% 
  dplyr::na_if(0)

# 정당에 따른 정렬
dataDtlL4$key = forcats::fct_relevel(dataDtlL4$key, rev(c("자유한국당", "더불어민주당", "중도층")))

selLabel = paste0("제", c(1:99), "투")
dataDtlL4$label = forcats::fct_relevel(dataDtlL4$label, selLabel)
# dataDtlL4$label = forcats::fct_relevel(dataDtlL4$label, rev(selLabel))

# ************************************************
# 선거 빈도분포
# ************************************************
plotSubTitle = sprintf("%s", "서울특별시 강서구 선거 빈도분포")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(dataDtlL4, aes(x = label, y = val, fill = key, group = key, label = round(val, 0))) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white") +
  coord_flip() +
  labs(x = "세부 투표구", y = "비율", fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 14)
    , legend.position = "top"
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
  ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("자유한국당" = ggplotDefaultColor[1], "더불어민주당" = ggplotDefaultColor[3], "중도층" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  # facet_wrap(~투표구2, scale = "free", ncol = 3) +
  facet_wrap(~투표구2, scale = "free", ncol = 4) +
  # facet_wrap(~투표구, scale = "free", ncol = 5) +
  # facet_grid(~ 투표구, space = "free") +
  # facet_wrap(~투표구, scale = "free", space = "free", ncol = 5) +
  # scale_x_discrete(drop = FALSE) +
  ggsave(filename = saveImg, width = 16, height = 12, dpi = 600)


# ************************************************
# 스토리 보드
# ************************************************
# 빈도분포
ggFreqPlot = ggplot(dataDtlL4, aes(x = label, y = val, fill = key, group = key, label = round(val, 0))) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_flip() +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = NULL) +
  theme(
    text = element_text(size = 9)
    , legend.position = "none"
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
  ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("자유한국당" = ggplotDefaultColor[1], "더불어민주당" = ggplotDefaultColor[3], "중도층" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  facet_wrap(~투표구, scale = "free", ncol = 4)


# 지도
ggMapPlot = ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 0.3) +
  geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
  # geom_point(data = dataDtlL3, aes(x = lon, y = lat, color = factor(val)), shape = 16, show.legend = FALSE) +
  # ggrepel::geom_label_repel(
  #   data = dataDtlL3
  #   , aes(x = lon, y = lat, fill = factor(val), label = label)
  #   , color = "white"
  #   , segment.color = "black"
  #   , show.legend = FALSE
  #   , segment.size = 0.2
  #   , size = 3
  # ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  scale_color_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , labels = c("자유한국당", "더불어민주당", "기타야당")
  ) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL)

ggMapPlotTheme = theme(
  text = element_text(size = 16)
  , panel.grid.major.x = element_blank()
  , panel.grid.major.y = element_blank()
  , panel.grid.minor.x = element_blank()
  , panel.grid.minor.y = element_blank()
  , axis.text.x = element_blank()
  , axis.ticks.x = element_blank()
  , axis.title.x = element_blank()
  , axis.text.y = element_blank()
  , axis.ticks.y = element_blank()
  , axis.title.y = element_blank()
  , plot.subtitle = element_text(hjust = 1)
  , legend.position = "top"
)


plotSubTitle = sprintf("%s", "서울특별시 강서구 선거 통합도")
saveImgMerge = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

(ggMapPlot & ggMapPlotTheme ) / (ggFreqPlot) +
  patchwork::plot_layout(heights = c(2, 1)) +
  ggsave(filename = saveImgMerge, width = 10, height = 20, dpi = 600)


#=================================================
# 서울특별시 강서구 인구현황
#=================================================
# 선거 데이터 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0214_선거분석(강서병).xlsx"))
# data = openxlsx::read.xlsx(fileInfo, sheet = 5)
data = openxlsx::read.xlsx(fileInfo, sheet = 9)

dataL1 = data %>%
  as.tibble() %>%
  na.omit() %>%
  readr::type_convert()

dataL2 = dataL1 %>%
  dplyr::mutate(
    투표구 = stringr::str_trim(투표구)
    , 나이 = stringr::str_trim(나이)
    , 투표수 = as.numeric(투표수)
  ) %>% 
  dplyr::filter(! 나이 %in% c("합계", "평균연령", "중위연령", "65세이상", "85세이상", "100세이상", "15세미만")) %>%
  dplyr::mutate(
    age = strsplit(나이, "세") %>% unlist() %>% as.numeric()
  ) %>% 
  dplyr::mutate(
    type = dplyr::case_when(
      18 <= age & age <= 20 ~ "18-20세"
      , 21 <= age & age <= 30 ~ "21-30세"
      , 31 <= age & age <= 40 ~ "31-40세"
      , 41 <= age & age <= 50 ~ "41-50세"
      , 51 <= age & age <= 60 ~ "51-60세"
      , 61 <= age & age <= 70 ~ "61-70세"
      , 71 <= age ~ "71세 이상"
    )
  ) %>% 
  dplyr::filter(
    ! is.na(age)
    , ! is.na(type)
  ) %>% 
  dplyr::select(-나이, -age)

statData = dataL2 %>% 
  dplyr::group_by(투표구, type) %>% 
  dplyr::summarise(
    sumKeyVal = sum(투표수, na.rm = TRUE) 
  )

statDataL2 = dataL2 %>% 
  dplyr::group_by(투표구) %>% 
  dplyr::summarise(
    sumVal = sum(투표수, na.rm = TRUE) 
  )

# dataL2$투표구 %>% unique() %>% sort()
# [1] "가양1동"  "가양2동"  "가양3동"  "강서구"   "공항동"   "등촌1동"  "등촌2동" 
# [8] "등촌3동"  "발산1동"  "방화1동"  "방화2동"  "방화3동"  "염창동"   "우장산동"
# [15] "화곡1동"  "화곡2동"  "화곡3동"  "화곡4동"  "화곡6동"  "화곡8동"  "화곡본동"

# codeDataL1$읍면동명칭 %>% unique() %>% sort()
# [1] "가양1동"  "가양2동"  "가양3동"  "공항동"   "등촌1동"  "등촌2동"  "등촌3동" 
# [8] "발산1동"  "방화1동"  "방화2동"  "방화3동"  "염창동"   "우장산동" "화곡1동" 
# [15] "화곡2동"  "화곡3동"  "화곡4동"  "화곡6동"  "화곡8동"  "화곡본동"

dataL3 = statData %>% 
  dplyr::left_join(statDataL2, by = c("투표구" = "투표구")) %>% 
  tidyr::spread(key = "type", value = "sumKeyVal")

# dataL3 = dataL1 %>%
#   dplyr::left_join(statData, by = c("투표구" = "투표구", "type" = "type")) %>%
#   tidyr::spread(key = "type", value = "투표수")
#   # tidyr::spread(key = "나이", value = "투표수")


# ******************************************************************************
# 출력 데이터
# ******************************************************************************
writeData = dataL3 %>% 
  dplyr::rename(
    "합계" = sumVal
  ) %>%
  dplyr::arrange(투표구)

saveXlsxFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "[1단계] 서울특별시 강서구 인구현황 데이터")
wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "인구현황 데이터")
openxlsx::writeData(wb, "인구현황 데이터", writeData, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)


# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(CRS("+proj=longlat"))

# 법정동 코드 읽기 (2)
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

codeDataL1 = codeData %>%
  dplyr::filter(
    stringr::str_detect(시도명칭, regex("서울특별시"))
    , stringr::str_detect(시군구명칭, regex("강서구"))
  ) 

# 통합 데이터셋
dataL5 = mapGlobal %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
  dplyr::left_join(dataL3, by = c("adm_dr_nm" = "투표구")) 

# 중심 위/경도 반환
posData = sf::st_centroid(dataL5$geometry) %>% 
  sf::st_coordinates() %>% 
  as.tibble() %>% 
  dplyr::rename(
    "lon" = "X"
    , "lat" = "Y"
  )

dataL6 = dplyr::bind_cols(dataL5, posData)

# ************************************************
# 선거 주제도
# ************************************************
dataL7 = na.omit(dataL6)

dataL8 = dataL7 %>% 
  as.tibble() %>% 
  dplyr::mutate(
    geometry = NULL
  )

# plotSubTitle2 = sprintf("%s", "서울특별시 강서구 선거 인구현황 (크기 비율 X)")
plotSubTitle2 = sprintf("%s", "서울특별시 강서구 선거 인구현황 (크기 비율 O)")
saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle2)

ggplot() +
  theme_bw() +
  # coord_fixed(ratio) +
  coord_fixed(ratio = 1) +
  geom_sf(data = dataL7, fill = NA, inherit.aes = FALSE) +
  geom_sf_text(data = dataL7, aes(label = 읍면동명칭)) +
  # scatterpie::geom_scatterpie(
  #   aes(x = lon, y = lat, group = factor(읍면동명칭), r = 0.005)
  #   , cols=c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
  #   , data = dataL8, color = NA, alpha = 0.75
  #   ) +
  scatterpie::geom_scatterpie(
    aes(x = lon, y = lat, group = factor(읍면동명칭), r = sumVal/5000000)
    , cols=c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
    , data = dataL8, color = NA, alpha = 0.75
  ) +
  scatterpie::geom_scatterpie_legend(
    dataL8$sumVal/5000000
    , x =  min(posData$lon, na.rm = TRUE) - 0.02
    , y = min(posData$lat, na.rm = TRUE)
    ) +
  labs(
    x = NULL
    , y = NULL
    , color = NULL
    , fill = NULL
    , subtitle = plotSubTitle2
  ) +
  theme(
    text = element_text(size = 14)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , legend.position = "top"
    , legend.box = "horizontal"
    , plot.margin = unit(c(0.2, 0, 0, 0), 'lines')
  ) +
  ggsave(filename = saveImg2, width = 10, height = 8, dpi = 600)






library(ggsubplot)
library(ggplot2)
library(maps)
library(plyr)

#Get world map info
world_map <- map_data("world")

#Create a base plot
p <- ggplot()  + geom_polygon(data=world_map,aes(x=long, y=lat,group=group), col = "blue4", fill = "lightgray") + theme_bw()

# Calculate the mean longitude and latitude per region (places where subplots are plotted),
cntr <- ddply(world_map,.(region),summarize,long=mean(long),lat=mean(lat))

# example data
myd <- data.frame (region = rep (c("USA","China","USSR","Brazil", "Australia","India", "Canada"),5),
                   categ = rep (c("A", "B", "C", "D", "E"),7), frequency = round (rnorm (35, 8000, 4000), 0))


subsetcntr  <- subset(cntr, region %in% c("USA","China","USSR","Brazil", "Australia","India", "Canada"))

simdat <- merge(subsetcntr, myd)
colnames(simdat) <- c( "region","long","lat", "categ", "myvar" )


myplot  <- p+geom_subplot2d(aes(long, lat, subplot = geom_bar(aes(x = categ, y = myvar, fill = categ, width=1), position = "identity")), ref = NULL, data = simdat)

print(myplot)















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
# R을 이용한 GEV 분포에 대한 CDF 및 PDF 시각화

# GEV 분포
# 1. GEV 분포를 작성하였으며 GEVcdn이 아닌 lmom의 함수로 구하고 싶습니다.
# 2. GEV cdf를 적분을 적용하여 pdf로 변환하여 pdf값도 산정하고 싶습니다.
# 3. 95분위수 값의 결과를 보고싶습니다!
# 4.산정된 분포에따른 데이터 값을 확인하고 싶습니다.
# 5. cdf와 pdf의 플럿을 그리고싶습니다.

# BMS
# # 네! 검토사항은 BMA가 가우시안분포로 잘돌아가는지에 대한 검토입니다!
# 확인해주셔야될부분은
# 1. 모델이 가우시안이 적용이되는 것과, MCMC가 잘적용되었는지가 궁금합니다.
# (mcmc='enumeration'에서 enumeration가 MCMC 방법이 맞는지가 궁금합니다.)
# 2. 분산과 표준편차가 잘산정되는지와 예측결과가 다음과같이 산정되는 것이 맞는지 결과가 괜찮은지에 대한 것입니다!

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0240"
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
library(tidyverse)
library(readr)
library(stringr)
library(lmom)
library(lmomco)
library(BMS)

#===============================================
# GEV 분포
#===============================================
set.seed(100)

# fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0240_Song_Final+Historical-Ensemble+result.csv"))
# # fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0240_Historical.csv"))
# da = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))
# 
# # da <- read.csv('F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Song_Final Historical-Ensemble result.csv.csv')
# pdf <- data.frame(da$극락교)
# cdf <- data.frame(da$극락교)
# # i = 2
# for (i in 2:3){
#   name = colnames(da)[i]
#   path <- 'F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/'
#   path <- paste0(path,name,'_param.csv')
#   dataset <- as.data.frame(da[,i])[,1]
#   lmompara = lmom::pelgev(lmom::samlmu(dataset,nmom= 20))
#   lmr_gev_param = lmom::lmrgev(lmompara,nmom=20)
#   
#  
#   
#   paramdf = data.frame(c(lmompara,lmr_gev_param))
#   colnames(paramdf) <- 'value'
#   # write.csv(paramdf, path)
#   
#   pd <- GEVcdn::dgev(dataset,location=lmompara[1],scale=lmompara[2],shape=lmompara[3])
#   cd <- GEVcdn::pgev(dataset,location=lmompara[1],scale=lmompara[2],shape=lmompara[3])
#   
#   
#   pdf <- data.frame(pdf,pd)
#   cdf <- data.frame(cdf,cd)
# }
# 
# c1=cdfgev(da$극락교,lmompara)
# 
# plot(da$극락교,c1)
# 
# pdf <- pdf[,-1]
# cdf <- cdf[,-1]
# colnames(pdf) <- colnames(da)[c(-1)]
# colnames(cdf) <- colnames(da)[c(-1)]
# write.csv(pdf,'F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Ensemble pdf.csv')
# write.csv(cdf,'F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Ensemble cdf.csv')


# ******************************************************************************
# 1. GEV 분포를 작성하였으며 GEVcdn이 아닌 lmom의 함수로 구하고 싶습니다.
# 2. GEV cdf를 적분을 적용하여 pdf로 변환하여 pdf값도 산정하고 싶습니다.
# 3. 95분위수 값의 결과를 보고싶습니다!
# 4.산정된 분포에따른 데이터 값을 확인하고 싶습니다.
# 5. cdf와 pdf의 플럿을 그리고싶습니다.
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0240_Song_Final+Historical-Ensemble+result.csv"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

colList = c("극락교", "OBS")
# colInfo = "극락교"

dataL2 = data.frame()
paramData = tibble::tibble()

for (colInfo in colList) {

  # 변수 선택
  selData = get(colInfo, data)
  
  clmData = lmomco::lmoms(selData)
  
  # 일반화된 극단값 분포를 위한 파라미터 정보 (xi 모수, alpha 알파, shape 모양)
  paramInfo = lmomco::pargev(clmData)
  
  # CDF 결과
  cdfRes = lmom::cdfgev(selData, para = paramInfo$para)
  
  # PDF 결과
  pdfRes = lmomco::pdfgev(selData, para = paramInfo)
  
  # 95 분위수 결과
  quagev(0.95, paramInfo)
  
  dataL1 = data.frame(type = colInfo, obs = selData, cdf = cdfRes, pdf = pdfRes) %>% 
    dplyr::arrange(obs)
  
  dataL2 = dplyr::bind_rows(dataL2, dataL1)
  
  tmpParamData = paramInfo$para %>% t() %>% as.tibble()
  paramData = dplyr::bind_rows(paramData, tmpParamData)
}

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "paramData")
readr::write_csv(x = paramData, file = saveFile)


dataL3 = dataL2 %>% 
  dplyr::filter(
    cdf >= 0.95
  )

  
# CDF 그림
subTitle = "CDF 그래프"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(dataL2, aes(x = obs, y = cdf, colour = type, size = type)) +
  geom_line() +
  labs(x = "OBS", y = "CDF", fill = NULL, subtitle = subTitle) +
  theme(
    text = element_text(size = 18)
  ) +
  scale_size_manual(values = 2:1) +
  scale_colour_manual(values = c("black", "green")) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# PDF 그림
subTitle = "PDF 그래프"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(dataL2, aes(x = obs, y = pdf, colour = type, size = type)) +
  geom_line() +
  labs(x = "OBS", y = "PDF", fill = NULL, subtitle = subTitle) +
  theme(
    text = element_text(size = 18)
  ) +
  scale_size_manual(values = 2:1) +
  scale_colour_manual(values = c("black", "green")) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
  
# 95 이상 CDF 그림
subTitle = "95 이상 CDF 그래프"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(dataL3, aes(x = obs, y = cdf, colour = type, size = type)) +
  geom_line() +
  labs(x = "OBS", y = "CDF", fill = NULL, subtitle = subTitle) +
  theme(
    text = element_text(size = 18)
  ) +
  scale_size_manual(values = 2:1) +
  scale_colour_manual(values = c("black", "green")) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# 95 이상 PDF 그림
subTitle = "95 이상 PDF 그래프"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(dataL3, aes(x = obs, y = pdf, colour = type, size = type)) +
  geom_line() +
  labs(x = "OBS", y = "PDF", fill = NULL, subtitle = subTitle) +
  theme(
    text = element_text(size = 18)
  ) +
  scale_size_manual(values = 2:1) +
  scale_colour_manual(values = c("black", "green")) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


#===============================================
# BMS
#===============================================

# Full enumaration of model space (Tables 4 and 5)
# BMS Package:
# da= read.csv('F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Historical.csv')

# fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0240_Historical.csv"))
# # da = read.csv(fileInfo)
# da = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))
# datafls
# 
# set.seed(2011)
# # x <- da[,-c(1,14)]
# x <- da[,-c(1, 2)]
# x <- as.data.frame(x)
# 
# bms_da <- BMS::bms(x,mcmc='enumeration',g='UIP',mprior='gaussian', burn = 1000, nmodel = 500, logstep = 10000)
# c1=coef(bms_da)
# coef(bms_da,exact=TRUE,std.coefs=TRUE)
# 
# 
# #bms_da$mprior.info
# write.csv(c1, "F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Markov BMA historical BMA unertainty.csv")
# 
# l1=predict(bms_da,x)
# 
# write.csv(l1, "F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Markov BMA historical BMA.csv")
# 
# ######### BMS projection
# bb=readr::read_csv('F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Song_Final Each Model SSP245 Ensemble result.csv.csv')
# TT=da[,2:12]
# 
# l2=predict(bms_da,TT)
# write.csv(l2, "F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/SSP2-4.5 Markov BMA historical BMA.csv")

# ******************************************************************************
# 네! 검토사항은 BMA가 가우시안분포로 잘돌아가는지에 대한 검토입니다!
# 확인해주셔야될부분은
# 1. 모델이 가우시안이 적용이되는 것과, MCMC가 잘적용되었는지가 궁금합니다.
# (mcmc='enumeration'에서 enumeration가 MCMC 방법이 맞는지가 궁금합니다.)
# 2. 분산과 표준편차가 잘산정되는지와 예측결과가 다음과같이 산정되는 것이 맞는지 결과가 괜찮은지에 대한 것입니다!
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0240_Historical.csv"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

dataL1 = data %>% 
  dplyr::select(-c(Period, Observation))

# MCMC 적용
mcmcModel = BMS::bms(dataL1, burn = 50000, iter = 1e+05, g = "BRIC", mprior = "uniform", nmodel = 2000, mcmc = "bd", user.int = F)

# 요약 결과
summary(mcmcModel)

# MCMC 그래프
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "MCMC 그래프")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(mcmcModel)
dev.off()

# 회귀계수
coefData = coef(mcmcModel) %>% 
  as.tibble()

saveCsvFile = sprintf("%s/%s.csv", globalVar$outPath, "Markov BMA historical BMA unertainty")
readr::write_csv(coefData, file = saveCsvFile)

# 예측 결과
mcmcPrd = predict(mcmcModel, dataL1) %>% 
  as.tibble()

saveCsvFile = sprintf("%s/%s.csv", globalVar$outPath, "Markov BMA historical BMA")
readr::write_csv(mcmcPrd, file = saveCsvFile)

######### BMS projection
# bb=readr::read_csv('F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/Song_Final Each Model SSP245 Ensemble result.csv.csv')
# TT=da[,2:12]
# 
# l2=predict(bms_da,TT)
# write.csv(l2, "F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/SSP2-4.5 Markov BMA historical BMA.csv")


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
# R을 이용한 2018년 사회조사 데이터 탐색 및 분석

# 제목 : 2018년 사회조사 데이터 전처리
# 저자 : 김건우
# 제작 시기 : 2021-11-12
# 기능 : 원본 CSV 파일 읽기, 코드 정보에 대한 범주형 변환, 정제 파일 저장

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0244"
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
library(treemap)
library(gmodels)
library(corrplot)

# 파일 찾기
fileInfo = Sys.glob(file.path(globalVar$inpPath, "보건_교육_안전_가족_환경_2018.csv"))

# ******************************************************
# 데이터 정제 (원본 CSV 파일 읽기, 7종 컬럼명 설정)
# ******************************************************
# 가구번호 : hshldNm
# 가구원번호 : hshldMmsNm
# 만연령 : age
# 가구주관계코드: hshldRlCd
# 교육정보코드 : edctnInCd
# 재학상태코드 : stdntStCd
# 재학학년코드 : gradeCd
data = readr::read_csv(file = fileInfo, col_names = c("hshldNm", "hshldMmsNm", "age", "hshldRlCd", "edctnInCd", "stdntStCd", "gradeCd"))

# 범주형 변수 (즉 컬럼명 맨 끝에 Cd가 포함된 경우) 변환
dataL1 = data %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::ends_with("Cd")), as.factor)

# ******************************************************
# 요약 통계
# ******************************************************
summary(dataL1)

# > summary(dataL1)
# hshldNm           hshldMmsNm             age              hshldRlCd    
# Min.   :    1.000   Length:42550       Min.   :  0.00000   01     :18546  
# 1st Qu.: 4557.000   Class :character   1st Qu.: 25.00000   03     :11393  
# Median : 8871.000   Mode  :character   Median : 45.00000   02     :10534  
# Mean   : 9096.633                      Mean   : 43.23217   06     :  996  
# 3rd Qu.:13626.000                      3rd Qu.: 60.00000   05     :  328  
# Max.   :18546.000                      Max.   :104.00000   04     :  313  

# edctnInCd     stdntStCd    gradeCd     
# 3      :11893   1   :  107   1   :  585  
# 5      : 8259   2   :  718   2   : 3678  
# 4      : 5648   3   : 6238   3   : 3702  
# 2      : 4469   4   :  717   4   :  518  
# 1      : 3981   5   :  877   5   :   54  
# 0      : 1596   NA's:33893   6   :   13  
# 6      : 1267                NA's:34000  
# 7      : 348 
# NA's   : 5089               


# ******************************************************
# 데이터 탐색
# ******************************************************
# 탐색 목적: 과제의 주제와 관련하여 데이터 시각화 탐색을 통해 확인하고자 하는 사항을 간단히 기술
# 데이터 시각화 탐색: 수업시간에 배운 그래프들(점 그래프, 막대 그래프, 누적 막대 그래프, 
# 선 그래프, 파이 차트, 박스 플롯, 트리맵)에서 서로 다른 유형의 그래프 2가지 이상을 사용하여 시각화. 
# 수집된 데이터의 항목 중에서 분석과 관련된 항목들을 포함하여 시각화
# 시각화한 그래프들에서 각 그래프가 나타내고 있는 의미/특징 설명.
# 예를 들어, 항목별 응답 분포, 유사 항목의 응답 분포 비교 등 데이터가 포함하고 있는 특징 설명

# 결측값 제거
dataL2 = na.omit(dataL1)

# 교육정도 이름 부여
dataL2[ , "edctnInNm"] = NA_character_
dataL2[which(dataL2$stdntStCd == 0), "edctnInNm"] = "안 받았음"
dataL2[which(dataL2$stdntStCd == 1), "edctnInNm"] = "초등학교"
dataL2[which(dataL2$stdntStCd == 2), "edctnInNm"] = "중학교"
dataL2[which(dataL2$stdntStCd == 3), "edctnInNm"] = "고등학교"
dataL2[which(dataL2$stdntStCd == 4), "edctnInNm"] = "대학(4년제 미만)"
dataL2[which(dataL2$stdntStCd == 5), "edctnInNm"] = "대학교(4년제 이상)"
dataL2[which(dataL2$stdntStCd == 6), "edctnInNm"] = "대학원 석사과정"
dataL2[which(dataL2$stdntStCd == 7), "edctnInNm"] = "대학원 박사 과정"

# 재학상태 이름 부여
dataL2[ , "stdntStNm"] = NA_character_
dataL2[which(dataL2$stdntStCd == 1), "stdntStNm"] = "졸업"
dataL2[which(dataL2$stdntStCd == 2), "stdntStNm"] = "재학"
dataL2[which(dataL2$stdntStCd == 3), "stdntStNm"] = "수료"
dataL2[which(dataL2$stdntStCd == 4), "stdntStNm"] = "휴학"
dataL2[which(dataL2$stdntStCd == 5), "stdntStNm"] = "중퇴"

# 재학학년 이름 부여
dataL2[ , "gradeNm"] = NA_character_
dataL2[which(dataL2$gradeCd == 1), "gradeNm"] = "1학년"
dataL2[which(dataL2$gradeCd == 2), "gradeNm"] = "2학년"
dataL2[which(dataL2$gradeCd == 3), "gradeNm"] = "3학년"
dataL2[which(dataL2$gradeCd == 4), "gradeNm"] = "4학년"
dataL2[which(dataL2$gradeCd == 5), "gradeNm"] = "5학년"
dataL2[which(dataL2$gradeCd == 6), "gradeNm"] = "6학년"

# 빈도
table(dataL2$stdntStNm)

# %상대 빈도
(table(dataL2$stdntStNm) / length(dataL2$stdntStNm)) * 100

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 막대 그래프
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 재학상태에 따른 빈도분석을 통해 시각화하였다.
# 그 결과 수료, 중퇴, 재학, 휴학의 빈도는 6238, 877, 718, 717 순으로 수료가 가장 많았다.
# 무엇보다도 높은 중퇴 비율은  해외유학, 검정고시, 공무원 등 자발적 의지 학업 중단으로 판단된다.

ggplot(dataL2, aes(x = stdntStNm, fill = stdntStNm)) +
  geom_bar() +
  labs(x = "재학상태", fill = "재학상태", y = "빈도")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 트리맵
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 재학학년에 따른 상대 빈도분석을 통해 시각화하였다.
# 그 결과 3학년, 2학년, 1학년, 4학년, 5학년, 6학년 순으로 상대빈도가 낮았고 특히 고핵년 (5-6학년)의 비율이 가장 낮았다.
# 이는 앞서 설명한 바와 같이 초-중학교 학업 중단 1위인 해외 유학으로 인해 판단된다.

getFreq = as.data.frame(table(dataL2$gradeNm))
getFreq$RelFreq = (getFreq$Freq / sum(getFreq$Freq, na.rm = TRUE)) * 100
colnames(getFreq) = c("재학학년", "빈도", "상대빈도")

treemap::treemap(getFreq, index=c("재학학년"), vSize="상대빈도", vColor="상대빈도", type = "value")

# ******************************************************
# 데이터 분석
# ******************************************************
# 과제 주제에 따라 수집된 데이터를 데이터 분석 기법 (교차분석이나 상관분석)으로 분석
# 서로 다른 3개 (또는 3개 이상의) 가설을 설정하고 각 가설에 대해 분석 (교차분석이나 상관분석)을 수행하며,
# 각 분석 결과가 가지는 의미 설명.

# 교육정보 및 재학상태에 따른 주제
dataL2$hshldRlCd = as.numeric(dataL2$hshldRlCd)
dataL2$edctnInCd = as.numeric(dataL2$edctnInCd)
dataL2$stdntStCd = as.numeric(dataL2$stdntStCd)
dataL2$gradeCd = as.numeric(dataL2$gradeCd)

dataL3 = dataL2[ , c("age", "edctnInCd", "stdntStCd", "gradeCd")]

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 상관분석
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 나이 및 학업 (교육정도, 재학상태, 재학학년) 관련하여 상관분석을 통해 시각화하였다.
# 그 결과 나이가 많을수록 교육정도, 재학상태, 재학학년과의 낮은 상관관계를 갖으나
# 반면에 교육정도와 재학상태의 상관계수는 0.25로서 양의 관계를 지닌다.
# 이는 고학력자일수록 수료-휴학-중퇴할 가능성이 크다는 것을 나타낸다.

# 상관계수  그래프
corrplot::corrplot(cor(dataL3), method = "number")

# 산점도  그래프
pairs(dataL3, panel = panel.smooth)


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
# R을 이용한 파이차트, 그룹형 바차트, 워드클라우드 시각화

#================================================
# 초기 환경변수 설정
#================================================
env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0248"
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
library(RColorBrewer)
library(tidyverse)
library(readr)
library(RmecabKo)
library(stringr)
library(wordcloud2)
library(htmlwidget)

# 명사 추출을 위한 메타 정보
RmecabKo::install_mecab()

# ****************************************
# 파이차트 작성
# ****************************************
carData = read.csv(file = "car.csv")

carDataL1 = subset(carData, 사고유형대분류 %in% c("차대차"))
carDataL2 = subset(carDataL1, select = c(사고유형대분류, 사고유형, 사고건수))

label = paste0(carDataL2$사고유형, " ", round((carDataL2$사고건수 / sum(carDataL2$사고건수, na.rm = TRUE) * 100)), "%")

png(file = "차대차 교통사고 유형별 사고건수.png", width = 10, height = 8, units = "in", res = 600)
pie(carDataL2$사고건수, labels = label, col = RColorBrewer::brewer.pal(5, "Set2"), main = "차대차 교통사고 유형별 사고건수")
dev.off()

# ****************************************
# 바차트 작성
# ****************************************
barData = carData
barData[, "부상자수"] =  barData[, "중상자수"] + barData[, "경상자수"]

tmpData1 = tapply(barData$사망자수, barData$사고유형대분류, FUN=sum)
tmpData2 = tapply(barData$부상자수, barData$사고유형대분류, FUN=sum)
tmpData = data.frame(tmpData1, tmpData2)

barDataL1 = t(data.matrix(tmpData))
row.names(barDataL1) = c("사망자수", "부상자수")

png(file = "사고형대분류통계.png", width = 10, height = 8, units = "in", res = 600)
barplot(barDataL1, main = "사고형대분류통계", xlab="사고유형대분류이름", ylab="사람수"
        , col=RColorBrewer::brewer.pal(3, "Set2"), beside=TRUE, font.axis=2, legend = TRUE)
dev.off()

# ****************************************
# 워드클라우드 작성
# ****************************************
covidData = read.csv(file = "covidnews8.csv")

covidDataTextAll = paste(covidData$본문, collapse = " ")

covidDataL1 = RcppMeCab::pos(utf8::as_utf8(covidDataTextAll), format = "data.frame")
covidDataL2 = subset(covidDataL1, pos %in% c("NNG"))
covidDataL3 = subset(covidDataL2, select = c(token))

# 키워드 빈도에 따른 시각화
covidDataL3$token = replace(covidDataL3$token, stringr::str_detect(covidDataL3$token, regex("코로나")), "코로나19")

keywordData = covidDataL3 %>%
  group_by(token) %>%
  summarise(freq = n()) %>%
  mutate(len = stringr::str_length(token)) %>% 
  filter(
    freq >= 2
    , len >= 2
  ) %>% 
  arrange(desc(freq))

fig = wordcloud2::wordcloud2(data = keywordData)

# html 저장
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# html에서 png로 저장
webshot::webshot("fig.html", "워드클라우드.png", vwidth = 800, vheight = 600, delay = 10)


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
# R을 이용한 지도학습 (선형회귀분석과 정규화, 상호작용 효과, 분류 및 판별 분석)

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0251"
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
library(RColorBrewer)
library(tidyverse)
library(readr)
library(spdep)
library(rgdal)
library(foreign) 
library(car)
library(RColorBrewer)
library(classInt)
library(spatialreg)
library(spdep)
library(rgdal)
library(foreign) 
library(ROCit)
library(klaR)
library(spdep)
library(rgdal)
library(foreign) 
library(ROCit)
library(klaR)
library(rsample)
library(gmodels)
library(caret)

## Mapping function
mapping.seq <- function(polys, x, nclass, main="") {  
  pal.red <- brewer.pal(nclass, "Reds")
  q.n <- classIntervals(x, nclass, style="quantile") 
  cols.red <- findColours(q.n, pal.red)
  plot(polys, col=cols.red)
  brks <- round(q.n$brks,2)
  leg <- paste(brks[-(nclass+1)], brks[-1], sep=" - ")
  legend("bottomright", fill=pal.red, legend=leg, bty="n")
  if (!missing(main)) title(main)
}


## Read Shapefile
sample.shp <- readOGR(dsn = globalVar$inpPath, layer = "Seoul_dong", encoding = 'ESRI Shapefile')
sample.df <- read.dbf(file.path(globalVar$inpPath, 'Seoul_dong.dbf'))
sample.df$Div <- as.factor(sample.df$Div)

# ******************************************************************************
# Part 1: 선형 회귀 분석과 정규화
# ******************************************************************************
# 1번 문제
## Multiple linear regression
mult.lm1 <- lm(Price~M_priv+H_univ+E_prog+Year+Park+Sub+Nurser+Hospit+Culture, data = sample.df)
summary(mult.lm1)
vif(mult.lm1)

# 2번 문제
##autocorrelation
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "잔차의 공간적 자기상관")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

mapping.seq(sample.shp, mult.lm1$residuals, 6, "SA in residuals")
nb <- poly2nb(sample.shp, queen=T)
sample.listw <- nb2listw(nb, style = 'W')
lm.morantest(mult.lm1, sample.listw)

dev.off()

# 3번 문제
mult.lm1 <- lm(Price~M_priv+H_univ+E_prog+Year+Park+Sub+Nurser+Hospit+Culture, data = sample.df)
sample.esf <- MASS::stepAIC(mult.lm1, direction='both')
summary(sample.esf)

# 4번 문제
##
## The Lasso obtained with alpha=1
##
x <- as.matrix(sample.df[,c("Year", "Nurser", "Hospit", "Park",   
                            "Culture", "Sub","H_univ", "M_priv","E_prog")])
y <- sample.df$Price

cv.out <- glmnet::cv.glmnet(x, y, alpha=1, nfolds = 5)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "LASSO 회귀계수의 오차 변화")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(cv.out)

(bestlam <- cv.out$lambda.min)

dev.off()

lasso.mod <- glmnet::glmnet(x, y, alpha=1, lambda=bestlam)
lasso.coef <- predict(lasso.mod, type="coefficients", s=bestlam)


# ******************************************************************************
# Part 2: 상호작용 효과
# ******************************************************************************
# 6번
lmModel = lm(Price ~ Sub)
summary(lmModel)


# 7번
#Adding interaction variable
#prepare Plot
divSymbol <- ifelse(Div==levels(Div)[1],15,16)      # Symbols & colors for well type
divCol <- ifelse(Div==levels(Div)[1],"red","blue")

# Regression with intercept dummy
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "intercept dummy 효과")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

dummy.lm1 <- lm(Price~Sub+Div, data = sample.df)
summary(dummy.lm1)
plot(Sub, Price, pch=divSymbol, col=divCol)
abline(dummy.lm1$coef[1],dummy.lm1$coef[2],col="red")
abline(dummy.lm1$coef[1]+dummy.lm1$coef[3],dummy.lm1$coef[2],col="blue")
legend("topleft",legend=c("그외","강남"), col=c("red","blue"),pch=c(15,16))

dev.off()

# 8번
# Regression with slope dummy
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "slope dummy 효과")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

dummy.lm2 <- lm(Price~Sub+Sub:Div, data = sample.df)
summary(dummy.lm2)
plot(Sub, Price, pch=divSymbol, col=divCol)
abline(dummy.lm2$coef[1],dummy.lm2$coef[2],col="red")
abline(dummy.lm2$coef[1], dummy.lm2$coef[2]+dummy.lm2$coef[3],col="blue")
legend("topleft",legend=c("그외","강남"), col=c("red","blue"),pch=c(15,16))

dev.off()

# 9번
# Regression with both
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "intercept-slope dummy 효과")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

dummy.lm3 <- lm(Price~Sub+Sub*Div, data = sample.df)
summary(dummy.lm3)
plot(Sub, Price, pch=divSymbol, col=divCol)
abline(dummy.lm3$coef[1],dummy.lm3$coef[2],col="red")
abline(dummy.lm3$coef[1]+dummy.lm3$coef[3], dummy.lm3$coef[2]+dummy.lm3$coef[4],col="blue")
legend("topleft",legend=c("그외","강남"), col=c("red","blue"),pch=c(15,16))

dev.off()

# ******************************************************************************
# Part 3: Classification and sampling
# ******************************************************************************

# 10번
set.seed(100)
n <- nrow(sample.df)
sample.idx <-sample(1:n, round(n * 0.7))
train.df <- sample.df[sample.idx, ]
test.df <- sample.df[-sample.idx,]

logit.train <- glm(Div~M_priv+H_univ+Price, data=train.df, family=binomial(link="logit"))
summary(logit.train)

logit.Test <- predict(logit.train, newdata=test.df, type="response")
div.Pred <- ifelse(logit.Test < 0.5, "ETC", "Gangnam")
gmodels::CrossTable(x=test.df$Div, y=div.Pred, prop.r=F, prop.c=F, prop.chisq = FALSE)

# 정확도 (Accuracy) : (TP + TN) / (TN + FP + FN + TP)
# 0.905511811
(100+15)/127

# 민감도 (Sensitivity) : TP / (TP + FN)
# 0.9708737864
(100/103)

# 특이도 (Specificity) :  TN / (TN + FP)
# 0.625
(15/24)

logit.Roc <- rocit(score = logit.Test, class = test.df$Div)
summary(logit.Roc)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Simple Random Sampling")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(logit.Roc)

dev.off()

# 11번
##
## Stratified random sampling
## 
set.seed(100)

sample.idx  <- rsample::initial_split(sample.df, prop = 0.7,strata = "Div")
train.df  <- rsample::training(sample.idx)
test.df   <- rsample::testing(sample.idx)

lda.train <- MASS::lda(Div~M_priv+H_univ+Price, data=train.df)
lda.test <- predict(lda.train, test.df)

gmodels::CrossTable(x=test.df$Div, y=lda.test$class, prop.r=F, prop.c=F, prop.chisq = FALSE)

# 정확도 (Accuracy) : (TP + TN) / (TN + FP + FN + TP)
# 0.8984375
(102+13)/128

# 민감도 (Sensitivity) : TP / (TP + FN)
# 0.9272727273
(102/110)


# 특이도 (Specificity) :  TN / (TN + FP)
# 0.7222222222
(13/18)


lda.Roc <- rocit(score = lda.test$posterior[,2], class = test.df$Div)
summary(lda.Roc)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Stratified Random Sampling")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(lda.Roc)

dev.off()


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
# R을 이용한 퇴직 여부에 따른 로지스틱 회귀분석

# 퇴직 여부 (0, 1)에 영향을 미치는 요인들의 상관관계
# 전체 변수 및 유의미한 변수 (종속변수 : 퇴직여부 / 독립변수 : 퇴직여부 외 변수) 에 대한 로지스틱 회귀분석
# 결과 해석

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0253"
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
library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)
library(ggcorrplot)
library(caret)

# 컴퓨터 내부에서 특정 난수 생성
# 즉 이 코드는 훈련 및 데이터 셋 분할과정에서 사용
set.seed(3)

# *******************************************
# 엑셀 파일 읽기
# *******************************************
# 데이터 설명 :
# 퇴직 : 0 (퇴직), 1 (재직)
# 나이 : 23 ~ 64
# 학력 : 1 (고등학교), 2 (전문대), 3 (4년제), 4 (대학원)
# 국내외.학력 : 1 (국내대학교), 2 (해외대학교)
# 성별 : 1 (남자), 2 (여자)
# 난이도 : 2 ~ 11
# 직급 : 사원 (1), 대리 (2), 과장 (3), 4 (차장), 5 (부장), 6 (이사), 7 (상무), 8 (전무), 9 (부사장)
# 근속년수 : 0 ~ 35.07
# 승진후.지난.시간 : 0 ~ 14.1
# 입사일

# 출처 : 공공데이터포털에서 데이터 융합하여 만든 가상 데이터셋

fileInfo = Sys.glob(file.path(globalVar$inpPath, "LogisticsRegression.xlsx"))
data = xlsx::read.xlsx(fileInfo, sheetName = "in", encoding = "UTF-8")

summary(data)

# NA값을 제거
dataL1 = na.omit(data)

# 자료형 변환 (number > factor)
dataL1$퇴직여부 = factor(dataL1$퇴직여부)

#=====================================================================
# 상관분석 
#=====================================================================
# 상관관계 행렬에서 퇴직 여부를 기준으로
# 음의 관계 (학력, 국내외 학력, 성별, 난이도, 승진후 지난시간)을 보인
# 반면 나이, 직급, 근속년수, 입사일에서는 양의 관계를 보인다.
# 특히 승진후 지난시간, 근속년수, 직급의 경우 유의수준 0.05 이하로서 통계적으로 유의한 결과를 보였다.

# 상관계수
corRes = cor(data)
pvalRes = ggcorrplot::cor_pmat(data)    
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "상관계수 행렬")

ggcorrplot::ggcorrplot(
  corRes
  , outline.col = "white"
  , lab = TRUE
  , p.mat = pvalRes
  , sig.level = 0.05
  , colors = c("#6D9EC1", "white", "#E46726")
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

#=====================================================================
# 유의미 변수 선택
#=====================================================================
# Initial Model:
#     퇴직여부 ~ 나이 + 학력 + 국내외.학력 + 성별 + 난이도 + 직급 +
#     근속년수 + 승진후.지난.시간 + 상사
#
# Final Model:
#     퇴직여부 ~ 나이 + 난이도 + 직급 + 근속년수 + 승진후.지난.시간

# 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 퇴직여부 제외한 전체 변수
# 종속변수 : 퇴적여부
glmFitVarAll = glm(퇴직여부 ~ ., data = dataL1, family = binomial)

# 전체 변수에 대한 요약 결과
summary(glmFitVarAll)

# 1) 기본값으로 변수 선택
# stepRes = step(glmFitVarAll)

# 2) AIC 기준으로 변수 선택
stepAicRes = MASS::stepAIC(glmFitVarAll, direction = "both")

# 유의미한 변수에 대한 요약 결과
summary(stepAicRes)

# 한 눈에 분석 결과 확인 가능
stepAicRes$anova

#=====================================================================
# 훈련 및 테스트 셋 설정 (60 : 40)
#=====================================================================
# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.6)

# 해당 인덱스에 따라 자료 할당
trainData = dataL1[ind, ]
testData = dataL1[-ind, ]

# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)

#=====================================================================
# 전체 변수
# 훈련 데이터를 이용한 회귀모형 학습
# 테스트 데이터를 이용한 검증 수행
#=====================================================================
# 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 퇴직여부 제외한 전체 변수
# 종속변수 : 퇴적여부
glmFit = glm(퇴직여부 ~ ., data = trainData, family = binomial)

# 회귀모형에 대한 요약 결과
summary(glmFit)

# 실제 퇴직여부
yObs = as.numeric(as.character(testData$퇴직여부))

# 테스트셋을 이용한 예측 퇴직여부
yHat = predict.glm(glmFit, newdata = testData, type = "response")

# 카테고리형 정확도 측정
yHatYn = ifelse(yHat > 0.5, 1, 0)
conMatRes = caret::confusionMatrix(data = factor(yHatYn), reference = factor(yObs))
# 정확도 : 0.809
conMatRes$overall["Accuracy"] %>% round(4)

# 민감도 : 0.6296 
conMatRes$byClass["Sensitivity"] %>% round(4)

# 특이도 : 0.8871 
conMatRes$byClass["Specificity"] %>% round(4)

# ROC 커브를 위한 설정
logitRoc = ROCit::rocit(score = yHatYn, class = yObs)

# 요약 결과
# summary(logitRoc)

mainTitle = "ROC 곡선-전체 변수"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(logitRoc, main = mainTitle)

dev.off()

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.81
logitRoc$AUC

# 이항편차 측정 : 낮을수록 좋음 : 20.89
abdiv::binomial_deviance(yObs, yHat)


#=====================================================================
# 유의미한 변수
# 훈련 데이터를 이용한 회귀모형 학습
# 테스트 데이터를 이용한 검증 수행
#=====================================================================
# 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 나이, 직급, 근속년수, 승진후.지난.시간
# 종속변수 : 퇴적여부
glmFitSel = glm(퇴직여부 ~ 나이 + 직급 + 근속년수 + 승진후.지난.시간, data = trainData, family = binomial)

# 실제 퇴직여부
yObs = as.numeric(as.character(testData$퇴직여부))

# 테스트셋을 이용한 예측 퇴직여부
yHat = predict.glm(glmFitSel, newdata = testData, type = "response")

# 카테고리형 정확도 측정
yHatYn = ifelse(yHat > 0.5, 1, 0)
conMatRes = caret::confusionMatrix(data = factor(yHatYn), reference = factor(yObs))
# 정확도 : 0.809
conMatRes$overall["Accuracy"] %>% round(4)

# 민감도 : 0.6296 
conMatRes$byClass["Sensitivity"] %>% round(4)

# 특이도 : 0.8871 
conMatRes$byClass["Specificity"] %>% round(4)

# ROC 커브를 위한 설정
logitRoc = ROCit::rocit(score = yHat, class = yObs)

# 요약 결과
# summary(logitRoc)

mainTitle = "ROC 곡선-유의미한 변수"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(logitRoc, main = mainTitle)

dev.off()

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.84
logitRoc$AUC

# 이항편차 측정 : 낮을수록 좋음 : 20.89
abdiv::binomial_deviance(yObs, yHat)


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
# R을 이용한 2019-2020 한국복지패널 데이터에 대한 막대 그래프 시각화

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0256"
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
library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)
library(ggcorrplot)
library(caret)
library(haven)

# ******************************************************************************
# 참조 테이블
# ******************************************************************************
xlsxFileInfo = Sys.glob(file.path(globalVar$inpPath, "(2019년 14차 한국복지패널조사) 조사설계서-가구용(beta2).xlsx"))
xlsxData = openxlsx::read.xlsx(xlsxFileInfo, sheet = 4)

xlsxDataL1 = xlsxData %>% 
  dplyr::select(소분류, X4) %>% 
  dplyr::rename(
    code = 소분류
    , name = X4
  ) %>% 
  dplyr::mutate_at(vars(code), funs(as.numeric))


# ******************************************************************************
# 2019년 한국복지패널조사
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, "Koweps_h14_2019_beta2.sav"))
data = haven::read_sav(fileInfo)

dataL1 = data %>% 
  dplyr::select(h1401_4, h1403_8) %>% 
  dplyr::filter(
    ! is.na(h1401_4)
    , ! is.na(h1403_8)
  ) %>% 
  dplyr::left_join(xlsxDataL1, by = c("h1403_8" = "code")) %>% 
  dplyr::mutate(
    sex = dplyr::case_when(
      h1401_4 == 1 ~ "남"
      , h1401_4 == 2 ~ "여"
    )
  )

dataL2 = dataL1 %>% 
  dplyr::group_by(sex, name) %>% 
  dplyr::summarise(cnt = n()) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  dplyr::slice(1:10)

sexList = dataL2$sex %>% unique() %>% sort()

# sexInfo = "남"
for (sexInfo in sexList) {
  
  dataL3 = dataL2 %>% 
    dplyr::filter(sex == sexInfo)
  
  dataL3$name = forcats::fct_relevel(dataL3$name, rev(dataL3$name))
  
  plotSubTitle = sprintf("%s %s %s", "2019년", sexInfo, "직업빈도 상위 10개")
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
  
  makePlot = ggplot(dataL3, aes(x = as.factor(name), y = cnt, fill = cnt)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = cnt), hjust = 1.5, vjust = 0.5, color = "white", size = 4) +
    coord_flip() +
    labs(x = "직종", y = "빈도수", fill = "빈도", title = plotSubTitle)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
}


# ******************************************************************************
# 2020년 한국복지패널조사
# ******************************************************************************
fileInfo2 = Sys.glob(file.path(globalVar$inpPath, "Koweps_h15_2020_beta1.sav"))
data2 = haven::read_sav(fileInfo2)

data2L1 = data2 %>% 
  dplyr::select(h1501_4, h1503_8) %>% 
  dplyr::filter(
    ! is.na(h1501_4)
    , ! is.na(h1503_8)
  ) %>% 
  dplyr::left_join(xlsxDataL1, by = c("h1503_8" = "code")) %>% 
  dplyr::mutate(
    sex = dplyr::case_when(
      h1501_4 == 1 ~ "남"
      , h1501_4 == 2 ~ "여"
    )
  )

data2L2 = data2L1 %>% 
  dplyr::group_by(sex, name) %>% 
  dplyr::summarise(cnt = n()) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  dplyr::slice(1:10)

sexList = data2L2$sex %>% unique() %>% sort()

# sexInfo = "남"
for (sexInfo in sexList) {
  
  data2L3 = data2L2 %>% 
    dplyr::filter(sex == sexInfo)
  
  data2L3$name = forcats::fct_relevel(data2L3$name, rev(data2L3$name))
  
  plotSubTitle = sprintf("%s %s %s", "2020년", sexInfo, "직업빈도 상위 10개")
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
  
  makePlot = ggplot(data2L3, aes(x = as.factor(name), y = cnt, fill = cnt)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = cnt), hjust = 1.5, vjust = 0.5, color = "white", size = 4) +
    coord_flip() +
    labs(x = "직종", y = "빈도수", fill = "빈도", title = plotSubTitle)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
}


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
# R을 이용한 2016-2018 한국복지패널 데이터에 대한 막대 그래프 시각화

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0258"
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
library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)
library(ggcorrplot)
library(caret)
library(haven)

# ******************************************************************************
# 참조 테이블
# ******************************************************************************
xlsxFileInfo = Sys.glob(file.path(globalVar$inpPath, "(2016년 11차 한국복지패널조사) 조사설계서-가구용(beta5).xlsx"))
xlsxData = openxlsx::read.xlsx(xlsxFileInfo, sheet = 4)

xlsxDataL1 = xlsxData %>% 
  dplyr::select(소분류) %>% 
  tidyr::separate(col = 소분류, into = c("code", "name"), sep = ' ') %>%
  dplyr::mutate(
    name = gsub("[[:punct:]]", "", name)
  ) %>% 
  dplyr::mutate_at(vars(code), funs(as.numeric))

# ******************************************************************************
# 2016년 한국복지패널조사
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, "Koweps_h11_2016_beta5.sav"))
data = haven::read_sav(fileInfo)

colList = colnames(data)

for (colInfo in colList) {
  cat(attr(get(colInfo, data), 'label'), "\n")
}

dataL1 = data %>% 
  dplyr::select(h1103_8, h1101_4) %>% 
  dplyr::filter(
    ! is.na(h1103_8)
    , ! is.na(h1101_4)
  ) %>% 
  dplyr::left_join(xlsxDataL1, by = c("h1103_8" = "code")) %>% 
  dplyr::mutate(
    sex = dplyr::case_when(
      h1101_4 == 1 ~ "남"
      , h1101_4 == 2 ~ "여"
    )
  )

dataL2 = dataL1 %>% 
  dplyr::group_by(sex, name) %>% 
  dplyr::summarise(cnt = n()) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  dplyr::slice(1:10)

sexList = dataL2$sex %>% unique() %>% sort()

# sexInfo = "남"
for (sexInfo in sexList) {
  
  dataL3 = dataL2 %>% 
    dplyr::filter(sex == sexInfo)
  
  dataL3$name = forcats::fct_relevel(dataL3$name, rev(dataL3$name))
  
  plotSubTitle = sprintf("%s %s %s", "2016년", sexInfo, "직업빈도 상위 10개")
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
  
  makePlot = ggplot(dataL3, aes(x = as.factor(name), y = cnt, fill = cnt)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = cnt), hjust = 1.5, vjust = 0.5, color = "white", size = 4) +
    coord_flip() +
    labs(x = "직종", y = "빈도수", fill = "빈도", title = plotSubTitle)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
}


# ******************************************************************************
# 2017년 한국복지패널조사
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, "Koweps_h12_2017_beta4.sav"))
data = haven::read_sav(fileInfo)

colList = colnames(data)

for (colInfo in colList) {
  cat(attr(get(colInfo, data), 'label'), "\n")
}

dataL1 = data %>% 
  dplyr::select(h1203_8, h1201_4) %>% 
  dplyr::filter(
    ! is.na(h1203_8)
    , ! is.na(h1201_4)
  ) %>% 
  dplyr::left_join(xlsxDataL1, by = c("h1203_8" = "code")) %>% 
  dplyr::mutate(
    sex = dplyr::case_when(
      h1201_4 == 1 ~ "남"
      , h1201_4 == 2 ~ "여"
    )
  )

dataL2 = dataL1 %>% 
  dplyr::group_by(sex, name) %>% 
  dplyr::summarise(cnt = n()) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  dplyr::slice(1:10)

sexList = dataL2$sex %>% unique() %>% sort()

# sexInfo = "남"
for (sexInfo in sexList) {
  
  dataL3 = dataL2 %>% 
    dplyr::filter(sex == sexInfo)
  
  dataL3$name = forcats::fct_relevel(dataL3$name, rev(dataL3$name))
  
  plotSubTitle = sprintf("%s %s %s", "2017년", sexInfo, "직업빈도 상위 10개")
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
  
  makePlot = ggplot(dataL3, aes(x = as.factor(name), y = cnt, fill = cnt)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = cnt), hjust = 1.5, vjust = 0.5, color = "white", size = 4) +
    coord_flip() +
    labs(x = "직종", y = "빈도수", fill = "빈도", title = plotSubTitle)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
}


# ******************************************************************************
# 2018년 한국복지패널조사
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, "Koweps_h13_2018_beta3.sav"))
data = haven::read_sav(fileInfo)

colList = colnames(data)

for (colInfo in colList) {
  cat(attr(get(colInfo, data), 'label'), "\n")
}

dataL1 = data %>% 
  dplyr::select(h1303_8, h1301_4) %>% 
  dplyr::filter(
    ! is.na(h1303_8)
    , ! is.na(h1301_4)
  ) %>% 
  dplyr::left_join(xlsxDataL1, by = c("h1303_8" = "code")) %>% 
  dplyr::mutate(
    sex = dplyr::case_when(
      h1301_4 == 1 ~ "남"
      , h1301_4 == 2 ~ "여"
    )
  )

dataL2 = dataL1 %>% 
  dplyr::group_by(sex, name) %>% 
  dplyr::summarise(cnt = n()) %>% 
  dplyr::arrange(desc(cnt)) %>% 
  dplyr::slice(1:10)

sexList = dataL2$sex %>% unique() %>% sort()

# sexInfo = "남"
for (sexInfo in sexList) {
  
  dataL3 = dataL2 %>% 
    dplyr::filter(sex == sexInfo)
  
  dataL3$name = forcats::fct_relevel(dataL3$name, rev(dataL3$name))
  
  plotSubTitle = sprintf("%s %s %s", "2018년", sexInfo, "직업빈도 상위 10개")
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
  
  makePlot = ggplot(dataL3, aes(x = as.factor(name), y = cnt, fill = cnt)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = cnt), hjust = 1.5, vjust = 0.5, color = "white", size = 4) +
    coord_flip() +
    labs(x = "직종", y = "빈도수", fill = "빈도", title = plotSubTitle)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
}


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
# R을 이용한 말랭이 처리조건별, 시간에 따른 품질특성 분석

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0262"
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
library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)
library(ggcorrplot)
library(caret)
library(haven)
library(laercio)
library(agricolae)

#===============================================================================
# 1. 처리조건에 따른 품질 특성 (적색도, 수분함량)
#===============================================================================
# 엑셀 파일 읽기
xlsxFileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "rawdata.xlsx"))
data = openxlsx::read.xlsx(xlsxFileInfo, sheet = 3)

dataL1 = data %>% 
  readr::type_convert()

dataL1$type = as.factor(dataL1$type)
dataL1$proc = as.factor(dataL1$proc)

summary(dataL1)


# ******************************************************************************
# 1.가. [적색도, 수분함량] 처리조건에 따른 품질 특성
# ******************************************************************************
# anova 검정 시 집단 간 평균에 유의미한 차이 분석
aovModel = aov(a ~ type + proc, data = dataL1)

# 그 결과 유의 수준 (Pr(>F))이 0.05 이하로서 집단 간의 평균 차이가 있음 (대립가설 채택)
summary(aovModel)

# Df   Sum Sq   Mean Sq F value    Pr(>F)   
# type         8 160.6901 20.086261 2.94534 0.0066241 **
#   Residuals   72 491.0165  6.819674                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 추가로 사후분석 (Duncan's LSR test)을 통해 어느 집단 간에 차이가 있는가를 분석

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [적색도] 처리조건에 따른 품질 특성 (type, proc)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type", "proc"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[적색도] 처리조건에 따른 품질 특성 (type, proc)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# 9개 그룹
result$groups %>% 
  as.tibble() %>% 
  dplyr::select(groups) %>% 
  unique() %>% 
  nrow()

# Means with the same letter are not significantly different.
# 
# a groups
# G5%+S1% 3시간:건조 후          20.05333333      a
# G5%+S1% 30분:건조 후           19.48000000     ab
# G5%+S1% 2시간:냉동보관 30일 후 18.88666667    abc
# 무처리:냉동보관 30일 후        18.38666667   abcd
# G5%+S1% 1시간:건조 후          18.35333333   abcd
# G5%+S1% 3시간:냉동보관 30일 후 17.99000000   abcd
# G5%+S1% 1시간:냉동보관 30일 후 17.98333333   abcd
# G5%+S1% 2시간:건조 후          15.86666667  abcde
# S1% 2시간:냉동보관 30일 후     15.40000000  abcde
# S1% 3시간:냉동보관 30일 후     15.27000000  abcde
# S1% 30분:냉동보관 30일 후      15.02333333   bcde
# G5%+S1% 1시간:침지 전          14.16333333    cde
# G5%+S1% 2시간:침지 전          14.16333333    cde
# G5%+S1% 30분:침지 전           14.16333333    cde
# G5%+S1% 3시간:침지 전          14.16333333    cde
# S1% 1시간:침지 전              14.16333333    cde
# S1% 2시간:침지 전              14.16333333    cde
# S1% 30분:침지 전               14.16333333    cde
# S1% 3시간:침지 전              14.16333333    cde
# 무처리:침지 전                 14.16333333    cde
# S1% 2시간:건조 후              13.51666667     de
# S1% 3시간:건조 후              13.47666667     de
# S1% 30분:건조 후               13.43000000     de
# G5%+S1% 30분:냉동보관 30일 후  13.37666667     de
# S1% 1시간:냉동보관 30일 후     12.98000000      e
# 무처리:건조 후                 12.63333333      e
# S1% 1시간:건조 후              10.88333333      e

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [적색도] 처리조건에 따른 품질 특성 (type)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[적색도] 처리조건에 따른 품질 특성 (type)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# a groups
# G5%+S1% 3시간 17.40222222      a
# G5%+S1% 1시간 16.83333333     ab
# G5%+S1% 2시간 16.30555556     ab
# G5%+S1% 30분  15.67333333     ab
# 무처리        15.06111111    abc
# S1% 2시간     14.36000000     bc
# S1% 3시간     14.30333333     bc
# S1% 30분      14.20555556     bc
# S1% 1시간     12.67555556      c

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [적색도] 처리조건에 따른 품질 특성 (proc)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("proc"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[적색도] 처리조건에 따른 품질 특성 (proc)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# a groups
# 냉동보관 30일 후 16.14407407      a
# 건조 후          15.29925926     ab
# 침지 전          14.16333333      b


# ******************************************************************************
# 1.나. [수분함량] 처리조건에 따른 품질 특성
# ******************************************************************************
# anova 검정 시 집단 간 평균에 유의미한 차이 분석
aovModel = aov(water ~ type + proc, data = dataL1)

# 그 결과 유의 수준 (Pr(>F))이 0.05 이하로서 집단 간의 평균 차이가 있음 (대립가설 채택)
summary(aovModel)

# 추가로 사후분석 (Duncan's LSR test)을 통해 어느 집단 간에 차이가 있는가를 분석

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [수분함량] 처리조건에 따른 품질 특성 (type, proc)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type", "proc"), alpha = 0.05, console = TRUE)

# 8개 그룹
result$groups %>% 
  as.tibble() %>% 
  dplyr::select(groups) %>% 
  unique() %>% 
  nrow()

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[수분함량] 처리조건에 따른 품질 특성 (type, proc)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# water groups
# S1% 3시간:침지 전              85.70000000      a
# G5%+S1% 2시간:침지 전          85.20000000      a
# S1% 2시간:침지 전              85.20000000      a
# G5%+S1% 3시간:침지 전          85.00000000      a
# S1% 1시간:침지 전              85.00000000      a
# G5%+S1% 30분:침지 전           84.40000000      a
# S1% 30분:침지 전               83.40000000      a
# 무처리:침지 전                 81.73333333      a
# G5%+S1% 1시간:침지 전          81.70000000      a
# G5%+S1% 2시간:건조 후          20.30000000      b
# G5%+S1% 1시간:냉동보관 30일 후 19.80000000     bc
# G5%+S1% 2시간:냉동보관 30일 후 19.40000000    bcd
# G5%+S1% 3시간:냉동보관 30일 후 19.30000000    bcd
# G5%+S1% 30분:냉동보관 30일 후  19.10000000    bcd
# G5%+S1% 30분:건조 후           19.00000000    bcd
# G5%+S1% 1시간:건조 후          18.10000000    bcd
# S1% 30분:건조 후               18.00000000    bcd
# G5%+S1% 3시간:건조 후          17.70000000    bcd
# S1% 3시간:건조 후              17.70000000    bcd
# S1% 2시간:냉동보관 30일 후     17.30000000    bcd
# S1% 1시간:건조 후              16.80000000    bcd
# S1% 1시간:냉동보관 30일 후     16.80000000    bcd
# S1% 3시간:냉동보관 30일 후     16.20000000   bcde
# S1% 30분:냉동보관 30일 후      16.00000000   bcde
# S1% 2시간:건조 후              14.90000000    cde
# 무처리:건조 후                 14.70000000     de
# 무처리:냉동보관 30일 후        11.90000000      e

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [수분함량] 처리조건에 따른 품질 특성 (type)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[수분함량] 처리조건에 따른 품질 특성 (type)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# water groups
# 무처리        54.36000000      a
# G5%+S1% 2시간 41.63333333      b
# G5%+S1% 30분  40.83333333      b
# G5%+S1% 3시간 40.66666667      b
# G5%+S1% 1시간 39.86666667      b
# S1% 3시간     39.86666667      b
# S1% 1시간     39.53333333      b
# S1% 2시간     39.13333333      b
# S1% 30분      39.13333333      b

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [수분함량] 처리조건에 따른 품질 특성 (proc)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("proc"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[수분함량] 처리조건에 따른 품질 특성 (proc)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# water groups
# 침지 전          83.70909091      a
# 건조 후          17.46666667      b
# 냉동보관 30일 후 17.31111111      b




#===============================================================================
# 2. [적색도, 수분함량] 과육크기에 따른 품질 특성
#===============================================================================
# 엑셀 파일 읽기
data = openxlsx::read.xlsx(xlsxFileInfo, sheet = 4)

dataL1 = data %>% 
  readr::type_convert()

dataL1$type = as.factor(dataL1$type)
dataL1$type2 = as.factor(dataL1$type2)
dataL1$proc = as.factor(dataL1$proc)

summary(dataL1)

# ******************************************************************************
# 2.가. [적색도] 과육크기에 따른 품질 특성
# ******************************************************************************
# anova 검정 시 집단 간 평균에 유의미한 차이 분석
aovModel = aov(a ~ type + type2 + proc, data = dataL1)

# 그 결과 유의 수준 (Pr(>F))이 0.05 이하로서 집단 간의 평균 차이가 있음 (대립가설 채택)
summary(aovModel)

# 추가로 사후분석 (Duncan's LSR test)을 통해 어느 집단 간에 차이가 있는가를 분석

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [적색도] 과육크기에 따른 품질 특성 (type, type2, proc)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type", "type2", "proc"), alpha = 0.05, console = TRUE)

# 12개 그룹
result$groups %>% 
  as.tibble() %>% 
  dplyr::select(groups) %>% 
  unique() %>% 
  nrow()


saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[적색도] 과육크기에 따른 품질 특성 (type, type2, proc)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# a groups
# G5%:4조각:건조 후                   25.43666667      a
# G5%:4조각:냉동보관 30일 후          25.28000000      a
# G10%+S1%:4조각:건조 후              23.90333333      a
# G10%+S1%:4조각:냉동보관 30일 후     23.75666667      a
# G10%:4조각:냉동보관 30일 후         23.57666667      a
# G10%:4조각:건조 후                  22.89333333      a
# 무처리:4조각:건조 후                22.41666667     ab
# 무처리:4조각:냉동보관 30일 후       22.14000000    abc
# G5%+S1%:4조각:건조 후               21.70000000   abcd
# G5%+S1%:4조각:냉동보관 30일 후      20.93666667  abcde
# G10%:4조각:침지 전                  20.22666667 abcdef
# G10%+S1%:4조각:침지 전              20.22666667 abcdef
# G5%:4조각:침지 전                   20.22666667 abcdef
# G5%+S1%:4조각:침지 전               20.22666667 abcdef
# 무처리:4조각:침지 전                20.22666667 abcdef
# 무처리+S1%:4조각:침지 전            20.22666667 abcdef
# 무처리+S1%:4조각:건조 후            17.53000000 bcdefg
# G5%:깍두기 모양:건조 후             16.94333333  cdefg
# G5%:깍두기 모양:냉동보관 30일 후    16.71666667   defg
# G10%:깍두기 모양:냉동보관 30일 후   15.85333333    efg
# G10%:깍두기 모양:건조 후            15.55666667     fg
# 무처리:깍두기 모양:건조 후          14.53000000      g
# 무처리:깍두기 모양:냉동보관 30일 후 14.35666667      g
# 무처리:깍두기 모양:침지 전          13.66666667      g
# 무처리+S1%:4조각:냉동보관 30일 후   13.15666667      g

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [적색도] 과육크기에 따른 품질 특성 (type)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[적색도] 과육크기에 따른 품질 특성 (type)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()


# Means with the same letter are not significantly different.
# 
# a groups
# G10%+S1%   22.62888889      a
# G5%+S1%    20.95444444     ab
# G5%        20.92066667     ab
# G10%       19.62133333     bc
# 무처리     17.88944444     cd
# 무처리+S1% 16.97111111      d

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [적색도] 과육크기에 따른 품질 특성 (type2)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type2"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[적색도] 과육크기에 따른 품질 특성 (type2)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()


# Means with the same letter are not significantly different.
# 
# a groups
# 4조각       21.33814815      a
# 깍두기 모양 15.37476190      b

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [적색도] 과육크기에 따른 품질 특성 (proc)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("proc"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[적색도] 과육크기에 따른 품질 특성 (proc)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# a groups
# 건조 후          20.10111111      a
# 냉동보관 30일 후 19.53037037      a
# 침지 전          19.28952381      a



# ******************************************************************************
# 2.나. [수분함량] 과육크기에 따른 품질 특성
# ******************************************************************************
# anova 검정 시 집단 간 평균에 유의미한 차이 분석
aovModel = aov(water ~ type + type2, data = dataL1)

# 그 결과 유의 수준 (Pr(>F))이 0.05 이하로서 집단 간의 평균 차이가 있음 (대립가설 채택)
summary(aovModel)

# 추가로 사후분석 (Duncan's LSR test)을 통해 어느 집단 간에 차이가 있는가를 분석

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [수분함량] 과육크기에 따른 품질 특성 (type, type2)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type", "type2"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[수분함량] 과육크기에 따른 품질 특성 (type, type2)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# water groups
# G10%+S1%:4조각      27.4      a
# G5%+S1%:4조각       26.6     ab
# G10%:4조각          25.5    abc
# G5%:4조각           25.1    abc
# 무처리:4조각        23.5   abcd
# G10%:깍두기 모양    22.7   bcde
# G5%:깍두기 모양     21.5    cde
# 무처리+S1%:4조각    20.3     de
# 무처리:깍두기 모양  18.4      e

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [수분함량] 과육크기에 따른 품질 특성 (type)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[수분함량] 과육크기에 따른 품질 특성 (type)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# Means with the same letter are not significantly different.
# 
# water groups
# G10%+S1%   27.40      a
# G5%+S1%    26.60     ab
# G10%       24.10    abc
# G5%        23.30     bc
# 무처리     20.95      c
# 무처리+S1% 20.30      c

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# [수분함량] 과육크기에 따른 품질 특성 (type2)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
result = agricolae::duncan.test(aovModel, c("type2"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "[수분함량] 과육크기에 따른 품질 특성 (type2)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()


# Means with the same letter are not significantly different.
# 
# water groups
# 4조각       24.73333333      a
# 깍두기 모양 20.86666667      b


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
# R을 이용한 수도권 지하철 CCTV 설치 현황 및 성범죄 신고현황 시각화

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0263"
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
library(xlsx)
library(tidyverse)

#===============================================================================
# 수도권 지하철 CCTV 설치 현황
#===============================================================================
# 엑셀 파일 읽기
xlsxFileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0263_수도권 지하철 CCTV 설치 현황 및 성범죄 신고현황에 대한 데이터.xlsx"))
data = openxlsx::read.xlsx(xlsxFileInfo, sheet = 1)

dataL1 = data %>% 
  as.tibble() %>% 
  readr::type_convert() %>% 
  dplyr::mutate_at(vars(type), funs(as.factor)) %>% 
  dplyr::arrange(rat)

# type 정렬
dataL1$type = forcats::fct_relevel(dataL1$type, rev(as.character(dataL1$type)))

plotSubTitle = sprintf("%s", "수도권 지하철 CCTV 설치 현황")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(dataL1, aes(x = type, y = rat, fill = type, label = round(rat, 2))) +
  geom_bar(stat = "identity") +
  geom_text(hjust = 0.5, vjust = 1.25, color = "white", size = 5) +
  labs(x = "지하철 호선", y = "비율", fill = "비율", title = plotSubTitle) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

#===============================================================================
# 성범죄 신고현황
#===============================================================================
# 엑셀 파일 읽기
xlsxFileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0263_수도권 지하철 CCTV 설치 현황 및 성범죄 신고현황에 대한 데이터.xlsx"))
data = openxlsx::read.xlsx(xlsxFileInfo, sheet = 2)

dataL1 = data %>% 
  as.tibble() %>% 
  readr::type_convert() %>% 
  dplyr::mutate_at(vars(type), funs(as.factor)) %>% 
  dplyr::arrange(rat)

# type 정렬
dataL1$type = forcats::fct_relevel(dataL1$type, rev(as.character(dataL1$type)))

plotSubTitle = sprintf("%s", "성범죄 신고현황")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(dataL1, aes(x = year, y = rat, fill = type, label = round(rat, 2))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(hjust = 0.5, vjust = 1.25, color = "white", size = 5) +
  ylim(0, 100) +
  labs(x = "성범죄 연도", y = "비율", fill = "비율", title = plotSubTitle) +
  facet_wrap(~ type, scale = "free_x") +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)