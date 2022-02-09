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

a= 10
plot(a)
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
# R을 이용한 지도학습 (일반화가법모형, 의사결정나무, 앙상블)

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0266"
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
library(ISLR)
library(splines)
library(gam)
library(akima)
library(RColorBrewer)
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
library(tree)
library(MASS)
library(randomForest)
library(gbm)
library(BART)

## shp 파일 읽기
mapShp = readOGR(dsn = globalVar$inpPath, layer = "Seoul_dong", encoding = 'ESRI Shapefile')
mapData = read.dbf(file.path(globalVar$inpPath, 'Seoul_dong.dbf'))
mapData$Div = as.factor(mapData$Div)

# Mapping function
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

#===============================================================================
# Part 1: Generalized Additive Models (GAM)
#===============================================================================

# ******************************************************************************
# 1. 아래의 GAM 모형(e.g., gam.m1)을 추정하시오. 
# 아래와 같이 모든 변수는 Smoothing spline으로 변환하여 추정하시오. 
# 종속변수와 독립변수 간의 관계를 도표화하고, 그 결과를 해석하시오.
# ******************************************************************************
# 모수 효과의 경우 대부분 독립변수는 유의수준 0.01 이하에 만족하나
# 일부 변수 (Park, Culture)는 통계적으로 유의하지 못한 결과를 보인다.

# 그리고 비모수 효과의 경우 대부분 독립변수는 비선형 관계 (유의수준 0.01 이하)를 만족하나
# 일부 변수 (Park, Sub)는 선형 관계 (통계적으로 유의하지 못한 결과)를 띤다. 
# 즉 유의수준 (Pr(F))가 0.05 이하일 경우 대립가설 기각하기 때문에 비선형 관계를 띠고
# 그 반대의 경우 귀무가설 채택하므로 선형 관게를 의미한다.

gamModelFor = formula(Price ~ s(M_priv)+s(H_univ)+s(E_prog)+s(Year)+s(Park)+s(Sub)+s(Nurser)+s(Hospit)+s(Culture))
gam.m1 <- gam::gam(gamModelFor, data = mapData)
summary(gam.m1)

# ******************************************************************************
# 2. 위 모형에서 가장 설명력이 낮다고 판단되는 변수 2개를 제외하고 
# GAM 모형(e.g., gam.m2)을 다시 추정하시오. 해당 변수 2개를 제외한 근거는 무엇인가? 
# 1에서 생성한 모형과 2에서 생성한 모형은 유의미한 차이를 보이는가?
# ******************************************************************************
# 앞선 gam.m1에서 모수 효과의 경우 대부분 독립변수는 유의수준 0.01 이하에 만족하나
# 일부 변수 (Park, Culture)는 통계적으로 유의하지 못한 결과를 보인다.

# 분산표에 의하면 유의수준 0.05 이하로서 유의한 차이를 보인다.

gamModelFor2 = formula(Price ~ s(M_priv)+s(H_univ)+s(E_prog)+s(Year)+s(Sub)+s(Nurser)+s(Hospit))
gam.m2 <- gam::gam(gamModelFor2, data = mapData)
summary(gam.m2)

anova(gam.m1, gam.m2, test="F")

# ******************************************************************************
# 3. 1에서 생성된 모형 중 Smoothing splines으로 변환할 필요가 없는 변수는 
# 무엇이며 그 근거는 무엇인가? 
# 해당 변수를 Smoothing splines로 변형하지 않고 GAM 모형(e.g., gam.m3)을 추정하시오. 
# 1에서 생성한 모형과 3에서 생성한 모형 간 유의미한 차이 존재하는가? 
# ******************************************************************************
# 앞선 gam.m1에서 비모수 효과의 경우 대부분 독립변수는 비선형 관계 (유의수준 0.01 이하)를 만족하나
# 일부 변수 (Park, Sub)는 선형 관계 (통계적으로 유의하지 못한 결과)를 띤다. 
# 즉 유의수준 (Pr(F))가 0.05 이하일 경우 대립가설 기각하기 때문에 비선형 관계를 띠고
# 그 반대의 경우 귀무가설 채택하므로 선형 관게를 의미한다.

# 분산표에 의하면 유의수준 0.05 이상로서 차이가 없다.

gamModelFor3 = formula(Price ~ s(M_priv)+s(H_univ)+s(E_prog)+s(Year)+Park+Sub+s(Nurser)+s(Hospit)+s(Culture))
gam.m3 <- gam::gam(gamModelFor3, data = mapData)
summary(gam.m3)

anova(gam.m1, gam.m3, test="F")

# ******************************************************************************
# 4. 3에서 추정한 모형(e.g., gam.m3)의 잔차를 지도화하고, 
# 공간적 자기상관(Moran’s I)를 값을 추정하시오. 해당 잔차는 공간적 자기상관을 보이는가? 
# 특히 과소 추정된 지역은 어디인지 확인하고, 어떤 변수를 추가로 활용할 수 있을지 추론하시오. 
# ******************************************************************************
# 양의 공간적 자기상관
# Moran I statistic : 0.5877 (p-value = 2.2204e-16)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "잔차의 공간적 자기상관")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

mapping.seq(mapShp, gam.m3$residuals, 6, "gam.m3 in residuals")
nb <- poly2nb(mapShp, queen=T)
sample.listw <- nb2listw(nb, style = 'W')
lm.morantest(gam.m3, sample.listw)

dev.off()

#===============================================================================
# Part 2: Decision trees and Ensemble methods
#===============================================================================
# 자료는 Train과 Test 셋으로 구분하고, Train에 1,000개의 관측치, 
# 그리고 test에 나머지의 관측지 수를 활용할 것이다. 
# Simple Random Sampling(base R 기반)을 활용하고 
# 채점 목적으로 seed 값은 1 (i.e., set.seed(1))로 유지한다.  

set.seed(1)

seoulFor = formula(Price ~  M_priv+H_univ+E_prog+Year+Park+Sub+Nurser+Hospit+Culture)

idx <-sample(1:nrow(mapData), 200) 
train.df <- mapData[idx, ]
test.df <- mapData[-idx, ]

# ******************************************************************************
# 5. 아래의 모형에 대한 Regression tree를 추정하고 그 결과를 시각화하시오.
# ******************************************************************************
tree.seoul <- tree::tree(seoulFor, data=train.df)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "회귀 트리")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(tree.seoul)
text(tree.seoul, pretty = 0)

dev.off()

# ******************************************************************************
# 6. Regression tree를 pruning 한다. 가장 적합한 terminal node의 수는 얼마인가? 
# 결과를 해석하고, test MSE를 제시하여라.
# ******************************************************************************
# 회귀 트리 결과 총 9개 변수 중에서 6개 변수 (H_univ, Nurser, E_prog, Sub, M_priv, Year)를 사용하여 의사결정을 수행하였다. 
# 특히 아파트 매매가격 추정에 있어서 보육시설 접근성 (Nurser), 대학 진학률 (H_Univ) 순으로 높았다. 
# 이에 학습모형을 통해 예측 성능은 42655.32의 MSE 오차를 보인다.
# terminal node : 13
# test MSE : 42655.32

summary(tree.seoul)

prune.seoul <- prune.tree(tree.seoul, best = 13)
yhat <- predict(prune.seoul, newdata = test.df)

# test MSE
mean((yhat - test.df$Price)^2)

# ******************************************************************************
# 7. 앞의 Regression tree에 bagging을 적용한다. 
# 아파트 매매가격 추정에 가장 중요한 변수는 무엇인지를 도표와 함께 설명하시오. 
# 그리고 test MSE를 제시하시오.
# ******************************************************************************
# 랜덤포레스트 결과 아파트 매매가격 추정에 있어서 보육시설 접근성(Nurser) 및 대학 진학률(H_Univ) 순으로 높았다. 
# 이에 학습모형을 통해 예측 성능은 30761.15의 MSE 오차를 보인다.
# test MSE : 30761.15

set.seed(1)

bag.seoul <- randomForest(seoulFor, data = train.df, mtry = 13, importance = TRUE)
yhat.bag <- predict(bag.seoul, newdata = test.df)

# 중요 변수
importance(bag.seoul)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "배깅-중요 변수")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

varImpPlot(bag.seoul)

dev.off()

# test MSE
mean((yhat.bag - test.df$Price)^2)


# ******************************************************************************
# 8. 다음은 앞의 Regression tree에 Random Forest를 적용한다. 
# 각 분리에 사용될 변수의 수(i.e., mtry)를 2~6까지 변화시키며 
# test MSE를 가장 작게 하는 변수를 선택하시오. 
# 그리고 그 결과 test MSE와 변수의 중요도를 도표화하여 설명하시오.
# ******************************************************************************
# test MSE를 가장 작게 하는 변수 (mtry) : 4
# 랜덤포레스트 결과 아파트 매매가격 추정에 있어서 보육시설 접근성(Nurser) 및 대학 진학률(H_Univ) 순으로 높았다. 
# 이에 학습모형을 통해 예측 성능은 30350.00의 MSE 오차를 보인다.
set.seed(1)

rfResData = data.frame()
for (i in c(2:6)) {
  
  rf.seoul <- randomForest::randomForest(seoulFor, data = train.df, mtry = i, importance = TRUE)
  yhat.rf <- predict(rf.seoul, newdata = test.df)
  
  # test MSE
  mseVal = mean((yhat.rf - test.df$Price)^2)
  
  tmpData = data.frame("mtry" = i, "mse" = mseVal)
  
  rfResData = rbind(rfResData, tmpData)
}

bestRfModel <- randomForest::randomForest(seoulFor, data = train.df, mtry = 5, importance = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "랜덤포레스트-중요 변수")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

varImpPlot(bestRfModel)

dev.off()

importance(bestRfModel)


# ******************************************************************************
# 9. 마지막으로 앞의 Regression tree에 Boosting을 적용한다. 
# 분리의 수(interaction.depth)를 1에서 10까지 조정하며, 
# 각 분리 수마다 가장 적합한 전체 트리의 수(n.trees)를 5-folds CV를 이용하여 추정하시오. 
# 각 분리의 수에 따른 test MSE를 제시하고 가장 낮은 모형을 선택하시오.
# ******************************************************************************
# 가장 낮은 모형 (interaction.depth) : 2
set.seed(1)

gbmResData = data.frame()
for (i in c(1:10)) {
  
  boost.seoul.cv <- gbm::gbm(seoulFor, data = train.df, distribution = "gaussian", 
                             n.trees = 5000, interaction.depth = i, cv.folds = 5)
  
  best.iter <- gbm.perf(boost.seoul.cv, method = 'cv')
  boost.seoul <- gbm(seoulFor, data = train.df, distribution = "gaussian", 
                     n.trees = best.iter, interaction.depth = i)
  yhat.boost <- predict(boost.seoul, newdata = test.df, n.trees = best.iter)
  
  # test MSE
  mseVal = mean((yhat.boost - test.df$Price)^2)
  
  tmpData = data.frame("interaction.depth" = i, "mse" = mseVal)
  
  gbmResData = rbind(gbmResData, tmpData)
}

bestBbmModel <- gbm(seoulFor, data = train.df, distribution = "gaussian", 
                    n.trees = best.iter, interaction.depth = 2)

summary(bestBbmModel)


# ******************************************************************************
# 10. 9번 모형에서 가장 중요도가 높은 변수 2개와 아파트 매매가격 간의 관계를 설명하시오.
# ******************************************************************************
# 대학 진학률 (H_Univ)이 높을수록 아파트 매매가격이 감소한다.
# 이는 강남권/용산구에 살면서 부모의 재력을 이어받은 자녀들은 명문대에 떨어지면 유학이나 재수를 선택하기 때문이다.
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "부스팅-중요 변수1")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(bestBbmModel, i = "H_univ")

dev.off()

# 보육시설 접근성 (Nurser)이 많을수록 아파트 매매 가격이 증가한다.
# 이는 양질의 보육시설에 대한 수요가 높아지면서 어린이집을 품고 있는 아파트가 인기를 끌기 때문이다.
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "부스팅-중요 변수2")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(bestBbmModel, i = "Nurser")

dev.off()

# ******************************************************************************
# 11. Regression tree, bagging, Random forest, boosting 중 test MSE를 기준으로 
# 아파트 매매가격을 가장 잘 예측하는 모형은 무엇인가?
# ******************************************************************************
# Regression tree : 42655.32
# Random forest :  30350.00
# boosting : 30949.44


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
# R을 이용한 보건 데이터 처리 및 연관 분석

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0267"
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
library(arules)
library(arulesViz)


# Step 1. 데이터 읽어오기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "복지_사회참여_문화와여가_소득과소비_노동_2019_20211207_52279.csv"))
# health <- read.csv("2016_사회조사_보건데이터_정제.csv",stringsAsFactors=T)
data = read.csv(file = fileInfo, header = FALSE)

# V1 : 가구번호
# V2 : 가구원번호
# V3 : 만연령
# V4 : 성별코드
# V5 : 개인적인간관계만족도코드
# V6 : 사회적관계망_대화상대도움요청대상인원수
# V7 : 사회적관계망_교류가족친척인원수
# V8 : 사회적관계망_교류가족친척외인원수
# V9 : 독서_교양서적권수
# V10 : 주말여가활동_동반자코드
# V11 : 여가활용만족도코드
# V12 : 주관적소득수준코드
# V13 : 현재소득여부
# V14 : 소득만족도코드
# V15 : 소비생활만족도코드
# V16 : 가구소득코드
# V17 : 분류코드_교육정도코드

# Step 2. 요약 통계 확인
summary(data)

# Step 3. 연관 분석할 데이터 정제하기
## 연관 분석할 데이터 항목 선택
# check = subset(data, select=c(성별, 혼인상태, 주관적만족감, 건강평가, 가정생활스트레스, 일상생활스트레스, 가구소득))

dataL1 = data %>% 
  dplyr::select(V5, V7, V8, V11, V16) %>% 
  na.omit()

# 소득수준에 따른 사회관계망 만족도와 인원수 연관분석
# 소득수준과 여가활동 만족도
# 사회관계망 만족도 연관분석
# 
# 
# 정제된  데이터를  가지고  apriori  함수(support,  confidence  조건은  학생들이  조정)를  이용하여  연관  규칙들을  찾고,  apriori  함수로  찾은  연관  규칙들에서  분석하고  싶은  패턴을  최소  2개  이상  정의inspect  함수를  이용하여  찾아진  각  패턴(규칙)  결과를  이용하여하고 
# , Ÿ 지지도(support)가  가장  높은  패턴(규칙)에  대해서  설명(분석)하기.
# Ÿ 신뢰도(confidence)가  가장  높은  패턴(규칙)에  대해서  설명(분석)하기.
# Ÿ 향상도(lift)가  가장  높은  패턴(규칙)에  대해서  설명(분석)하기. (예:  사회조사데이터를  통한  스트레스에  영향을  주는  요인  분석,  등) 
# # 

# ******************************************************************************
# 범주형 데이터 정제
# ******************************************************************************
# 개인적인간관계만족도코드 : 1-매우만족 2-약간만족 3-보통 4-약간불만족 5-매우불만족
dataL1$개인적인간관계만족도코드 = factor(dataL1$V5, levels=c(1:5), labels=c('매우만족', '약간만족', '보통', '약간불만족', '매우불만족'))

# 사회적관계망_교류가족친척인원수
dataL1$사회적관계망_교류가족친척인원수 = factor(dataL1$V7)

# 사회적관계망_교류가족친척외인원수
dataL1$사회적관계망_교류가족친척외인원수 = factor(dataL1$V8)

# 여가활용만족도코드 : 1-매우만족 2-약간만족 3-보통 4-약간불만족 5-매우불만족
dataL1$여가활용만족도코드 = factor(dataL1$V11, levels=c(1:5), labels=c('매우만족', '약간만족', '보통', '약간불만족', '매우불만족'))

# 여가활용만족도코드 : 1-매우만족 2-약간만족 3-보통 4-약간불만족 5-매우불만족
dataL1$여가활용만족도코드 = factor(dataL1$V11, levels=c(1:5), labels=c('매우만족', '약간만족', '보통', '약간불만족', '매우불만족'))

# 가구소득코드 : 1-100만원미만 2-100~200만원 3-200~300만원 4-300~400만원 5-400~500만원 6-500~600만원 7- 600~700만원 8-700만원이상, 9-800만원이상
dataL1$가구소득코드 <- factor(dataL1$V16, levels=c(1:9), labels=c('100만원미만', '100~200만원', '200~300만원', '300~400만원', '400~500만원', '500~600만원', '600~700만원', '700만원이상', '800만원이상'))

summary(dataL1)



# 소득수준에 따른 사회관계망 만족도와 인원수 연관분석
dataL2 = dataL1 %>% 
  # dplyr::select(가구소득코드, 사회적관계망_교류가족친척인원수, 사회적관계망_교류가족친척외인원수)
  dplyr::select(V16, V7, V8)


# 
# 
# ### 주관적만족감: 1-매우만족 2-약간만족 3-보통 4-약간불만족 5-매우불만족
# check$주관적만족감 <- factor(check$주관적만족감, levels=c(1:5), labels=c('매우만족', '약간만족', '보통', '약간불만족', '매우불만족'))
# 
# ### 건강평가: 1-매우좋다 2-좋은편이다 3-보통이다 4-나쁜편이다 5-매우나쁘다
# check$건강평가 <- factor(check$건강평가, levels=c(1:5), labels=c('매우좋다',	'좋은편이다', '보통이다',	'나쁜편이다', '매우나쁘다'))
# 
# ### 가정생활스트레스: 1-매우많이느꼈다 2-느끼는편이다 3-느끼지않는편이다 4-전혀느끼지않았다 5-해당없음
# check$가정생활스트레스 <- factor(check$가정생활스트레스, levels=c(1:5), labels=c('매우많이느꼈다', '느끼는편이다', '느끼지않는편이다', '전혀느끼지않았다', '해당없음'))
# 
# ### 일상생활스트레스: 1-매우많이느꼈다 2-느끼는편이다 3-느끼지않는편이다 4-전혀느끼지않았다 5-해당없음
# check$일상생활스트레스 <- factor(check$일상생활스트레스, levels=c(1:5), labels=c('매우많이느꼈다', '느끼는편이다', '느끼지않는편이다', '전혀느끼지않았다', '해당없음'))
# 
# ### 가구소득(월): 1-100만원미만 2-100~200만원 3-200~300만원 4-300~400만원 5-400~500만원 6-500~600만원 7- 600~700만원 8-700만원이상
# check$가구소득 <- factor(check$가구소득, levels=c(1:8), labels=c('100만원미만', '100~200만원', '200~300만원', '300~400만원', '400~500만원', '500~600만원', '600~700만원', '700만원이상'))
# 
# ## 정제 결과 확인
# summary(check)
# 
# 

# Step 5. 연관 분석
## 조건 (support=0.1, confidence=0.8, minlen=1, maxlen=10으로 연관분석
rule1 <- arules::apriori(dataL2)
summary(rule1)

plot(rule1)
plot(rule1, method="group")
plot(rule1, method="graph")

rule1.df <- as(rule1, "data.frame")


rule2 <- arules::apriori(dataL2, parameter=list(support=0.3))
summary(rule2)
rule2.df <- as(rule2, "data.frame")


rule_hstress <- inspect(subset(rule2, subset=rhs %in% c("가정생활스트레스=매우많이느꼈다", "가정생활스트레스=느끼는편이다")))

rule_lstress <- inspect(subset(rule2, subset=rhs %in% c("일상생활스트레스=매우많이느꼈다", "일상생활스트레스=느끼는편이다")))

rule_hstress2 <- inspect(subset(rule2, subset=rhs %in% c("가정생활스트레스=느끼지않는편이다", "가정생활스트레스=전혀느끼지않았다")))

rule_lstress2 <- inspect(subset(rule2, subset=rhs %in% c("일상생활스트레스=느끼지않는편이다", "일상생활스트레스=전혀느끼지않았다")))

rule_mhstress <- inspect(subset(rule2, subset= lhs %pin% c("혼인상태=") & rhs %in% c("가정생활스트레스=매우많이느꼈다", "가정생활스트레스=느끼는편이다")))

rule_ihstress <- inspect(subset(rule2, subset= lhs %pin% c("가구소득=") & rhs %in% c("가정생활스트레스=매우많이느꼈다", "가정생활스트레스=느끼는편이다")))


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
# R을 이용한 2018-2020년 서울시 지하철 데이터 시각화 (시계열, 막대그래프, 트리맵)

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0269"
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
library(ggplot2)
library(openxlsx)
library(lubridate)
library(tidyr)
library(treemapify)
library(ggplot2)

# 자료 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "서울시+교통2018-2019-2020.xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1)

# 자료 전처리
dataL1 = data %>% 
  tibble::as.tibble() %>% 
  dplyr::select(-c("합.계")) %>% 
  tidyr::gather(-c("날짜", "호선", "역번호", "역명", "구분"), key = "key", value = "val") %>% 
  dplyr::mutate(
    dtDate = readr::parse_datetime(날짜, "%Y-%m-%d")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
    , dtDay = lubridate::day(dtDate)
    , dtRefDate = lubridate::make_date(month = dtMonth, day = dtDay)
  )


# ******************************************************************************
# 시계열 그래프는 년도별로 색깔을 다르게 하여 1년치의 승차객수의 합으로 
# 시각화를 하고자합니다.
# ******************************************************************************
dataL2 = dataL1 %>% 
  dplyr::group_by(dtRefDate, dtYear) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  )

plotSubTitle = sprintf("%s %s", "2018-2020년", "지하철 시계열")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(dataL2, aes(x = dtRefDate, y = sumVal, color = factor(dtYear))) +
  geom_line() +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  labs(x = "연도", y = "승객수", color = "연도", subtitle = plotSubTitle) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ******************************************************************************
# 비교 막대그래프는 승차와 하차를 막대 안에서 색깔로 비교하고 x축은 호선으로 
# 구분하고자 합니다
# ******************************************************************************
dataL2 = dataL1 %>% 
  dplyr::group_by(호선, 구분) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  )

plotSubTitle = sprintf("%s %s", "2018-2020년", "지하철 막대그래프")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(dataL2, aes(x = 호선, y = sumVal, fill = 구분)) +
  geom_bar(position = "dodge", stat = "identity") +
  # scale_x_date(date_labels = "%B", date_breaks = "2 month") +
  labs(x = "호선", y = "승객수", fill = "구분", subtitle = plotSubTitle) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ******************************************************************************
# 텍스트 시각화는 이미지 맵으로 4호선만 활용하여 승차를 많이한 기준으로 
# 이미지맵을 만들고 하차를 많이한 기준으로 이미지맵을 그리고 싶습니다. 
# 텍스트는 엑셀 열 이름인 시간대로요!
# ******************************************************************************
typeList = dataL1$구분 %>% unique() %>% sort()

for (typeInfo in typeList) {
  
  dataL2 = dataL1 %>% 
    dplyr::filter(
      호선 == "4호선"
      , 구분 == typeInfo
    ) %>% 
    dplyr::group_by(key, 구분) %>% 
    dplyr::summarise(
      sumVal = sum(val, na.rm = TRUE)
    )
  
  plotSubTitle = sprintf("%s %s %s %s", "2018-2020년", "지하철", typeInfo, "트리맵")
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
  
  makePlot = ggplot(dataL2, aes(area = sumVal, fill = key, label = paste(key, sumVal, sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(colour = "white", place = "centre", size = 15) +
    labs(x = NULL, y = NULL, fill = NULL, subtitle = plotSubTitle) +
    theme(
      text = element_text(size = 18)
      , legend.position = "none"
    )
  
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
# R을 이용한 지도학습 ()
# 작업하시기 편하신 데이터 자율선정하셔서 분석 진행해주시면 됩니다.
# 
# -1) 분석 데이터(활용데이터)
# -2) 방법론(classification) 활용
# -3) 분석 결과(r코드 및 결과 이미지 삽입)
# 순으로 진행 해주시고
# 이번엔 제가 첨부드린 ppt에 디자인 고려안해주셔도 되니 내용만 넣어주시면 되세요
# 
# 기존과 동일하게 넣어주시되,
# 분석할 데이터, 방법론에 대한 간략설명, 분석결과(이미지,r코드) 넣어주시면 되세요!
#   ** 독립변수나 종속변수는 1개는 안되고 되도록 2개이상으로 많으면 좋아요 ㅠㅠ
# 
# 첨부드린 자료는 참고용으로 보내드렸어요 보시고 자율선정해주셔서 분석 부탁드리겠습니다
# 
# 기한은 다음주 17일 늦어도 그주 주말까지 부탁드릴게요
# 보시고 연락주세요 감사합니다~!

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0271"
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
library(ISLR)
library(splines)
library(gam)
library(akima)
library(RColorBrewer)
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
library(tree)
library(MASS)
library(randomForest)
library(gbm)
library(BART)
library(spdep)
library(rgdal)
library(foreign) 
library(ROCit)
library(klaR)
library(class)

# shp 파일 읽기
mapShp = readOGR(dsn = globalVar$inpPath, layer = "Seoul_dong", encoding = 'ESRI Shapefile')
mapData = read.dbf(file.path(globalVar$inpPath, 'Seoul_dong.dbf'))
mapData$Div = as.factor(mapData$Div)

# 컬럼 선택
mapDataL1 = mapData %>% 
  dplyr::select(Price, Year, Nurser, Hospit, Park, Culture, Sub, E_prog, H_univ, M_priv, Div)

# ******************************************************************************
# 변수 선택을 통해 유의미한 변수 도출
# ******************************************************************************
# AIC 기준으로 유의미한 변수 선택
glmFitVarAll = glm(Div ~ ., data=mapDataL1, family=binomial(link="logit"))
stepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

# 결과에 대한 요약
summary(stepAic)

# 유의미한 변수 도출
# Div ~ Price + Year + Nurser + Park + Culture + H_univ + M_priv

# ******************************************************************************
# 훈련 및 테스트 데이터셋 분류
# ******************************************************************************
set.seed(100)

# 훈련 및 테스트 데이터셋 분류
sample.idx <-sample(1:nrow(mapDataL1), nrow(mapDataL1) * 0.7) 
train.df <- mapDataL1[sample.idx, ]
test.df <- mapDataL1[-sample.idx,]

mainTitle = "훈련 데이터셋 정보"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(mapShp, main = mainTitle)
plot(mapShp[sample.idx, ], col='red', add=TRUE)

dev.off()

# 테스트셋 종속 변수
yObs = factor(test.df$Div, levels = c(0, 1), labels = c("ETC", "Gangnam"))

# ******************************************************************************
# 로지스틱 회귀분석
# ******************************************************************************
logit.train <- glm(stepAic$formula, data=train.df, family=binomial(link="logit"))
summary(logit.train)

logit.Test <- predict(logit.train, newdata=test.df, type="response")
div.Pred <- ifelse(logit.Test < 0.5, 0, 1)
yHat = factor(div.Pred, levels = c(0, 1), labels = c("ETC", "Gangnam"))

conMatRes = caret::confusionMatrix(data = yHat, reference = yObs)

# 정확도 : 0.937 
conMatRes$overall["Accuracy"] %>% round(3)

# 민감도 : 0.971 
conMatRes$byClass["Sensitivity"] %>% round(3)

# 특이도 : 0.792 
conMatRes$byClass["Specificity"] %>% round(3)

# ROC 커브를 위한 설정
logit.Roc <- ROCit::rocit(score = logit.Test, class = test.df$Div)

# 요약 결과
# summary(logit.Roc)

mainTitle = "ROC 곡선-로지스틱 회귀분석"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(logit.Roc, main = mainTitle)

dev.off()

# ******************************************************************************
# 선형 판별분석
# ******************************************************************************
lda.train <- lda(stepAic$formula, data=train.df)
# summary(lda.train)

lda.test <- predict(lda.train, test.df)
yHat = factor(lda.test$class, levels = c(0, 1), labels = c("ETC", "Gangnam"))

conMatRes = caret::confusionMatrix(data = yHat, reference = yObs)

# 정확도 : 0.906 
conMatRes$overall["Accuracy"] %>% round(3)

# 민감도 : 0.981 
conMatRes$byClass["Sensitivity"] %>% round(3)

# 특이도 : 0.583 
conMatRes$byClass["Specificity"] %>% round(3)

# ROC 커브를 위한 설정
lda.Roc <- ROCit::rocit(score = lda.test$posterior[,2], class = test.df$Div)

# 요약 결과
summary(lda.Roc)

mainTitle = "ROC 곡선-선형 판별분석"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(lda.Roc, main = mainTitle)

dev.off()


# ******************************************************************************
# 비선형 판별분석
# ******************************************************************************
qda.train <- qda(stepAic$formula, data=train.df)

qda.test <- predict(qda.train, test.df)
yHat = factor(qda.test$class, levels = c(0, 1), labels = c("ETC", "Gangnam"))

conMatRes = caret::confusionMatrix(data = yHat, reference = yObs)

# 정확도 : 0.898 
conMatRes$overall["Accuracy"] %>% round(3)

# 민감도 : 0.971 
conMatRes$byClass["Sensitivity"] %>% round(3)

# 특이도 : 0.583 
conMatRes$byClass["Specificity"] %>% round(3)

# ROC 커브를 위한 설정
qda.Roc <- ROCit::rocit(score = qda.test$posterior[,2], class = test.df$Div)

# 요약 결과
# summary(qda.Roc)

mainTitle = "ROC 곡선-비선형 판별분석"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(qda.Roc, main = mainTitle)

dev.off()


# ******************************************************************************
# K 인접 기법
# ******************************************************************************
# 변수 스케일링
mapDataL2 = mapDataL1 %>% 
  dplyr::select(-c("Div")) %>% 
  scale()

# 트레이닝 및 테스트 데이터 셋 분류
train.x <- mapDataL2[sample.idx, ]
test.x <- mapDataL2[-sample.idx,]

knn.pred <- class::knn(train = train.x, test = test.x, cl = train.df$Div, k=1, prob = TRUE)
yHat = factor(knn.pred, levels = c(0, 1), labels = c("ETC", "Gangnam"))

conMatRes = caret::confusionMatrix(data = yHat, reference = yObs)

# 정확도 : 0.929 
conMatRes$overall["Accuracy"] %>% round(3)

# 민감도 : 0.981 
conMatRes$byClass["Sensitivity"] %>% round(3)

# 특이도 : 0.708 
conMatRes$byClass["Specificity"] %>% round(3)


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
# R을 이용한 의사소통 및 진로결정 데이터 분석 (분산분석, 상관분석, 회귀분석) 및 보고서

# 안녕하세요. 과제를 하고 있는 대학생입니다.
# 이번학기 통계수업을 듣고 설문 수집을 하고 R을 활용해서 분석을 한 뒤 보고서를 작성해야 하는 과제를 하고 있습니다.
# 제가 가지고 있는 데이터로 어떤 분석을 해야 효과적일지 이야기를 좀 나눠보고 싶고,
# R 스크립트도 제출해야하는데 처음 해보는 것이라 도움을 받고자 올려봅니다.

# 주제: 대학생의 부모-자녀 의사소통과 진로결정몰입도 간의 관계 연구
# 가설: 대학생들이 부모-자녀 간 의사소통이 활발하게 이루어질수록 진로결정몰입도가 높아질 것이다.

# 변수명입니다!
# com_mean: 의사소통척도 평균
# com_sum: 의사소통척도 합계
# com_normal: 부모님과 일상적인 주제로 대화할 때 소통방식
# com_important: 부모님과 중요한 주제로 대화할 때 소통방식
# com_time: 일주일에 부모님과 소통하는 시간
# dream_decide: 진로결정여부
# IMM_MEAN: 진로결정몰입척도 평균
# IMM_SUM: 진로결정몰입척도 합계

# 1. 분산분석(anova): IMM_MEAN 과 com_time, com_important, dream_decide
# 2. 상관분석: IMM_MEAN과 com_mean
# 3. 회귀분석: IMM_MEAN과 com_mean
# * 스크립트를 첨부해야해서 같이 보내주시면 감사하겠습니다!

#================================================
# 초기 환경변수 설정
# ================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0274"
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
library(ggpubr)
library(ggplot)
library(agricolae)
library(ggcorrplot)

# 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "r+final+assignment.csv"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "r+finalassignment_20211217.csv"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

# 결측값 제거
dataL1 = data %>% 
  na.omit()

# 이상치 제거
lmFit = lm(IMM_MEAN ~ com_mean, data = dataL1)
# summary(lmFit)

cooksDis =  cooks.distance(lmFit) 
outlierVal = 4 * mean(cooksDis, na.rm = TRUE)
dataL1$outFlag = ifelse(cooksDis > outlierVal, TRUE, FALSE)
# dataL1$outFlag[30] = FALSE

dataL2 = dataL1 %>% 
  dplyr::filter(outFlag == FALSE)

# 문자형을 범주형 변수로 변환
dataL2$com_time = as.factor(dataL2$com_time)
dataL2$com_important = as.factor(dataL2$com_important)
dataL2$dream_decide = as.factor(dataL2$dream_decide)

summary(dataL2)

# ******************************************************************************
# 1. 분산분석(anova): IMM_MEAN 과 com_time, com_important, dream_decide
# ******************************************************************************
# anova 검정 시 집단 간 평균에 유의미한 차이 분석
aovModel = aov(IMM_MEAN ~ com_time + com_important + dream_decide, data = dataL2)

# 그 결과 dream_decide (진로결정여부)의 유의 수준 (Pr(>F))이 약 0.1 이하로서 
# 집단 간의 평균 차이가 있음 (대립가설 채택)
# 반면에 com_time (일주일에 부모님과 소통하는 시간), 
# com_important (부모님과 중요한 주제로 대화할 때 소통방식)의 
# 유의 수준 (Pr(>F))이 약 0.1 이상으로서 집단 간의 평균 차이가 없음 (대립가설 기각)
summary(aovModel)

# Df    Sum Sq    Mean Sq F value  Pr(>F)
# com_time       4 0.2140050 0.05350124 0.46833 0.75833
# com_important  2 0.2759776 0.13798881 1.20790 0.31789
# dream_decide   4 0.9878504 0.24696260 2.16181 0.10703
# Residuals     22 2.5132579 0.11423900      


# 추가로 사후분석 (Duncan's LSR test)을 통해 어느 집단 간에 차이가 있는가를 분석
result = agricolae::duncan.test(aovModel, c("com_time", "com_important",  "dream_decide"), alpha = 0.05, console = TRUE)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "부모님 의사소통 조건에 따른 진로결정몰입척도 (com_time, com_important, dream_decide)")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(result)
dev.off()

# 9개 그룹
result$groups %>% 
  as.tibble() %>% 
  dplyr::select(groups) %>% 
  unique() %>% 
  nrow()


# ******************************************************************************
# 2. 상관분석: IMM_MEAN과 com_mean
# ******************************************************************************
# 상관계수
dataL3 = dataL2 %>% 
  dplyr::select(IMM_MEAN, com_mean)

corRes = cor(dataL3)
pvalRes = ggcorrplot::cor_pmat(dataL3)

plotSubTitle = sprintf("%s", "상관계수 행렬")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggcorrplot::ggcorrplot(
  corRes
  , outline.col = "white"
  , lab = TRUE
  , p.mat = pvalRes
  , sig.level = 0.05
  , colors = c("#6D9EC1", "white", "#E46726")
  ) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# 상관분석
lmFit = lm(IMM_MEAN ~ com_mean, data = dataL2)

summary(lmFit)

plotSubTitle = sprintf("%s", "의사소통척도에 따른 진로결정몰입척도 산점도")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggpubr::ggscatter(
  dataL2, x = "com_mean", y = "IMM_MEAN"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  labs(
    title = NULL
    , x = "평균 의사소통척도"
    , y = "평균 진로결정몰입척도"
    , color = NULL
    , subtitle = plotSubTitle
  ) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.90, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)


# ******************************************************************************
# 3. 회귀분석: IMM_MEAN과 com_mean
# ******************************************************************************
dataL3 = data.frame(
  yObs = dataL2$IMM_MEAN
  , yHat = predict(lmFit, newdata = dataL2)
)

biasVal = Metrics::bias(actual = dataL3$yObs, predicted = dataL3$yHat)
rmseVal = Metrics::rmse(actual = dataL3$yObs, predicted = dataL3$yHat)

# summary(dataL3)

plotSubTitle = sprintf("%s", "진로결정몰입척도 예측 및 실측 산점도")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggpubr::ggscatter(
  dataL3, x = "yHat", y = "yObs"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , add.params = list(color = "blue", fill = "lightblue")
  ) +
  labs(
    title = NULL
    , x = "진로결정몰입척도 예측"
    , y = "진로결정몰입척도 실측"
    , color = NULL
    , subtitle = plotSubTitle
  ) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.90, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.80, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 5) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.75, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 5) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)


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
# R을 이용한 서울특별시 버스위치 및 노선정보 구글맵 시각화

#================================================
# 초기 환경변수 설정
# ================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0275"
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
library(ggplot)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(RCurl)
library(stringr)
library(ggrepel)
library(ggmap)
library(forcats)

# 공공데이터포털 API키
reqDataKey = globalVar$dataKey
# reqDataKey = "u9oRMh10jAyyk+nlhHGphWG+4aapJz6++xQm+SyvyD7LxuOJUwjzcighNSi2zOm/K0xQWKNFL2RUh3vymjD2ag=="

# 구글 파일키 등록
reqGoogleKey = globalVar$googleKey
# reqGoogleKey = ""
ggmap::register_google(key = reqGoogleKey)

# ******************************************************************************
# 검색 옵션
# ******************************************************************************
# 버스 번호
searchBusRouteNm = 402

# ******************************************************************************
# 공공데이터포털 API (노선정보조회 서비스)
# ******************************************************************************
# 요청 URL
reqBusRouteInfoUrl = "http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList"

# 요청 파라미터
reqKey = stringr::str_c("?serviceKey=", RCurl::curlEscape(stringr::str_conv(reqDataKey, encoding = "UTF-8")))
reqResultType = stringr::str_c("&resultType=", "json")
reqStrSrch = stringr::str_c("&strSrch=", searchBusRouteNm)

resData = httr::GET(
  stringr::str_c(reqBusRouteInfoUrl, reqKey, reqResultType)
  ) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON() 

refData = resData$msgBody$itemList %>% 
  as.tibble()

data = refData %>% 
  dplyr::filter(busRouteNm == searchBusRouteNm)


# ******************************************************************************
# 공공데이터포털 API (노선정보조회 서비스)
# ******************************************************************************
# 요청 URL
reqBusRouteInfoUrl = "http://ws.bus.go.kr/api/rest/busRouteInfo/getStaionByRoute"

# 요청 키
reqBusRouteId= stringr::str_c("&busRouteId=", data$busRouteId)

resData = httr::GET(
  stringr::str_c(reqBusRouteInfoUrl, reqKey, reqResultType, reqBusRouteId)
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON()

dataL1 = resData$msgBody$itemList %>% 
  as.tibble()


# ******************************************************************************
# 공공데이터포털 API (버스위치정보조회 서비스)
# ******************************************************************************
# 요청 URL
reqBusPosUrl = "http://ws.bus.go.kr/api/rest/buspos/getBusPosByRtid"

# 요청 키
reqBusRouteId= stringr::str_c("&busRouteId=", data$busRouteId)

resData = httr::GET(
  stringr::str_c(reqBusPosUrl, reqKey, reqResultType, reqBusRouteId)
) %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON()

dataL2 = resData$msgBody$itemList %>% 
  as.tibble()


# ******************************************************************************
# 구글맵 시각화
# ******************************************************************************
# 자료 전처리 (컬럼 선택, 이름 변경, 자동 형 변환)
dataL3 = dataL2 %>% 
  dplyr::left_join(dataL1, by = c("sectOrd" = "seq")) %>% 
  dplyr::select(gpsX.x, gpsY.x, stationNm, plainNo) %>% 
  dplyr::rename(
    lon = gpsX.x
    , lat = gpsY.x
  ) %>%
  dplyr::arrange(lon) %>% 
  tibble::rowid_to_column() %>% 
  dplyr::mutate(
    label = stringr::str_c("[", plainNo, "]\n", stationNm)
    , legend = stringr::str_c(rowid, ". [", plainNo, "] ", stationNm)
  ) %>%
  readr::type_convert()

dataL3$legend = forcats::fct_relevel(factor(dataL3$legend), dataL3$legend)

# 구글맵 지정
map = ggmap::get_googlemap(
  center = c(lon = mean(dataL3$lon, na.rm = TRUE), lat = mean(dataL3$lat, na.rm = TRUE))
  , zoom = 12
  , markers = dataL3 %>% dplyr::select(lon, lat)
  )

# 구글맵 시각화
plotSubTitle = sprintf("%s", "서울특별시 버스위치 및 노선정보 구글맵 시각화")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggmap::ggmap(map, extent = "device") +
  ggrepel::geom_label_repel(data = dataL3, aes(x = lon, y = lat, color = legend), hjust = 0, alpha = 0.75, size = 4, label = dataL3$label, show.legend = TRUE) +
  labs(color = "[버스 번호] 정류장 이름") +
  theme(
    text = element_text(size = 18)
    , legend.position = c(0.80, 0.80)
    , legend.title = element_text(size = 12)
    , legend.text = element_text(size = 10)
    ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


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
# R을 이용한 CDF 분포 추정 그리고 JS (잭슨 샤넌) 및 KL (쿨러 라이블러 발산)를 이용한 서로 간의 분포 비교

#================================================
# 초기 환경변수 설정
# ================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0281"
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

# globalVar = list()
# globalVar$inpPath = "F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/GEV"
# globalVar$figPath = "F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/GEV"
# globalVar$outPath = "F:/R/tu/tttt/tut/GCM/Water quality quantity/Baysian/GEV"

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(tidyverse)
library(readr)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(dplyr)
library(data.table)
library(Rcpp)
library(philentropy)
library(h2o)

# ******************************************************************************
# 1. M1 , M2 분포 회귀식으로 중간값 분포값 추정 예 (350)
# ******************************************************************************
# # Y=ax^2+bx+c의 회귀식이 있으면 x에 확률 pdf와 cdf가 들어가고 저 값을 산정합니다!

# 옵션 설정 (95 %)
sysOpt = list(
  "probs" = 0.95
)

# 초기화
h2o::h2o.init()

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "예시.xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1)

dataL1 = data %>% 
  na.omit() %>% 
  readr::type_convert()

# tmpData = dataL1[1:30, ]

dataL2 = dplyr::bind_rows(
  dataL1[ , 1:4]
  , dataL1[ , 5:8]
  , dataL1[ , 9:12]
  ) %>%
  dplyr::arrange(type, obs)


# ******************************************************************************
# 전체 데이터에 대한 예측
# ******************************************************************************
typeList = dataL2$type %>% unique() %>% sort()

# obs 컬럼을 기준으로 최대값 선택
statData = dataL2 %>%
  dplyr::group_by(type) %>% 
  dplyr::summarise(
    minVal = min(obs, na.rm = TRUE)
    , maxVal = max(obs, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(desc(maxVal)) %>% 
  slice(length(typeList))

# 학습을 위한 테스트 데이터
minVal = as.integer(min(statData$minVal, na.rm = TRUE) - 1)
maxVal = as.integer(max(statData$maxVal, na.rm = TRUE) + 1)

testDataL1 = tibble(obs = seq(minVal, maxVal))

# 반복문 수행
typeList = dataL2$type %>% unique() %>% sort()
dataL3 = tibble::tibble()

# typeInfo = "OBS"
for (typeInfo in typeList) {
  
  # 훈련 데이터
  trainData = dataL2 %>% 
    dplyr::filter(type == typeInfo)
 
  # CDF/PDF 다중선형회귀모형 학습
  # lmCdfModel = lm(cdf ~ poly(obs, 4), data = trainData)
  # lmPdfModel = lm(pdf ~ poly(obs, 4), data = trainData)
  
  # 요약 결과
  # summary(lmCdfModel)
  # summary(lmPdfModel)

  # CDF 학습 모델
  saveCdfFile = sprintf("%s/%s/%s-%s-%s-%s-%s.model", globalVar$inpPath, serviceName, 'final', typeInfo, 'h2o', 'cdf', 'train')

  # CDF 학습 모델이 있을 경우
  if (fs::file_exists(saveCdfFile)) {
    amlCdfModel = h2o::h2o.loadModel(saveCdfFile)
  } else {
    # CDF 모델 학습
    amlCdfModel = h2o::h2o.automl(
      x = "obs"
      , y = "cdf"
      , training_frame = as.h2o(trainData)
      , nfolds = 2
      , sort_metric = "RMSE"
      , stopping_metric = "RMSE"
      , seed = 1
      , max_models = 5
    )

    amlCdfBestModel = h2o.get_best_model(amlCdfModel)
    h2o::h2o.saveModel(object = amlCdfBestModel, path = fs::path_dir(saveCdfFile), filename = fs::path_file(saveCdfFile), force = TRUE)
  }

  # 요약
  # summary(amlPdfModel)
  #   
  # PDF 학습 모델
  savePdfFile = sprintf("%s/%s/%s-%s-%s-%s-%s.model", globalVar$inpPath, serviceName, 'final', typeInfo, 'h2o', 'pdf', 'train')

  # PDF 학습 모델이 있을 경우
  if (fs::file_exists(savePdfFile)) {
    amlPdfModel = h2o::h2o.loadModel(savePdfFile)
  } else {
    # PDF 모델 학습
    amlPdfModel = h2o::h2o.automl(
      x = "obs"
      , y = "pdf"
      , training_frame = as.h2o(trainData)
      , nfolds = 2
      , sort_metric = "RMSE"
      , stopping_metric = "RMSE"
      , seed = 1
      , max_models = 5
    )

    amlPdfBestModel = h2o.get_best_model(amlPdfModel)
    h2o::h2o.saveModel(object = amlPdfBestModel, path = fs::path_dir(savePdfFile), filename = fs::path_file(savePdfFile), force = TRUE)
  }

  # 요약
  # summary(amlPdfModel)

  # 앞선 테스트 데이터를 이용하되 type를 동적으로 변경
  testDataL2 = testDataL1 %>%
    dplyr::mutate(type = typeInfo)
  
  # 테스트 데이터셋에 적용
  # testDataL2$cdf = predict(lmCdfModel, newdata = testDataL2)
  # testDataL2$pdf = predict(lmPdfModel, newdata = testDataL2)
  
  testDataL2$cdf = as.data.frame(h2o::h2o.predict(object = amlCdfModel, newdata = as.h2o(testDataL2)))$predict
  testDataL2$pdf = as.data.frame(h2o::h2o.predict(object = amlPdfModel, newdata = as.h2o(testDataL2)))$predict
  
  dataL3 = dplyr::bind_rows(dataL3, testDataL2)
}

cdfData = dataL3 %>% 
  dplyr::select(-pdf) %>%
  tidyr::spread(key = "type", value = c("cdf"))

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "cdfData")
readr::write_csv(x = cdfData, file = saveFile)

pdfData = dataL3 %>% 
  dplyr::select(-cdf) %>% 
  tidyr::spread(key = "type", value = c("pdf"))

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "pdfData")
readr::write_csv(x = pdfData, file = saveFile)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "실측 및 예측에 대한 CDF 산점도")

ggplot() +
  geom_point(data = dataL2 %>% dplyr::filter(type == "OBS"), aes(x = obs, y = cdf, color = "실측 OBS")) +
  geom_point(data = dataL2 %>% dplyr::filter(type == "M1"), aes(x = obs, y = cdf, color = "실측 M1")) +
  geom_point(data = dataL2 %>% dplyr::filter(type == "M2"), aes(x = obs, y = cdf, color = "실측 M2")) +
  geom_line(data = dataL3, aes(x = obs, y = cdf, color = type)) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "실측 및 예측에 대한 PDF 산점도")

ggplot() +
  geom_line(data = dataL2 %>% dplyr::filter(type == "OBS"), aes(x = obs, y = pdf, color = "실측 OBS")) +
  geom_line(data = dataL2 %>% dplyr::filter(type == "M1"), aes(x = obs, y = pdf, color = "실측 M1")) +
  geom_line(data = dataL2 %>% dplyr::filter(type == "M2"), aes(x = obs, y = pdf, color = "실측 M2")) +
  geom_point(data = dataL3, aes(x = obs, y = pdf, color = type)) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


# ******************************************************************************
# 95% 데이터에 대한 예측
# ******************************************************************************
# obs 컬럼을 기준으로 최대값 선택
statData95 = dataL2 %>%
  dplyr::filter(cdf >= sysOpt["probs"]) %>% 
  dplyr::group_by(type) %>% 
  dplyr::summarise(
    minVal = min(obs, na.rm = TRUE)
    , maxVal = max(obs, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(desc(maxVal)) %>% 
  slice(length(typeList))

# 학습을 위한 테스트 데이터
minVal95 = as.integer(min(statData95$minVal, na.rm = TRUE) - 1)
maxVal95 = as.integer(max(statData95$maxVal, na.rm = TRUE) + 1)

testData95L1 = tibble(obs = seq(minVal95, maxVal95))

# 반복문 수행
typeList = dataL2$type %>% unique() %>% sort()
dataL4 = tibble::tibble()

# typeInfo = "OBS"
for (typeInfo in typeList) {
  
  # 훈련 데이터
  trainData = dataL2 %>% 
    dplyr::filter(
      type == typeInfo
      , cdf >= sysOpt["probs"]
      )
  
  # CDF/PDF 다중선형회귀모형 학습
  # lmCdfModel = lm(cdf ~ poly(obs, 3), data = trainData)
  # lmPdfModel = lm(pdf ~ poly(obs, 3), data = trainData)

  # 요약 결과
  # summary(lmCdfModel)
  # summary(lmPdfModel)
  
  # CDF 학습 모델
  saveCdfFile = sprintf("%s/%s/%s-%s-%s-%s-%s.model", globalVar$inpPath, serviceName, 'final', typeInfo, 'h2o', 'cdf95', 'train')
  
  # CDF 학습 모델이 있을 경우
  if (fs::file_exists(saveCdfFile)) {
    amlCdfModel = h2o::h2o.loadModel(saveCdfFile)
  } else {
    # CDF 모델 학습
    amlCdfModel = h2o::h2o.automl(
      x = "obs"
      , y = "cdf"
      , training_frame = as.h2o(trainData)
      , nfolds = 2
      , sort_metric = "RMSE"
      , stopping_metric = "RMSE"
      , seed = 1
      , max_models = 5
    )
    
    amlCdfBestModel = h2o.get_best_model(amlCdfModel)
    h2o::h2o.saveModel(object = amlCdfBestModel, path = fs::path_dir(saveCdfFile), filename = fs::path_file(saveCdfFile), force = TRUE)
  }
  
  # 요약
  # summary(amlPdfModel)
  #   
  # PDF 학습 모델
  savePdfFile = sprintf("%s/%s/%s-%s-%s-%s-%s.model", globalVar$inpPath, serviceName, 'final', typeInfo, 'h2o', 'pdf95', 'train')
  
  # PDF 학습 모델이 있을 경우
  if (fs::file_exists(savePdfFile)) {
    amlPdfModel = h2o::h2o.loadModel(savePdfFile)
  } else {
    # PDF 모델 학습
    amlPdfModel = h2o::h2o.automl(
      x = "obs"
      , y = "pdf"
      , training_frame = as.h2o(trainData)
      , nfolds = 2
      , sort_metric = "RMSE"
      , stopping_metric = "RMSE"
      , seed = 1
      , max_models = 5
    )
    
    amlPdfBestModel = h2o.get_best_model(amlPdfModel)
    h2o::h2o.saveModel(object = amlPdfBestModel, path = fs::path_dir(savePdfFile), filename = fs::path_file(savePdfFile), force = TRUE)
  }
  
  # 요약
  # summary(amlPdfModel)
  
  # 앞선 테스트 데이터를 이용하되 type를 동적으로 변경
  testData95L2 = testData95L1 %>%
    dplyr::mutate(type = typeInfo)
  
  # 테스트 데이터셋에 적용
  # testDataL2$cdf = predict(lmCdfModel, newdata = testDataL2)
  # testDataL2$pdf = predict(lmPdfModel, newdata = testDataL2)
  
  testData95L2$cdf = as.data.frame(h2o::h2o.predict(object = amlCdfModel, newdata = as.h2o(testData95L2)))$predict
  testData95L2$pdf = as.data.frame(h2o::h2o.predict(object = amlPdfModel, newdata = as.h2o(testData95L2)))$predict
  
  dataL4 = dplyr::bind_rows(dataL4, testData95L2)
}

cdfData95 = dataL4 %>% 
  dplyr::select(-pdf) %>%
  tidyr::spread(key = "type", value = c("cdf"))

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "cdfData95")
readr::write_csv(x = cdfData95, file = saveFile)

pdfData95 = dataL4 %>% 
  dplyr::select(-cdf) %>% 
  tidyr::spread(key = "type", value = c("pdf"))

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "pdfData95")
readr::write_csv(x = pdfData95, file = saveFile)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "실측 및 예측에 대한 CDF95 산점도")

ggplot() +
  geom_point(data = dataL2 %>% dplyr::filter(type == "OBS", cdf >= sysOpt["probs"]), aes(x = obs, y = cdf, color = "실측 OBS")) +
  geom_point(data = dataL2 %>% dplyr::filter(type == "M1", cdf >= sysOpt["probs"]), aes(x = obs, y = cdf, color = "실측 M1")) +
  geom_point(data = dataL2 %>% dplyr::filter(type == "M2", cdf >= sysOpt["probs"]), aes(x = obs, y = cdf, color = "실측 M2")) +
  geom_line(data = dataL4, aes(x = obs, y = cdf, color = type)) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "실측 및 예측에 대한 PDF95 산점도")

ggplot() +
  geom_line(data = dataL2 %>% dplyr::filter(type == "OBS", cdf >= sysOpt["probs"]), aes(x = obs, y = pdf, color = "실측 OBS")) +
  geom_line(data = dataL2 %>% dplyr::filter(type == "M1", cdf >= sysOpt["probs"]), aes(x = obs, y = pdf, color = "실측 M1")) +
  geom_line(data = dataL2 %>% dplyr::filter(type == "M2", cdf >= sysOpt["probs"]), aes(x = obs, y = pdf, color = "실측 M2")) +
  geom_point(data = dataL4, aes(x = obs, y = pdf, color = type)) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


# ******************************************************************************
# 2. JS와 KL을 이용하여 각각 M1, M2의 분포와 OBS 분포와의 차이 계산
# (분포의 길이는 데이터 길이가 가장 긴 모형의 길이로 산정)
# ******************************************************************************
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "LSTM+Naju+distribution.csv"))
# one = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

# 전체 데이터
one = dataL3

summary(one)

# one =fread(fileInfo) %>% data.frame(stringsAsFactors = F) %>% data.frame(stringsAsFactors = F)
head(one)

two = one$type %>% unique()

one_list = list()

for(i in 1:length(two)){
  one_list[[i]] = one %>% 
    dplyr::filter(type == two[i]) %>% 
    dplyr::select(pdf) %>% 
    t()
}


one_dat = one_list %>% do.call(rbind, .)

CL1<-philentropy::KL(one_dat, test.na = TRUE, unit = "log", est.prob = "empirical")

# write.csv(CL1,"F:/ploting/GEV PLOT/KL S Youngsan distribution PDF.csv")

saveCsvFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "KL S Youngsan distribution PDF")
write.csv(CL1, saveCsvFile)

for(i in 1:length(two)){
  
  one_list[[i]] = one %>% dplyr::filter(type == two[i]) %>% dplyr::select(cdf) %>% t()
  
}



one_dat = one_list %>% do.call(rbind, .)


CL2<-philentropy::KL(one_dat, test.na = TRUE, unit = "log", est.prob = "empirical")

# write.csv(CL2,"F:/ploting/GEV PLOT/KL S Youngsan distribution CDF.csv")

saveCsvFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "KL S Youngsan distribution CDF")
write.csv(CL2, saveCsvFile)

#######################################
# 옵션에서 확률 설정에 따른 데이터
one = dataL4
one_list = list()

two = one$type %>% unique()

for(i in 1:length(two)){
  
  one_list[[i]] = one %>% dplyr::filter(type == two[i]) %>% dplyr::select(pdf) %>% t()
  
}

one_dat = one_list %>% do.call(rbind, .)


CL3<-philentropy::JSD(one_dat, test.na = TRUE, unit = "log", est.prob = "empirical")

# write.csv(CL3,"F:/ploting/GEV PLOT/Youngsan/LSTM GEV/JSD LSTM Youngsan 95th distribution PDF.csv")

saveCsvFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "JSD LSTM Youngsan 95th distribution PDF")
write.csv(CL3, saveCsvFile)

str(one_dat)

for(i in 1:length(two)){
  
  one_list[[i]] = one %>% dplyr::filter(type == two[i]) %>% dplyr::select(cdf) %>% t()
  
}



one_dat = one_list %>% do.call(rbind, .)

CL4<-philentropy::JSD(one_dat, test.na = TRUE, unit = "log", est.prob = "empirical")

# write.csv(CL4,"F:/ploting/GEV PLOT/Youngsan/LSTM GEV/JSD LSTM Youngsan 95th distribution CDF.csv")

saveCsvFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "JSD LSTM Youngsan 95th distribution CDF")
write.csv(CL4, saveCsvFile)


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
# R을 이용한 판매량 데이터 예측 모형

#================================================
# 초기 환경변수 설정
# ================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0282"
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
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(dplyr)
library(data.table)
library(Rcpp)
library(philentropy)
library(h2o)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "판매량_예측.xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 2)

trainData = data %>% 
  dplyr::mutate(
    dtDate = readr::parse_date(date, "%Y-%m")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
    , dtXran = lubridate::decimal_date(dtDate)
  )

testData = tibble(dtDate = seq(as.Date("2018-09-01"), as.Date("2023-02-01"), "1 month")) %>% 
  dplyr::mutate(
    dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
    , dtXran = lubridate::decimal_date(dtDate)
  ) %>% 
  dplyr::filter(
    dtMonth %in% c(9, 10, 11)
  )

# ******************************************************************************
# 다중선형회귀모형
# ******************************************************************************
lmModel = lm(value ~ dtXran + dtYear + dtMonth, data = trainData)
summary(lmModel)

testData$prdLM = predict(lmModel, newdata = testData)

# ******************************************************************************
# 머신러닝 및 딥러닝 모형
# ******************************************************************************
# 초기화
# h2o::h2o.init()
# 
# # 모델 학습
# amlModel = h2o::h2o.automl(
#   x = c("dtXran", "dtYear", "dtMonth")
#   , y = c("value")
#   , training_frame = as.h2o(trainData)
#   , nfolds = 10
#   , sort_metric = "RMSE"
#   , stopping_metric = "RMSE"
#   , seed = 1
#   , max_models = 10
# )

summary(amlModel)

testData$prdDL = as.data.frame(h2o::h2o.predict(object = amlModel, newdata = as.h2o(testData)))$predict


# ******************************************************************************
# 에측 결과 저장
# ******************************************************************************
saveXlsxFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "판매량_예측결과")
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "예측 데이터")
openxlsx::writeData(wb, "예측 데이터", testData, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)


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
# R을 이용한 격자별 유출모형을 Fortran으로 코드 변환

#================================================
# 초기 환경변수 설정
# ================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0284"
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
# 2D 변환 작업
library(rgdal)
library(tidyr)
library(dplyr)
library(plot.matrix)
library(foreign)
library(tidyverse)
library(readr)
library(nml)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "rrrccc.asc"))
# RRRCCC <- read.asciigrid("rrrccc.asc")

RRRCCC <- sp::read.asciigrid(fileInfo)
test <- RRRCCC@data

DirLocal<-'D' 
DirBsn <-'220106'
Period<- '20210823_Test'         # 분석 기간 폴더 
DirSHP <- paste(DirLocal, ":/", DirBsn, "/SHP",sep="")
DirPeriod <- paste(DirLocal, ":/", DirBsn, "/RDR/", Period, sep="") 
DirTank <-  paste(DirLocal, ":/", DirBsn, "/RDR/", Period, "/05_Tank", sep="")  

# clip된 레이더 자료 list load
DirRDR <- paste(DirLocal, ":/", DirBsn, "/RDR/", Period, "/04_Clip",sep="") 
# setwd(DirRDR)
# filename<-list.files(path = ".", pattern =glob2rx("*cc_*asc*"), all.files = FALSE, full.names = FALSE, recursive = FALSE)
filename<-list.files(path = file.path(globalVar$inpPath, serviceName), pattern =glob2rx("*cc_*asc*"), all.files = FALSE, full.names = FALSE, recursive = FALSE)
timeList<-substr(basename(filename), nchar(basename(filename))-13, nchar(basename(filename))-4) 

# 유출 모형 매개변수 11개 
# a<-1000     #면적
# Qb<-15  ; s_ini<-6 ;  #기저유량 , #초기 저류고 
# a1<-0.05 ; a2<-0.15 ; a3<-0.3  #유출공계수
# h1<-25 ; h2<-5; h3<-8   #유출공높이
# b1<-0.35 ;b2<-0.21  #침투공계수

# # 유출모형
# tank_df<- function(data) {
#   
#   data$s1=data$s1+data$Rain
#   if (data$s1 >=h1){
#     q1<- (data$s1-h1)*a1
#     q2<- (data$s1-h2)*a2
#     inf1<-data$s1*b1
#     data$s1=data$s1-q1-q2-inf1
#   }else if (data$s1 <h1 & data$s1 >=h2 ){
#     q1<- 0
#     q2<- (data$s1-h2)*a2
#     inf1<-data$s1*b1
#     data$s1=data$s1-q1-q2-inf1
#   }else{
#     q1<- 0
#     q2<- 0
#     inf1<-data$s1*b1
#     data$s1=data$s1-q1-q2-inf1
#   }
#   data$s2=data$s2+inf1
#   if (data$s2 >=h3){
#     q3<- (data$s2-h3)*a3
#     inf2<-data$s2*b2
#     data$s2=data$s2-q3-inf2
#     total_q<-((q1+q2+q3)*a/3.6+Qb)
#   }else{
#     q3<- 0
#     inf2<-data$s2*b2
#     data$s2=data$s2-q3-inf2
#     total_q<-((q1+q2+q3)*a/3.6+Qb)
#   }
#   # data_result<- cbind(data$RRRCCC, data$Rain,data$s1, data$s2, total_q, q1, q2, q3, inf1, inf2)
#   data_result<- cbind(data$RRRCCC, data$Rain,data$s1, data$s2, total_q)
#   colnames(data_result)<-c("RRRCCC", "Rain", "s1", "s2", "Total_q")
#   return(data_result)
# }



# 유출분석
j = 1
for (j in 1:length(filename)){
  
  # setwd(DirRDR)
  time <- substr(basename(filename[j]), nchar(basename(filename))-13, nchar(basename(filename[j]))-4)
  
  fileInfoPattern = file.path(globalVar$inpPath, serviceName, filename[j])
  fileInfo <- Sys.glob(fileInfoPattern)
  
  if (length(fileInfo) < 1) {
    cat(sprintf("[ERROR] 입력자료가 없습니다. : %s", fileInfoPattern), "\n")
    next
  }
  
  R <- sp::read.asciigrid(fileInfo, as.image =FALSE, plot.image = TRUE, proj4string = CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996
+x_0=1000000 +y_0=2000000 +ellps=GRS80
+towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  rain <- R@data
  
  # setwd(DirTank)
  
  fileInfoStoPattern = file.path(globalVar$inpPath, serviceName, paste0('Sto1_',timeList[j], ".txt"))
  fileInfoSto = Sys.glob(fileInfoStoPattern)
  if (length(fileInfoSto) < 1) {
    cat(sprintf("[ERROR] 입력자료가 없습니다. : %s", fileInfoStoPattern), "\n")
    next
  }
  
  S1<- read.asciigrid(fileInfoSto, as.image=FALSE, colname=cbind("sto1"))
  
  fileInfoSto2Pattern = file.path(globalVar$inpPath, serviceName, paste0('Sto2_',timeList[j], ".txt"))
  fileInfoSto2 = Sys.glob(fileInfoSto2Pattern)
  if (length(fileInfoSto2) < 1) {
    cat(sprintf("[ERROR] 입력자료가 없습니다. : %s", fileInfoSto2Pattern), "\n")
    next
  }
  
  S2<- read.asciigrid(fileInfoSto2, as.image=FALSE, colname=cbind("sto2"))
  
  sto1 <- S1@data
  sto2 <- S2@data
  r1 <- cbind(test,rain, sto1, sto2)
  data<- r1 %>% drop_na()
  colnames(data)<-c("RRRCCC", "Rain", "s1", "s2")
  
  
  # result_df<-as.data.frame(tank_df(data))
  # result_df <- transform(result_df, Rain = sprintf("%.3f", result_df$Rain), s1 = sprintf("%.3f", result_df$s1), s2 = sprintf("%.3f", result_df$s2), Total_q = sprintf("%.3f", result_df$Total_q))
  # 
  # # write.table(x=data, quote = FALSE, row.names = FALSE, file=paste('RDR_',timeList[j+1], '.txt', sep=""))
  # saveTxtFile = sprintf("%s/%s_%s.txt", globalVar$outPath, serviceName, paste0('RDR_',timeList[j+1]))
  # write.table(x=data, quote = FALSE, row.names = FALSE, file=saveTxtFile)
  # 
  # 
  # flow<- select(result_df, RRRCCC, Rain, Total_q)
  # # write.table(x=flow, quote = FALSE, row.names = FALSE, file=paste('Outflow_', timeList[j], '.txt', sep=""))
  # 
  # saveTxtFile = sprintf("%s/%s_%s.txt", globalVar$outPath, serviceName, paste0('Outflow_', timeList[j]))
  # write.table(x=flow, quote = FALSE, row.names = FALSE, file=saveTxtFile)
  # 
  # storage<- select(result_df, RRRCCC, s1, s2)
  # # write.table(x=storage, quote = FALSE, row.names = FALSE, file=paste('Sto_',timeList[j+1], '.txt', sep=""))
  # saveTxtFile = sprintf("%s/%s_%s.txt", globalVar$outPath, serviceName, paste0('Sto_',timeList[j+1]))
  # write.table(x=storage, quote = FALSE, row.names = FALSE, file=saveTxtFile)
  
  
  # ****************************************************************************
  # Fortran 소스코드 수행
  # ****************************************************************************
  # 포트란 경로 입력 (수동 입력)
  # srcPath = "E:/04. TalentPlatform/Github/TalentPlatform-R/src/fortran"
  srcPath = file.path(".", "src", "fortran")
  srcFile = file.path(srcPath, "RunFortran.f90")
  exeFile = file.path(srcPath, "a.exe")
  
  # 입력자료 저장
  saveInpFile = sprintf("%s/%s_%s_%s.txt", srcPath, serviceName, "input", timeList[j])
  utils::write.table(data, file = saveInpFile, col.names = FALSE, row.names = FALSE)
  
  cat(sprintf("[CHECK] saveInpFile : %s", saveInpFile), "\n")
  
  # 출력자료 
  saveOutflowFile = sprintf("%s/%s_%s_%s.txt", srcPath, serviceName, 'Outflow', timeList[j])
  saveStoFile = sprintf("%s/%s_%s_%s.txt", srcPath, serviceName, 'RDR', timeList[j])
  saveRdrFile = sprintf("%s/%s_%s_%s.txt", srcPath, serviceName, 'Sto', timeList[j])
  
  # 템플릿 네임리스트 파일 정보
  nmlFileInfo = Sys.glob(file.path(srcPath, "TEMPLATE_namelistInfo.nml"))
  
  # 출력 네임리스트 파일
  saveNmlFile = sprintf("%s/%s_%s.nml", srcPath, serviceName, "namelistInfo")
  
  # 템플릿 네임리스트 파일 읽기 그리고 동적 변수 (입력자료, 출력자료) 치환
  # 출력 네임리스트 파일 저장
  readLines(nmlFileInfo) |>
    stringr::str_replace(pattern = "%inpFile", replace = saveInpFile) |>
    stringr::str_replace(pattern = "%outflowFile", replace = saveOutflowFile) |>
    stringr::str_replace(pattern = "%stoFile", replace = saveStoFile) |>
    stringr::str_replace(pattern = "%rdrFile", replace = saveRdrFile) |>
    writeLines(con = saveNmlFile)
  
  inpNmlFile = sprintf("%s/%s_%s.txt", globalVar$outPath, serviceName, "input")
  
  # 포트란 컴파일
  # gfortran ./fortran/RunFortran.f90
  system(paste(
    "gfortran"
    , srcFile
  ))
  
  # 포트란 실행 및 입력자료 (네임리스트 파일)
  # ./fortran/a.out ./fortran/LSH0284_namelistInfo.nml
  system(paste(
    exeFile
    , saveNmlFile
  ))
  
  # 포트란에서 출력 결과를 R에서 읽기
  outflowData = readr::read_table(file = saveOutflowFile, col_names = FALSE)
  stoData = readr::read_table(file = saveStoFile, col_names = FALSE)
  rdrFile = readr::read_table(file = saveRdrFile, col_names = FALSE)
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
# R을 이용한 선거 데이터 (서울특별시 용산구) 3단계 시각화 및 도표 삽입
# R을 이용한 선거 데이터 (경상남도 남해군) 3단계 시각화 및 도표 삽입

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
# serviceName = "LSH0214"
serviceName = "LSH0287"

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
library(readxl)
library(ggplot2) 
library(grid)
library(gridExtra) 
library(cowplot) 

#=================================================
# 선거 주제도
#=================================================
# 선거 데이터 읽기
# addrName = "서울특별시"
# addrDtlName = "용산구"

addrName = "경상남도"
addrDtlName = "남해군"

# addrName = "경기도"
# addrDtlName = "안성시"

pick(fileInfo)

fileInfoPattern = sprintf("선거분석 (%s %s).xlsx", addrName, addrDtlName)
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileInfoPattern))
# data = openxlsx::read.xlsx(fileInfo, sheet = 1)
# data = readxl::read_excel(fileInfo, sheet = 1)
data = xlsx::read.xlsx(fileInfo, sheetIndex = 1, encoding="UTF-8")

# 세부 투표구에 대한 위/경도 반환
# dataGeoL1 = dataGeo %>% 
#   dplyr::mutate(
#     addr = stringr::str_c(주소, 건물명, sep = " ")
#   )

# addrList = dataGeoL1$addr%>% unique %>% sort %>%
#   as.tibble()

# saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "서울시 강서구 투표구 정보")

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

# addrData = readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

# dataGeoL2 = dataGeoL1 %>% 
#   dplyr::left_join(addrData, by = c("addr" = "value"))

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

dataL3 = data %>% 
  dplyr::filter(세부투표구 %in% c("소계")) %>% 
  rowwise(투표구) %>% 
  dplyr::mutate(
    maxVal = max(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , val = dplyr::case_when(
      자유한국당 == maxVal ~ 1
      , 더불어민주당 == maxVal ~ 2
      , 중도층 == maxVal ~ 3
      )
    , 투표구2 = dplyr::case_when(
      # stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
      # , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
      # , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
      # , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
      # , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
      # , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
      # , TRUE ~ 투표구
      
      grepl("원효로제1동", 투표구) ~ "원효로1동"
      , grepl("원효로제2동", 투표구) ~ "원효로2동"
      , grepl("이촌제1동", 투표구) ~ "이촌1동"
      , grepl("이촌제2동", 투표구) ~ "이촌2동"
      , grepl("이태원제1동", 투표구) ~ "이태원1동"
      , grepl("이태원제2동", 투표구) ~ "이태원2동"
      , TRUE ~ 투표구
    )
  )

# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(CRS("+proj=longlat"))

# 법정동 코드 읽기 (2)
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

stringr::str_detect(codeData$시도명칭, regex("경상북도")) %>% 
  unique()


codeDataL1 = codeData %>%
  dplyr::filter(
  # stringr::str_detect(시도명칭, regex(addrName))
  # , stringr::str_detect(시군구명칭, regex(addrDtlName))
   grepl(addrName, 시도명칭)
   , grepl(addrDtlName, 시군구명칭)
  ) 


codeData$시도명칭 %>% unique()

# 통합 데이터셋
dataL5 = mapGlobal %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
  dplyr::left_join(dataL3, by = c("adm_dr_nm" = "투표구2")) 



# ************************************************
# 선거 주제도
# ************************************************
plotSubTitle = sprintf("%s %s 선거 주제도",addrName, addrDtlName)
saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

# ggplotDefaultColor = hue_pal()(3)
ggplotDefaultColor = c("red", "blue", "grey")


ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  # geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 0.3) +
  geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 1.0, color = "white") +
  geom_sf_text(data = dataL5, aes(label = 읍면동명칭), color = "white") +
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
    # , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[2], "3" = ggplotDefaultColor[3])
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  scale_color_manual(
    name = NULL
    , na.value = "transparent"
    # , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
    , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[2], "3" = ggplotDefaultColor[3])
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  # xlim(127.80, 128.08) +
  # ylim(34.69, 34.95) + 
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
  ggsave(filename = saveImg2, width = 8, height = 8, dpi = 600)


# ************************************************
# 선거 빈도분포
# ************************************************
dataDtlL4 = data %>% 
  dplyr::filter(! 세부투표구 %in% c("소계")) %>% 
  # rowwise(투표구) %>%
  dplyr::mutate(
    투표구2 = dplyr::case_when(
      stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
      , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
      , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
      , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
      , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
      , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
      , TRUE ~ 투표구
    )
    , label = str_match_all(세부투표구, "제[[:digit:]]+투") %>% unlist()
  ) %>% 
  dplyr::na_if(0) %>% 
  dplyr::select(투표구2, 세부투표구, `%자유한국당`, `%더불어민주당`, `%중도층`, label) %>% 
  tidyr::gather(-c(투표구2, 세부투표구, label), key = "key", value = "val") 


# 정당에 따른 정렬
dataDtlL4$key = forcats::fct_relevel(dataDtlL4$key, rev(c("%자유한국당", "%더불어민주당", "%중도층")))

selLabel = paste0("제", c(1:99), "투")
dataDtlL4$label = forcats::fct_relevel(dataDtlL4$label, selLabel)
# dataDtlL4$label = forcats::fct_relevel(dataDtlL4$label, rev(selLabel))

plotSubTitle = sprintf("%s %s 선거 빈도분포", addrName, addrDtlName)
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(dataDtlL4, aes(x = label, y = val, fill = key, group = key, label = round(val, 0))) +
  geom_bar(position = position_stack(), stat = "identity") +
  # geom_bar(stat = "identity") +
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
    # , values = c("%자유한국당" = ggplotDefaultColor[1], "%더불어민주당" = ggplotDefaultColor[3], "%중도층" = "gray")
    , values = c("%자유한국당" = ggplotDefaultColor[1], "%더불어민주당" = ggplotDefaultColor[2], "%중도층" = ggplotDefaultColor[3])
    , labels = c("자유한국당", "더불어민주당", "중도층")
  ) +
  # facet_wrap(~투표구2, scale = "free", ncol = 3) +
  facet_wrap(~투표구2, scale = "free", ncol = 4) +
  ggsave(filename = saveImg, width = 16, height = 12, dpi = 600)


# ************************************************
# 스토리 보드
# ************************************************
# # 빈도분포
# ggFreqPlot = ggplot(dataDtlL4, aes(x = label, y = val, fill = key, group = key, label = round(val, 0))) +
#   geom_bar(position = position_stack(), stat = "identity") +
#   geom_text(position = position_stack(vjust = 0.5), size = 3, color = "white") +
#   coord_flip() +
#   labs(x = NULL, y = NULL, fill = NULL, subtitle = NULL) +
#   theme(
#     text = element_text(size = 9)
#     , legend.position = "none"
#     , axis.ticks.x = element_blank()
#     , axis.text.x = element_blank()
#   ) +
#   scale_fill_manual(
#     name = NULL
#     , na.value = "transparent"
#     , values = c("자유한국당" = ggplotDefaultColor[1], "더불어민주당" = ggplotDefaultColor[3], "중도층" = "gray")
#     , labels = c("자유한국당", "더불어민주당", "중도층")
#   ) +
#   facet_wrap(~투표구, scale = "free", ncol = 4)
# 

# # 지도
# ggMapPlot = ggplot() +
#   theme_bw() +
#   coord_fixed(ratio = 1) +
#   geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 0.3) +
#   geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
#   # geom_point(data = dataDtlL3, aes(x = lon, y = lat, color = factor(val)), shape = 16, show.legend = FALSE) +
#   # ggrepel::geom_label_repel(
#   #   data = dataDtlL3
#   #   , aes(x = lon, y = lat, fill = factor(val), label = label)
#   #   , color = "white"
#   #   , segment.color = "black"
#   #   , show.legend = FALSE
#   #   , segment.size = 0.2
#   #   , size = 3
#   # ) +
#   scale_fill_manual(
#     name = NULL
#     , na.value = "transparent"
#     , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
#     , labels = c("자유한국당", "더불어민주당", "중도층")
#   ) +
#   scale_color_manual(
#     name = NULL
#     , na.value = "transparent"
#     , values = c("1" = ggplotDefaultColor[1], "2" = ggplotDefaultColor[3], "3" = "gray")
#     , labels = c("자유한국당", "더불어민주당", "기타야당")
#   ) +
#   labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL)
# 
# ggMapPlotTheme = theme(
#   text = element_text(size = 16)
#   , panel.grid.major.x = element_blank()
#   , panel.grid.major.y = element_blank()
#   , panel.grid.minor.x = element_blank()
#   , panel.grid.minor.y = element_blank()
#   , axis.text.x = element_blank()
#   , axis.ticks.x = element_blank()
#   , axis.title.x = element_blank()
#   , axis.text.y = element_blank()
#   , axis.ticks.y = element_blank()
#   , axis.title.y = element_blank()
#   , plot.subtitle = element_text(hjust = 1)
#   , legend.position = "top"
# )
# 
# 
# plotSubTitle = sprintf("%s", "서울특별시 강서구 선거 통합도")
# saveImgMerge = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# 
# (ggMapPlot & ggMapPlotTheme ) / (ggFreqPlot) +
#   patchwork::plot_layout(heights = c(2, 1)) +
#   ggsave(filename = saveImgMerge, width = 10, height = 20, dpi = 600)


#=================================================
# 인구현황
#=================================================
# [행정안전부] 주민등록 인구통계 : https://jumin.mois.go.kr/
# [검색조건] 연령별 인구현황
#   행정구역 (경상북도, 남해군)
#   등록구분 (거주자)
#   연간 (2021년)
#   구분 (남/여 구분)
#   연령 구분 단위 (1세)
#   만 연령구분 (0, 100이상)
# [다운로드] csv 파일 다운로드

addrName = "경상남도"
addrDtlName = "남해군"

# 선거 데이터 읽기
fileInfoPattern = sprintf("선거분석 (%s %s).xlsx", addrName, addrDtlName)
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileInfoPattern))
data = xlsx::read.xlsx(fileInfo, sheetIndex = 2, encoding = "UTF-8")

# fileInfoPattern = sprintf("선거분석 (%s %s).csv", addrName, addrDtlName)
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileInfoPattern))
# data = readr::read_csv(file = fileInfo)

dataL1 = data %>%
  as.tibble() %>%
  na.omit() %>%
  readr::type_convert()

sexListPattern = c("남", "여", "남|여")
# sexInfoPattern = "남"
saveDataL1 = tibble::tibble()

for (sexInfoPattern in sexListPattern) {
  
  # 한글변환 문제
  # sexInfo = stringr::str_replace(sexInfoPattern, "\\|", "") %>% unlist()
  sexInfo = gsub("\\|", "", sexInfoPattern)
  
  dataL2 = dataL1 %>%
    dplyr::select(투표구, tidyselect::matches("[[:digit:]]+세")) %>% 
    tidyr::gather(-투표구, key = "key", value = "투표수") %>% 
    dplyr::mutate(
      age = stringr::str_match_all(key, "[[:digit:]]+") %>% unlist()
      # , sex = stringr::str_match_all(key, "^남|여") %>% unlist()
      , sex = gsub("[^남|여]", "", key) %>% stringr::str_trim(side = c("both"))
      # , isSex = dplyr::case_when(
      #   stringr::str_detect(sex, regex(sexInfoPattern)) ~ TRUE
      #   , TRUE ~ FALSE
      # )
      , isSex = grepl(sexInfoPattern, sex)
    ) %>% 
    dplyr::filter(isSex == TRUE) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        16 <= age & age <= 20 ~ "16-20세"
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
    dplyr::select(-age)

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
  
  dataL4 = statData %>% 
    dplyr::left_join(statDataL2, by = c("투표구" = "투표구")) %>% 
    tidyr::spread(key = "type", value = "sumKeyVal") %>% 
    dplyr::mutate(
      투표구2 = dplyr::case_when(
        stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
        , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
        , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
        , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
        , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
        , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
        , TRUE ~ 투표구
      )
    )
  
  saveData = dataL4 %>%
    dplyr::rename(
      합계 = sumVal
    ) %>% 
    dplyr::mutate(
      성별 = sexInfo
    )
  
  # 데이터 병합
  saveDataL1 = dplyr::bind_rows(saveDataL1, saveData)
  
  
  # ****************************************************************************
  # 인구현황 배경지도
  # ****************************************************************************
  typeList = dataL4$투표구2 %>% unique() %>% sort()
  
  plotSubTitle = sprintf("%s %s 인구현황 막대 (%s)", addrName, addrDtlName, sexInfo)
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
  saveTmp = tempfile(fileext = ".png")
  
  makePlotBg = ggplot() +
    theme_bw() +
    coord_fixed(ratio = 1) +
    geom_sf(data = dataL5, fill = NA, inherit.aes = FALSE) +
    geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
    labs(
      x = NULL
      , y = NULL
      , color = NULL
      , fill = NULL
      , subtitle = plotSubTitle
    ) +
    xlim(127.80, 128.08) +
    ylim(34.69, 34.95) +
    theme(
      text = element_text(size = 14)
      , panel.border = element_blank()
      , panel.grid = element_blank()
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
      , plot.margin = unit(c(0.2, 0, 0, 0), "lines")
    )
  
  ggsave(makePlotBg, filename = saveTmp, width = 8, height = 8, dpi = 600)
  fs::file_copy(saveTmp, saveImg, overwrite = TRUE)

  
  # ****************************************************************************
  # 인구현황 막대 그래프
  # ****************************************************************************
  # typeInfo = typeList[1]
  for (typeInfo in typeList) {
    
    dataL6 = dataL4 %>%
      dplyr::ungroup() %>%
      dplyr::filter(투표구2 == typeInfo) %>%
      dplyr::select(-c("투표구", "sumVal")) %>%
      tidyr::gather(-투표구2, key = "key", value = "val") %>% 
      dplyr::mutate(
        per = val / sum(val, na.rm = TRUE) * 100.0
      )
    
    cbSet1 = RColorBrewer::brewer.pal(7, "Set1")
    
    plotSubTitle = sprintf("%s %s 인구현황 막대차트 (%s, %s)", addrName, addrDtlName, sexInfo, typeInfo)
    saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
    saveTmp = tempfile(fileext = ".png")
    
    makePlot = ggplot(dataL6, aes(x=key, y=per, group=key, fill=key, label = round(per, 0))) +
      geom_bar(position="dodge", stat="identity", show.legend = FALSE, alpha = 1) +
      geom_text(nudge_y = -1.50, color = "black", size = 3) +
      labs(
        x = NULL
        , y = NULL
        , color = NULL
        , fill = NULL
        , subtitle = NULL
      ) +
      scale_y_continuous(breaks = seq(0, 50, 10), minor_breaks = NULL) +
      scale_fill_manual(
        name = NULL
        , na.value = "transparent"
        , values = c("16-20세" = cbSet1[1], "21-30세" = cbSet1[2], "31-40세" = cbSet1[3], "41-50세" = cbSet1[4], "51-60세" = cbSet1[5], "61-70세" = cbSet1[6], "71세 이상" = cbSet1[7])
        , labels = c("16-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
      ) +
      theme_bw() +
      theme(
        text = element_text(size = 16)
        , panel.border = element_blank()
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
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent", color = NA)
        , legend.background = element_rect(fill = "transparent")
        , legend.box.background = element_rect(fill = "transparent")
      )
    
    ggsave(makePlot, filename = saveTmp, width = 2, height = 2, dpi = 600)
    
    fs::file_copy(saveTmp, saveImg, overwrite = TRUE)
  }
}


saveXlsxFile = sprintf("%s/%s_%s_%s_%s.xlsx", globalVar$outPath, serviceName, addrName, addrDtlName, "인구현황")

wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "(결과)인구현황")
openxlsx::writeData(wb, "(결과)인구현황", saveDataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)


# ****************************************************************************
# 범례 그리기
# ****************************************************************************
legendData = tibble::tibble(
  x = 1:7
  , y = 1:7
  , legend = c("16-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
)

plotSubTitle = "범례 정보"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
saveTmp = tempfile(fileext = ".png")

makePlotLegend = ggplot(legendData, aes(x = x, y = y, fill = legend)) +   
  geom_bar(position="dodge", stat="identity", rsize = 7) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("16-20세" = cbSet1[1], "21-30세" = cbSet1[2], "31-40세" = cbSet1[3], "41-50세" = cbSet1[4], "51-60세" = cbSet1[5], "61-70세" = cbSet1[6], "71세 이상" = cbSet1[7])
    , labels = c("16-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
  ) 

getLegend = cowplot::get_legend(makePlotLegend)                    
grid::grid.newpage()
png(file = saveTmp, width = 2, height = 2, units = "in", res = 600, bg = "transparent")
grid::grid.draw(getLegend)
dev.off()
fs::file_copy(saveTmp, saveImg, overwrite = TRUE)



# ****************************************************************************
# 인구현황 파이 그래프
# ****************************************************************************
for (sexInfoPattern in sexListPattern) {
  
  # 한글변환 문제
  # sexInfo = stringr::str_replace(sexInfoPattern, "\\|", "") %>% unlist()
  sexInfo = gsub("\\|", "", sexInfoPattern)
  
  dataL2 = dataL1 %>%
    dplyr::select(투표구, tidyselect::matches("[[:digit:]]+세")) %>% 
    tidyr::gather(-투표구, key = "key", value = "투표수") %>% 
    dplyr::mutate(
      age = stringr::str_match_all(key, "[[:digit:]]+") %>% unlist()
      # , sex = stringr::str_match_all(key, "^남|여") %>% unlist()
      , sex = gsub("[^남|여]", "",key) %>% stringr::str_trim(side = c("both"))
      # , isSex = dplyr::case_when(
      #   stringr::str_detect(sex, regex(sexInfoPattern)) ~ TRUE
      #   , TRUE ~ FALSE
      # )
      , isSex = grepl(sexInfoPattern, sex)
    ) %>% 
    dplyr::filter(isSex == TRUE) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        16 <= age & age <= 20 ~ "16-20세"
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
    dplyr::select(-age)
  
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
  
  dataL4 = statData %>% 
    dplyr::left_join(statDataL2, by = c("투표구" = "투표구")) %>% 
    tidyr::spread(key = "type", value = "sumKeyVal") %>% 
    dplyr::mutate(
      투표구2 = dplyr::case_when(
        stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
        , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
        , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
        , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
        , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
        , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
        , TRUE ~ 투표구
      )
    )
  
  saveData = dataL4 %>%
    dplyr::rename(
      합계 = sumVal
    ) %>% 
    dplyr::mutate(
      성별 = sexInfo
    )
  
  codeDataL1 = codeData %>%
    dplyr::filter(
      # stringr::str_detect(시도명칭, regex("서울특별시")), stringr::str_detect(시군구명칭, regex("용산구"))
      stringr::str_detect(시도명칭, regex(addrName)), stringr::str_detect(시군구명칭, regex(addrDtlName))
    ) 
  
  # 통합 데이터셋
  dataL5 = mapGlobal %>%
    dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
    dplyr::left_join(dataL4, by = c("adm_dr_nm" = "투표구2")) 
  
  # 중심 위/경도 반환
  posData = sf::st_centroid(dataL5$geometry) %>% 
    sf::st_coordinates() %>% 
    as.tibble() %>% 
    dplyr::rename(
      "lon" = "X"
      , "lat" = "Y"
    )
  
  dataL6 = dplyr::bind_cols(dataL5, posData) %>% 
    dplyr::mutate(
      xOffset = dplyr::case_when(
        읍면동명칭 == "대덕면" ~ -0.02
        , 읍면동명칭 == "금광면" ~ 0.02
        , 읍면동명칭 == "안성1동" ~ 0.025
        , TRUE ~ 0
      )
      , yOffset = dplyr::case_when(
        읍면동명칭 == "안성3동" ~ 0.01
        , TRUE ~ 0
      )
    )
 
  dataL7 = na.omit(dataL6)
  
  dataL8 = dataL7 %>% 
    as.tibble() %>% 
    dplyr::mutate(
      geometry = NULL
    )
  
  # ggplotDefaultColor = c("red", "blue", "grey")
  
  plotSubTitle2 = sprintf("%s %s 인구현황 파이차트 (%s)", addrName, addrDtlName, sexInfo)
  saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle2)
  
  makePiePlot = ggplot() +
    theme_bw() +
    coord_fixed(ratio = 1) +
    geom_sf(data = dataL7, fill = NA, inherit.aes = FALSE) +
    geom_sf_text(data = dataL7, aes(label = 읍면동명칭)) +
    # 크기 비율 X
    scatterpie::geom_scatterpie(
      aes(x = lon + xOffset, y = lat + yOffset, group = factor(읍면동명칭), r = 0.025)
      # aes(x = lon, y = lat, group = factor(읍면동명칭), r = 0.025)
      , cols=c("16-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
      , data = dataL8, color = NA, alpha = 0.8
      # , data = dataL8, color = NA, alpha = 0.75
    ) +
    # 크기 비율 O
    # scatterpie::geom_scatterpie(
    #   aes(x = lon, y = lat, group = factor(읍면동명칭), r = sumVal/5000000)
    #   , cols=c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
    #   , data = dataL8, color = NA, alpha = 0.75
    # ) +
    scatterpie::geom_scatterpie_legend(
      dataL8$sumVal/2000000
      # , x =  min(posData$lon, na.rm = TRUE)
      # , y = min(posData$lat, na.rm = TRUE)
      , x =  min(posData$lon, na.rm = TRUE) - 0.02
      , y = min(posData$lat, na.rm = TRUE) - 0.02
      # , x =  127.80 + 0.02
      # , y = 34.69 + 0.02
    ) +
    labs(
      x = NULL
      , y = NULL
      , color = NULL
      , fill = NULL
      , subtitle = plotSubTitle2
    ) +
    # scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) +
    # xlim(127.80, 128.08) + 
    # ylim(34.69, 34.95) + 
    scale_fill_brewer(palette = "Set1") +
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
      , plot.margin = unit(c(0.2, 0, 0, 0), "lines")
    )
  
    ggsave(makePiePlot, filename = saveImg2, width = 8, height = 8, dpi = 600)
    
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
# R을 이용한 행렬 수식 계산 및 ROC 시각화

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0289"

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
library(rockchalk)
library(tidyverse)
library(readr)
library(tidyverse)
library(readr)
library(ROCit)
library(ggplot2)
library(pROC)


# ******************************************************************************
# 행렬 수식 계산
# ******************************************************************************
# id <- c(1,1,2,2,2,3,3,3,3,3,4,4,5,5,6,6,6,7,8,8,8,8,8,9,9,9,10,10)
# a <-table(id)
# 
# ar <- function(n, rho) {
#   exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - (1:n - 1))
#   rho^exponent
# }
# 
# cor <- function(n){
#   ar_n <- 0.5*ar(n,0.3)
#   delta_n <- diag(sqrt(2),n)
#   cor_n <- t(delta_n)%*%ar_n%*%delta_n
#   cor_n
# }
# 
# 
# makeMat = function(idList) {
#   
#   id = c(1,1,2,2,2,3,3,3,3,3,4,4,5,5,6,6,6,7,8,8,8,8,8,9,9,9,10,10)
#   idTab = table(id)
#   refIdTab = data.frame(idTab)
#   
#   idList = 1:3
#   
#   data = data.frame()
#   for (i in idList) {
#     idDtlList = refIdTab[i, ]$Freq
#     for (j in 1:idDtlList) {
#       
#       if (i == 1) {
#         matVal = rnorm(10, 2, 0.5)
#       } else {
#         matVal = rockchalk::mvrnorm(10, rep(2,i-1), cor(i-1))
#       }
#       
#       tmpData = data.frame(
#         id = i
#         , t(matVal)
#       )    
#       
#       data = dplyr::bind_rows(data, tmpData)
#     }
#   }
#   
#   result = as.matrix(data)
#   
#   return(result)
# }
# 
# 
# # 1:2에 대한 실행
# mat = makeMat(1:2)
# print(mat)
# 
# # 1:4에 대한 실행
# mat = makeMat(1:4)
# print(mat)


# ******************************************************************************
# 행렬 수식 계산 (보완)
# ******************************************************************************
# sim_1.csv 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "sim_1.csv"))
inpData = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR")) 

#AR(1) function
ar <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - (1:n - 1))
  rho^exponent
}

#variance-covariance matrix function
cor <- function(n){
  ar_n <- 0.75*ar(n,0.5)
  delta_n <- diag(sqrt(4),n)
  cor_n <- t(delta_n)%*%ar_n%*%delta_n
  cor_n
}

makeMat <-  function(inpData) {
  
  set.seed(123456)
  data <- data.frame()
  
  # data에서 1줄씩 읽기
  
  tabData = table(inpData$cluster1)
  
  # for (j in 1:100) { # 테스트
  for (i in 1:nrow(inpData)) { # 실전
    rowData = inpData[j, ]
    # summary(rowData)
    
    # 해당 컬럼 정보 가져오기
    clu = rowData$cluster1
    chInfo = rowData$CHR
    isFlag = rowData$TF
    
    for (i in 1:chInfo) {
      
      if (i==1) {
        matVal <- mvrnorm(30, rep(0,1), 0.3*ar(1,0.5))
      }else if(i==2){
        matVal <- mvrnorm(30, rep(0,2), 0.3*ar(2,0.5))
      }else {
        #T가  아닌 cluster1는  matVal <- mvrnorm(30, rep(0,i), 0.3*ar(i,0.5)) 
        #이때 i는 cluster의 table 결과
        #T인 경우 cluster1는  matVal <- mvrnorm(30, rep(0,i), cor(i)) 
        #이때 i는 cluster의 table 결과  
        
        if (isFlag == TRUE) {
          matVal <- mvrnorm(30, rep(0,i), cor(i))
        } else {
          matVal <- mvrnorm(30, rep(0,i), 0.3*ar(i,0.5)) 
        }
      }
      
      Data <- data.frame(t(matVal))    
      data <- dplyr::bind_rows(data, Data)
    }
  }
  
  result <-  as.matrix(data)
  return(result)
}


mat = makeMat(inpData)
print(mat)




# ******************************************************************************
# ROC 시각화
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Output.csv"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

# 일단 제가 보낸 파일에서 fwerArea로 roc커브를 그려야하는데
# 이때 true는 L이 3이상 15이하인것 중에
# 5개로 무작위로 뽑아요

tmpData = data %>% 
  tibble::rowid_to_column() %>% 
  dplyr::filter(dplyr::between(L, 3, 15)) %>% 
  dplyr::sample_n(5) %>% 
  dplyr::mutate(isFlag = TRUE)


# tp:L# tp: true이면서 fwer<0.05도 만족한 값이구요
# fp:fwer<0.05인데 false인경우
# fn:true인데 fwer>=0.05
# tn:false인데 fwer>=0.05
dataL1 = data %>% 
  tibble::rowid_to_column() %>% 
  dplyr::left_join(tmpData, by = c("rowid" = "rowid"), suffix = c("", ".tmp")) %>% 
  dplyr::mutate(
    label = dplyr::case_when(
      fwerArea < 0.05 & isFlag == TRUE ~ TRUE
      , TRUE ~ FALSE
      )
    )


dataL1$label = as.factor(dataL1$label)

# 요약 결과
summary(rocRes)



# 이때 x축은 0.00으로 시작해서 0.01씩 커지게 해주시고 false positive라고 적어주세요
# y축은 1,2,3,4이런 빈도가 나오도록 해주시고 true positive라고 적어주세요!

# ROC 곡선
rocRes = pROC::roc(label ~ fwerArea, data = dataL1, ci = TRUE)

mainTitle = "ROC 곡선"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)

pROC::ggroc(rocRes, size = 1, legacy.axes = TRUE) +
  geom_abline(color = "dark grey", size = 0.5) +
  labs(x = "False Positive", y = "True Positive", subtitle = mainTitle) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  # scale_x_continuous(breaks = c(0, 0.01, 0.05, 0.1)) +
  # scale_y_continuous(breaks = c(0, 0.01, 0.02, 0.3)) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ROC 곡선2
rocRes = ROCit::rocit(score = dataL1$fwerArea, class = dataL1$label)

mainTitle = "ROC 곡선2"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(rocRes, main = mainTitle)

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
# R을 이용한 기상 정보, 고도, 종분포 간의 GAM 회귀분석

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0290"

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
library(rockchalk)
library(tidyverse)
library(readr)
library(tidyverse)
library(readr)
library(ROCit)
library(mgcv)
library(mgcViz)


# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Jiri_real+final.csv"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Jiri산.csv"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))


# 다시한 번 말씀드리면 각 수종의 juvenile&adult를 구분하였고, 한 수종당 adult, juvenile 고도 그래프가 1개씩 총 2개가 나와야합니다!! 
# 또한 gps 자료를 통해 기상과 종분포와의 관계를 예측 및 파악하고 싶습니다!!
grpList = data$Groups %>% unique() %>% sort()
speList = data$Species %>% unique() %>% sort()

# data$Lat %>% unique() %>% sort()
# data$Lon %>% unique() %>% sort()

# grpInfo = "adult"
# speInfo = "구상"
speInfo = "고로쇠"

for (grpInfo in grpList) {
  for (speInfo in speList) {
    
    dataL1 = data %>%
      dplyr::filter(
        Groups == grpInfo
      )

    # data$Groups = factor(data$Groups)
    # data$Species = factor(data$Species)
    data$grpFac = ifelse(data$Groups == grpInfo, 1, 0)
    data$speFac = ifelse(data$Species == speInfo, 1, 0)
    
    dataL1$speFac = ifelse(dataL1$Species == speInfo, 1, 0)
    
    
    gamModel = mgcv::gam(
      Groups == grpInfo & Species == speInfo ~ s(Elevation)
      , data = data
      , family = gaussian
    )
    
    
    # 종속 변수가 0 아니면 1인 경우: Logistic regression
    # 종속 변수가 순위나 선호도와 같이 순서만 있는 데이터인 경우: Ordinal regression
    # 종속 변수가 개수(count)를 나타내는 경우: Poisson regression
    gamModel = mgcv::gam(
      grpFac == 1 % speFac == 1 ~ s(Elevation)
      , data = data
      , family = gaussian
      # , family = binomial
      # , method = "REML"
      # , method = "ML"
      # , method = "P-ML"
      # , method = "P-REML"
    )
    
    # Gaussian distribution
    gamModel = mgcv::gam(
      speFac == 1 ~ s(Elevation)
      , data = dataL1
      # , family = binomial
      , family = gaussian
      # , method = "REML"
      # , method = "ML"
      # , method = "P-ML"
      # , method = "P-REML"
    )
    

    mainTitle = sprintf("Groups 및 Species에 따른 Elevation 결과 (%s, %s)", grpInfo, speInfo)
    # saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
    saveTmp = tempfile()
    
    png(file = saveTmp, width = 10, height = 8, units = "in", res = 600, pointsize = 25)
    
    print(
      plot(mgcViz::getViz(gamModel)) +
        scale_x_continuous(breaks = seq(0, 2400, 400)) +
        # scale_y_continuous(breaks = c(seq(-0.6, 1, 0.4), 0)) +
        xlim(0, 2400) +
        # ylim(-0.6, 1) +
        theme_bw() +
        labs(subtitle = mainTitle, x = "Elevation (m)", y = "Normalized probability of occupancy") +
        theme(text = element_text(size = 18))
      , pages = 1
    )
    
    dev.off()
    
    fs::file_copy(saveTmp, saveImg, overwrite = TRUE)
    
    
  }
}



# plot(gamModel, select = 1)

# Groups == grpInfo & 
# 
#     gamModel = mgcv::gam(
#       Species == speInfo ~ s(Elevation)
#       , data = data
#       , family = binomial
#       , method = "REML"
#     )
#     
#     plot(gamModel, allTerms = T)
#     
#     plot(gamModel, select = 1) + l_dens(type = "cond") + l_fitLine() + l_ciLine()
#     
#     
#     plot(getViz(gamModel))

# 
# mgcViz::check.gamViz(getViz(gamModel))
# mgcViz::gridPrint(getViz(gamModel))
# mgcViz::check0D(getViz(gamModel))
# mgcViz::check2D(getViz(gamModel))
# mgcViz::plot.gamViz(getViz(gamModel))
# mgcViz::plot.sos.smooth(getViz(gamModel))
# 
# 
# dat <- gamSim(1,n=1e3,dist="normal",scale=2)
# dat$fac <- as.factor( sample(letters[1:6], nrow(dat), replace = TRUE) )
# b <- gam(y~s(x0)+s(x1, x2)+s(x3)+fac, data=dat)
# # To plot all the effects we do:
#   
#   b <- getViz(b)
# print(plot(b, allTerms = T), pages = 1) # Calls print.plotGam()
# 


# summary(gamModel)

# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Species에 따른 Groups 결과 (adult=0, juvenile=1)")
# png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
# plot(mgcViz::getViz(gamModel), select = 2)
# dev.off()