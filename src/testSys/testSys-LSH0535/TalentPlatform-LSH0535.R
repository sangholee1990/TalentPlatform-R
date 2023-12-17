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
# R을 이용한 SpatialEpi 실습 환경 구축 및 주석 설명

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0535"

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
library(SpatialEpi)
library(proj)
library(maps)
# install.packages("maptools", repos="http://R-Forge.R-project.org")
library(maptools)

# =========================================================================
# 2.1 Converting Dierent Map Formats into SpatialPolygons
# =========================================================================
# 2.1.1 Converting Polygons to SpatialPolygons
# 데이터 읽기
data(scotland)

# 데이터 내 속성 추출
# 폴리곤 데이터
polygon <- scotland$polygon$polygon
nrepeats <- scotland$polygon$nrepeats
# 폴리곤 이름
names <- scotland$data$county.names

# 2023.12.18 UTM 좌표계 정의
# spatial.polygon <- SpatialEpi::polygon2spatial_polygon(polygon, coordinate.system = "+proj=utm", names, nrepeats)
spatial.polygon <- SpatialEpi::polygon2spatial_polygon(polygon, coordinate.system = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs", names, nrepeats)

# 좌우 그림 설정
par(mfrow = c(1, 2))

# 좌측 폴리곤 시각화
plot(polygon, type = "n", xlab = "Eastings (km)", ylab = "Northings (km)", main = "Polygon File")
polygon(polygon)

# 우측 폴리곤 및 argyll-bute 시각화
plot(spatial.polygon, axes = TRUE)
title(xlab = "Eastings (km)", ylab = "Northings (km)", main = "Spatial Polygon")
plot(spatial.polygon[23], add = TRUE, col = "red")


# 2.1.2 Converting maps objects to SpatialPolygons
# 폴리콘 내  pennsylvania 및 vermont 도시 추출
county.map <- map("county", c("pennsylvania", "vermont"), fill = TRUE, plot = FALSE)
# 폴리곤 내 도시 이름
county.names <- as.character(county.map$names)
# 폴리곤 내 좌표계 위경도 정의
county <- maptools::map2SpatialPolygons(county.map, IDs = county.names, proj4string = CRS("+proj=longlat"))


# 맵 데이터에서 특정 주 선택
state.map <- map("state", c(), fill = TRUE, plot = FALSE)
state.names <- as.character(state.map$names)
state <- maptools::map2SpatialPolygons(state.map, IDs = state.names, proj4string = CRS("+proj=longlat"))

# 폴리곤 내 도시 시각화
plot(county, axes = TRUE, border = "red")

# 맵 데이터 주 시각화
plot(state, add = TRUE, lwd = 2)

# 2.2 Converting Between Coordinate Systems
# 폴리곤 도시의 UTM 좌표계 변환
county.grid <- latlong2grid(county)
# 맵 데이터의 UTM 좌표계 변환
state.grid <- latlong2grid(state)
# 폴리곤 데이터 시각화
plot(county.grid, axes = TRUE, border = "red")
# 맵 데이터 시각화
plot(state.grid, add = TRUE, lwd = 2)


# 특정 위경도 설정
coord <- rbind(c(-73.75, 45.4667), c(-122.6042, 45.6605))
# 해당 위경도에서 따른 UTM 좌표 정보
latlong2grid(coord)

# =========================================================================
# 2.3 Plotting a Variable
# =========================================================================
# 데이터 읽기
data(scotland)
scotland.map <- scotland$spatial.polygon
# 임의의 데이터 생성
y <- runif(nrow(scotland$data))
# 임의의 데이터 시각화
mapvariable(y, scotland.map)


# =========================================================================
# 3 Data Examples
# =========================================================================
# 3.1 Pennsylvania Lung Cancer & Smoking Data
# 데이터 읽기
data(pennLC)
# 폴리곤 데이터 선택
penn.map <- pennLC$spatial.polygon
# 위경도를 그리드 좌표 변환
penn.map <- latlong2grid(penn.map)
# 인구 집계
population <- tapply(pennLC$data$population, pennLC$data$county, sum)
# 사례 집계
cases <- tapply(pennLC$data$cases, pennLC$data$county, sum)
# 위경도를 그리드 좌표 변환
geo <- latlong2grid(pennLC$geo[, 2:3])
# 발생률 계산
incidence <- (cases/population) * 1000
# 발생률 시각화
mapvariable(incidence, penn.map)

# 3.2 Scotland Lip Cancer among Males in 1975-1980
# 데이터 읽기
data(scotland)
# 폴리곤 데이터 선택
scotland.map <- scotland$spatial.polygon
# 사례수
y <- scotland$data$cases
# 기대치
E <- scotland$data$expected
# 표준화 사망비 계산
SMR <- y/E
# SMR 시각화
mapvariable(SMR, scotland.map)

# =========================================================================
# 4 Methods
# =========================================================================
# 4.1 Expected Numbers of Disease and Standardized Mortality Ratios
# 데이터 읽기
data(pennLC)
# 계층 설정
n.strata <- 16
# 인구 집계
population <- tapply(pennLC$data$population, pennLC$data$county, sum)
# 사례 집계
cases <- tapply(pennLC$data$cases, pennLC$data$county, sum)
# 기대 사례 수 계산
expected.cases <- expected(pennLC$data$population, pennLC$data$cases, n.strata)

# 4.2 Disease Mapping
# 4.2.1 Empirical Bayes
# 데이터 읽기
data(scotland)
data <- scotland$data
x <- data$AFF
Xmat <- cbind(x, x^2)
# 경험적 베이즈 분석
results <- eBayes(data$cases, data$expected, Xmat)
# 폴리곤 데이터 선택
scotland.map <- scotland$spatial.polygon
# 상대 위험도(RR) 시각화
mapvariable(results$RR, scotland.map)

# 4.3 Cluster Detection
# 4.3.1 Kulldor
# 특정 데이터 선택
data <- pennLC$data
# 위경도를 그리드 좌표 변환
geo <- latlong2grid(pennLC$geo[, 2:3])
# 인구수 집계
population <- tapply(data$population, data$county, sum)
# 사례수 집계
cases <- tapply(data$cases, data$county, sum)
# 기대 사례수 계산
expected.cases <- expected(data$population, data$cases, 16)
# 상한 설정
pop.upper.bound <- 0.5
# 시뮬레이션 횟수
n.simulations <- 999
# 유의수준 설정
alpha.level <- 0.05
# 그래프 여부
plot <- TRUE

# 이항 분포 사용
binomial <- kulldorff(geo, cases, population, NULL, pop.upper.bound, n.simulations, alpha.level, plot)
# 높은 클러스터 선택
cluster <- binomial$most.likely.cluster$location.IDs.included
# 높은 클러스터 시각화
plot(pennLC$spatial.polygon, axes = TRUE)
plot(pennLC$spatial.polygon[cluster], add = TRUE, col = "red")
title("Most Likely Cluster")

# 포아송 분포 사용
poisson <- kulldorff(geo, cases, population, expected.cases, pop.upper.bound, n.simulations, alpha.level, plot)
# 높은 클러스터 선택
cluster <- poisson$most.likely.cluster$location.IDs.included
# 높은 클러스터 시각화
plot(pennLC$spatial.polygon, axes = TRUE)
plot(pennLC$spatial.polygon[cluster], add = TRUE, col = "red")
title("Most Likely Cluster Controlling for Strata")

# 4.3.2 Besag-Newell
# 클러스터 크기 설정
k <- 1250
# 유의수준 설정
alpha.level <- 0.05

# Besag-Newell 클러스터 탐지
results <- SpatialEpi::besag_newell(geo, population, cases, expected.cases = NULL, k, alpha.level)
results

# 기대 사례 수를 포함한 Besag-Newell 클러스터 탐지
results <- SpatialEpi::besag_newell(geo, population, cases, expected.cases, k, alpha.level)
results