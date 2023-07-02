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
# R을 이용한 기본 plot 라이브러리를 이용한 시각

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0402"

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
  # source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
  source(file.path(contextPath, "src", "InitConfig.R"))
}

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
# library(tidyverse)
# library(readr)
# library(moonBook)
# library(webr)
# library(ggstatsplot)
# library(useful)

# ******************************************************************************
# 문제1

# Figure 2: Various mean wine variables versus quality는 
# 주어진 데이터셋 “wineQuality-red.csv”에 대해서, 한 화면에 여러개의 그래프가 그려지고 있습니다. 
# 아래 그림에서 보여지는 시각화를 재현하기 위한 코드를 작성하시오. 
# (Write code to reproduce the visualisation shown in figure 2.)
# ******************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "winequality-red.csv"))
data = read.csv(fileInfo, sep = ";")

# names(data)
# summary(data)

# colInfo = "pH"
colList = setdiff(names(data), "quality")
# colorList = rainbow(length(colList))
colorList = 1:length(colList)

saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "Mean of Wine Variable versus Quality")
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

par(mfrow = c(1, 1))
for (i in 1:length(colList)) {
  colInfo = colList[i]

  selData = data.frame(
    quality = data[, "quality"]
    , value = data[, colInfo]
  )

  # asFor = as.formula(paste("value", "value", sep = " ~ "))
  selDataL1 = aggregate(value ~ quality, data = selData, FUN = mean)
  selDataL1$value = log(selDataL1$value, base = 10)

  # ylim = c(0, 2.0), 
  if (i == 1) {
    plot(selDataL1$quality, selDataL1$value, col = colorList[i], ylim = c(-1.5, 2), xlab = "Quality", ylab = "Wine Variable(log)", main = "Mean of Wine Variable \n versus Quality", type = "b", pch = i + 1)
  } else {
    lines(selDataL1$quality, selDataL1$value, col = colorList[i], type = "b", pch = i + 1)
  }
}

legend("topright", legend = colList, col = 1:length(colList), lty = 1, pch = 1:length(colList) + 1, cex = 0.8, title = "Variables", text.font = 0)
dev.off()

# ******************************************************************************
# 문제2

# R에 기본 내장되어 있는 mtcars 데이터셋에 대해서 
# Figure 4.에서 보여진 그래프를 재현하는 코드를 작성하시오.

# 왼쪽 플롯은 마력(horsepower)과 변수 엔진 변위(disp) 및 기화기(carb) 수 사이의 관계를 보여줍니다.
# 오른쪽 플롯은 마력(horsepower)과 변수 엔진 변위(disp) 및 실린더(cyl) 수 사이의 관계를 보여줍니다. 
# 사용된 데이터 세트는 mtcars입니다.

# Main: HP=f(Disp, Crabs), HP=f(Disp, Cyls)
# Y축: Horsepower (HP)
# X축: Displacement (Disp) HP as a fuction of Disp & Carbs / Displacement (Disp), HP as a fuction of Disp & Crabs
# ******************************************************************************
# carb 레이블 및 컬러
labelList = sort(unique(mtcars$carb))
# colorList = rainbow(length(labelList))
colorList = 1:length(labelList)

# cyl 레이블 및 컬러
label2List = sort(unique(mtcars$cyl))
# color2List = rainbow(length(label2List))
color2List = 1:length(label2List)

# factor형으로 변환
mtcars$carb = as.factor(mtcars$carb)
mtcars$cyl = as.factor(mtcars$cyl)

saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "Displacement (Disp) HP as a fuction of Disp & Carbs and Disp & Cyls")
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

par(mfrow = c(1, 2))
plot(hp ~ disp, data = mtcars, col = "white", xlab = "Displacement (Disp) \n HP as a fuction of Disp & Carbs", ylab = "Horsepower (HP)", main = "HP = f(Disp, Crabs)")
text(hp ~ disp, labels = carb, col = colorList[mtcars$carb], data = mtcars, cex = 1.0)
legend("topleft", legend = labelList, title.col = "black", text.col = colorList, cex = 0.8, title = "Carbs", text.font = 0)

plot(hp ~ disp, data = mtcars, col = "white", xlab = "Displacement (Disp) \n HP as a fuction of Disp & Cyls", ylab = "Horsepower (HP)", main = "HP = f(Disp, Cyls)")
text(hp ~ disp, labels = cyl, col = color2List[mtcars$cyl], data = mtcars, cex = 1.0)
legend("topleft", legend = label2List, title.col = "black", text.col = color2List, cex = 0.8, title = "Cyls", text.font = 0)

dev.off()

# ******************************************************************************
# 문제3

# Q3. Q3. R에 내장된 iris$Sepal.Length에서 quantile() 함수와 
# 자체 작성한 myQuantile()와 산출된 데이터셋을 참고하여, 산점도와 boxplot을 그리는 코드를 구하시오.
# (Create a crude version of the boxplot function, an example output is shown in Figure 3.)

# 그림 3: 원시적인 boxplot 함수의 출력은 상단 이미지에 표시되고 boxplot()의 결과는 하단에 표시됩니다.
# 사용된 데이터 세트는 iris$Sepal.Length입니다.
# ******************************************************************************
# 함수 정의
# myQuantile = function (x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
#   # sort the input vector
#   sorted_data = sort(x)
#   
#   n = length(sorted_data)
#   
#   # calculate the values of q1, q2, q3, q4, and q5
#   q1 = min(sorted_data, na.rm = TRUE)
#   q2 = sorted_data[ceiling(n / 4)]
#   q3 = median(sorted_data, na.rm = TRUE)
#   q4 = sorted_data[ceiling(n * 3 / 4)]
#   q5 = max(sorted_data, na.rm = TRUE)
#   
#   # combine the row names and the quantiles into a named vector
#   res = c(q1, q2, q3, q4, q5)
#   names(res) = paste0(probs * 100, "%")
#   
#   # return the result
#   return (res)
# }

myQuantile = function(x, probs = seq(0, 1, 0.25), na.rm = FALSE) {

  if (na.rm) { x = x[!is.na(x)] }

  n = length(x)
  index = 1 + (n - 1) * probs

  lo = floor(index)
  hi = ceiling(index)

  x = sort(x, partial = unique(c(lo, hi)))
  qs = x[lo]

  i = 1:length(probs)
  h = index - lo
  qs = (1 - h) * qs + h * x[hi]

  names(qs) = paste0(probs * 100, "%")

  return(qs)
}

# 전처리
quaList = myQuantile(iris$Sepal.Length)
# colorList = rainbow(length(quaList) - 1)
colorList = 1:(length(quaList) - 1)

# for (i in 1:length(quaList)) {
#   if ((i + 1) > length(quaList)) { next }
#   iris[quaList[[i]] <= iris$Sepal.Length & iris$Sepal.Length <= quaList[[i + 1]], "color"] = colorList[i]
# }

iris[quaList[[1]] <= iris$Sepal.Length & iris$Sepal.Length < quaList[[2]], "color"] = colorList[1]
iris[quaList[[2]] <= iris$Sepal.Length & iris$Sepal.Length < quaList[[3]], "color"] = colorList[2]
iris[quaList[[3]] <= iris$Sepal.Length & iris$Sepal.Length < quaList[[4]], "color"] = colorList[3]
iris[quaList[[4]] <= iris$Sepal.Length & iris$Sepal.Length <= quaList[[5]], "color"] = colorList[4]

saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "scatter and boxplot")
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

par(mfrow = c(2, 1))
plot(iris$Sepal.Length, rep(1, times = length(iris$Sepal.Length)), ylim=c(1.4, 0.6), pch = 9, col = iris$color, xlab = NA, ylab = NA, xaxt = "n")
axis(3)

boxplot(iris$Sepal.Length, horizontal = TRUE, xaxt = "n")
axis(3)

dev.off()

# ******************************************************************************
# 문제4

# Q4. Iris dataset에서 Species별로 Sepal.Length, Sepal.Width, Petal.Length, Petal.Width에 대해 
# 아래와 같이 평균을 구하였습니다. 

# 상기 output을 사용하여 위의 output 데이터를 포함하는 트리 구조를 만드는 코드를 작성하십시오.
# 트리 구조는 다음과 같이 설명됩니다.
# - 뿌리(root)에서 세 개의 가지가 있으며 각각은 특정 종(Species)을 나타냅니다.
# - 각 종(Species) 가지(branch)는 다음 두 가지로 나뉩니다: 꽃받침(Sepal)과 꽃잎(Petal)
# - Sepal 가지는 길이와 너비로 구성된 두 개의 가지로 나뉩니다.
# - 마찬가지로 꽃잎 가지는 길이와 너비로 구성된 두 개의 가지로 나뉩니다.
# - 트리의 루트는 노드(nod)로만 구성되고 다른 쪽 끝은 12개의 분기(branches)로 구성됩니다.
# 가장 적절한 기본 제공 R 데이터 구조를 사용하여 이 트리 구조를 구축합니다.
# 트리 구조를 시각화할 필요가 없으며 코드를 작성하여 트리 구조를 생성하기만 하면 됩니다.

# ******************************************************************************
statData = aggregate(Sepal.Length ~ Species, data = iris, FUN = mean, na.rm = TRUE)
stat2Data = aggregate(Sepal.Width ~ Species, data = iris, FUN = mean, na.rm = TRUE)
stat3Data = aggregate(Petal.Length ~ Species, data = iris, FUN = mean, na.rm = TRUE)
stat4Data = aggregate(Petal.Width ~ Species, data = iris, FUN = mean, na.rm = TRUE)

treeData = list(
  setosa = list(
    Sepal = list(
      Length = statData$Sepal.Length[1]
      , Width = stat2Data$Sepal.Width[1]
    )
    , Petal = list(
      Length = stat3Data$Petal.Length[1]
        , Width = stat4Data$Petal.Width[1]
    )
  )
  , versicolor = list(
    Sepal = list(
      Length = statData$Sepal.Length[2]
        , Width = stat2Data$Sepal.Width[2]
    )
    , Petal = list(
      Length = stat3Data$Petal.Length[2]
        , Width =  stat4Data$Petal.Width[2]
    )
  )
  , virginica = list(
    Sepal = list(
      Length = statData$Sepal.Length[3]
      , Width = stat2Data$Sepal.Width[3]
    )
    , Petal = list(
      Length = stat3Data$Petal.Length[3]
      , Width =  stat4Data$Petal.Width[3]
    )
    
  )
)

print(treeData)

# ******************************************************************************
# 문제5

# quantile() 함수는 다소 정교하지만 기본은 매우 간단합니다. 
# 여기서 목표는 quantile() 함수의 기본을 재현하는 것입니다. 
# 다음과 같이 자신만의 quantile() 함수 버전을 작성하세요
# ******************************************************************************
myQuantile = function(x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  
  if (na.rm) { x = x[!is.na(x)] }
  
  n = length(x)
  index = 1 + (n - 1) * probs
  
  lo = floor(index)
  hi = ceiling(index)
  
  x = sort(x, partial = unique(c(lo, hi)))
  qs = x[lo]
  
  i = 1:length(probs)
  h = index - lo
  qs = (1 - h) * qs + h * x[hi]
  
  names(qs) = paste0(probs * 100, "%")
  
  return(qs)
}


# myQuantile 생성 함수
print(myQuantile(iris$Sepal.Length))

# quantile 기본 함수 
print(quantile(iris$Sepal.Length))

