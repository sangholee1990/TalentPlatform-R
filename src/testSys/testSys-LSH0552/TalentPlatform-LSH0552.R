#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#================================================
# 요구사항
#================================================
# R을 이용한 생물통계학 데이터 분석 및 문서 작성
 
# ================================================
# 초기 환경변수 설정
# ================================================
wd = getwd()
print(wd)

setwd(wd)

# ================================================
# Q1. Cells data analysis
# ================================================
# Q1-1. 다운로드 데이터 중 "cells.csv"를 불러와 cell이라는 변수에 할당하고 데이터의 구성을 파악하시오.
cell = read.csv("./LSH0552/Biostatics/cells.csv")
str(cell)

# Q1-2. cells(1)가 1인 사람들만 추출하여 cell_one이라는 변수에 할당하시오.
cell_one = subset(cell, cells == 1)
head(cell_one)

# Q1-3. cells가 1인 사람 중 sex가 male인 사람이 몇 명인지 파악하시오. (결과포함)
sum(cell_one$sex == "male", na.rm = TRUE)

# Q1-4. cells가 1인 사람 중 smoker가 True이고 weight가 over인 사람이 몇 명인지 파악하시오. (결과포함)
sum(cell_one$smoker == TRUE & cell_one$weight == "over", na.rm = TRUE)

# Q1-5. weight가 obese이고 smoker가 True인 사람들을 추출하여 Ob_Sm라는 변수에 할당한 후 이 사람들의 cells가 0~7인 사람이 각각 몇 명씩 있는지 파악하시오. (결과포함)
Ob_Sm = subset(cells, weight == "obese" & smoker == TRUE)
table(Ob_Sm$cells)

# ================================================
# Q2. flowering data analysis
# ================================================
# Q2-1. 다운로드 데이터 중 "flowering.csv"를 불러와 flower 변수에 저장한 후, flower의 성질을 파악하시오.
flower = read.csv("./LSH0552/Biostatics/flowering.csv")
str(flower)

# Q2-2. flower의 variety에 어떤 값이 있는지 파악하시오. (결과포함)
unique(flower$variety)

# Q2-3. flower의 variety가 A인 것만 추출한 후, flowered 값의 평균과 최댓값, 최솟값을 구하시오. (결과포함)
selFlower = subset(flower, variety == "A")
mean(selFlower$flowered, na.rm = TRUE)
max(selFlower$flowered, na.rm = TRUE)
min(selFlower$flowered, na.rm = TRUE)

# Q2-4. tapply 함수를 사용해 flower의 variety별 flowered열 값의 평균을 구하시오. (결과포함)
tapply(flower$flowered, flower$variety, mean)

# ================================================
# Q3. insect's sex_ratio data analysis
# ================================================
# Q3-1. 다운로드 데이터 중 "sexratio.csv"를 불러와 insect 변수에 할당 후, insect의 열의 구성을 파악하시오.
insect = read.csv("./LSH0552/Biostatics/sexratio.csv")
str(insect)

# Q3-2. insect 변수에서 density열을 x축으로 두고, 각 males열과 females 열을 y축으로 하는 2개의 산점도 플롯(scatter plot)을 그리시오. (아래 그림과 같은 결과가 나오는 코드 작성)
# (두 그래프의 y축의 범위 : 0 ~ 400 / x축 : total_density / y축 : males_density, females_density)

par(mfrow=c(1, 2))

# 여성에 대한 density 그래프
plot(insect$density, insect$females, xlab = "total_density", ylab = "females_density", ylim=c(0, 400))

# 남성에 대한 density 그래프
plot(insect$density, insect$males, xlab = "total_density", ylab = "males_density", ylim=c(0, 400))


# ================================================
# Q4. daphnia data analysis
# ================================================
# Q4-1. 다운로드 데이터 중 "daphnia.csv"를 불러와 dap라는 변수에 할당하고 데이터의 구성을 파악하시오.
dap = read.csv("./LSH0552/Biostatics/daphnia.csv")
str(dap)

# Q4-2. dap의 Growth.rate(1)열과 Daphnia(4)열만 추출하여 dap_sub라는 변수에 할당하시오.
dap_sub = dap[,c("Growth.rate", "Daphnia")]
head(dap_sub)

# Q4-3. dap_sub을 Growth.rate(1)열 기준으로 오름차순 정렬하여 dap_sub 변수에 재할당하시오.
dap_sub = dap_sub[order(dap_sub$Growth.rate),]
head(dap_sub)

# Q4-4. dap_sub에서 Growth.rate의 평균, 분산, 중간값, 최소값, 최대값을 구하시오. (결과포함)
mean(dap_sub$Growth.rate, na.rm = TRUE)
var(dap_sub$Growth.rate, na.rm = TRUE)
median(dap_sub$Growth.rate, na.rm = TRUE)
min(dap_sub$Growth.rate, na.rm = TRUE)
max(dap_sub$Growth.rate, na.rm = TRUE)

# Q4-5. dap_sub의 Daphnia열이 어떤 값들로 이루어졌는지 파악하시오. (결과포함)
unique(sort(dap_sub$Daphnia))

# Q4-6. Daphnia가 Clone1, Clone2, Clone3로 이루어진 것을 파악했다면, Clone1인 행들을 추출하여 dap_C1 변수에 할당, Clone2인 행들을 추출하여 dap_C2에 할당, Clone3인 행들을 추출하여 dap_C3에 할당하시오.
dap_C1 = subset(dap_sub, Daphnia == "Clone1")
head(dap_C1)

dap_C2 = subset(dap_sub, Daphnia == "Clone2")
head(dap_C2)

dap_C3 = subset(dap_sub, Daphnia == "Clone3")
head(dap_C3)

# Q4-7. Daphnia 별로 Growth.rate 값의 boxplot을 그리시오. (아래 그림과 같은 결과가 나오는 코드 작성) (제목 : "Growth.rate of Daphnia" / x축 : Clone1, Cone2, Clone3 / y축 : Growth.rate)
par(mfrow=c(1, 1))
boxplot(Growth.rate ~ Daphnia, data=dap_sub, main="Growth.rate of Daphnia", xlab=NULL, ylab="Growth.rate")
