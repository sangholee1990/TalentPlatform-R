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
# R을 이용한 불꽃놀이 형상화 코드 개선

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0522"

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
# 벡터화된 Unif_on_Circle 함수
Unif_on_Circle = function(a, b, r, n) {
  x = runif(n, a - r, a + r)
  y_radius = sqrt(r^2 - (x - a)^2)
  y = runif(n, -y_radius + b, y_radius + b)
  return(cbind(x, y))
}

Fireworks = function(num_fireworks = 10, num_points = 30, sleep_time = 0.1, color_alpha = 0.3) {
  # 입력 매개변수 유효성 검사
  if (num_fireworks <= 0 || num_points <= 0 || sleep_time < 0) {
    stop("Invalid parameters: num_fireworks, num_points, and sleep_time must be positive.")
  }
  
  # 폭죽 중심 및 반지름 벡터화 생성
  centers = cbind(runif(num_fireworks, -1, 1), runif(num_fireworks, -1, 1))
  radii = runif(num_fireworks, 0.1, 0.3)
  
  # 폭죽 점들 리스트 벡터화 생성
  firework_points = lapply(1:num_fireworks, function(i) {
    Unif_on_Circle(centers[i, 1], centers[i, 2], radii[i], num_points)
  })
  
  
  # 시각화 설정
  par(pty = "s", bg = "black")
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "")
  colors = sample(rainbow(num_points, alpha = color_alpha))
  
  # 폭죽 그리기 (시각화 과정에서는 for문 사용)
  for (i in 1:num_fireworks) {
    for (j in 1:num_points) {
      lines(x = c(centers[i, 1], firework_points[[i]][j, 1]),
            y = c(centers[i, 2], firework_points[[i]][j, 2]), 
            col = colors[j], lwd = 1)
    }
    if (i > 2) {
      points(firework_points[[i - 2]], col = colors, cex = 0.5)
    }
    Sys.sleep(sleep_time)
  }
  
  # 나머지 폭죽 처리
  for (i in c(num_fireworks - 1, num_fireworks)) {
    points(firework_points[[i]], col = colors, cex = 0.5)
    Sys.sleep(sleep_time)
  }
  
  # 마지막 폭죽 표시
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "")
  for (i in 1:num_fireworks) {
    points(firework_points[[i]], col = colors, cex = 0.5)
  }
}

Fireworks(10, 30)

num_fireworks = 10
num_points = 30
sleep_time = 0.1
color_alpha = 0.3

