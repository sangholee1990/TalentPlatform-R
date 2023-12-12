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
serviceName = "LSH0523"

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
    
    # 선분 그리기
    segments(
      x0 = rep(centers[i, 1], num_points)
      , y0 = rep(centers[i, 2], num_points)
      , x1 = firework_points[[i]][, 1]
      , y1 = firework_points[[i]][, 2]
      , col = colors
      , lwd = 1
    )
    
    
    if (i > 2) {
      points(firework_points[[i - 2]], col = colors, cex = 0.5)
    }
    
    # 나머지 폭죽 처리
    if (i %in% c(num_fireworks - 1, num_fireworks)) {
      points(firework_points[[i]], col = colors, cex = 0.5)
    }
    
    points(firework_points[[i]], col = colors, cex = 0.5)
    
    Sys.sleep(sleep_time)
  }
}

Fireworks(num_fireworks = 10, num_points = 30, sleep_time = 0.3, color_alpha = 0.3)

# ==============================================================================
# 2023.12.10 개선 코드
# ==============================================================================
# # 벡터화된 Unif_on_Circle 함수
# Unif_on_Circle = function(a, b, r, n) {
#   x = runif(n, a - r, a + r)
#   y_radius = sqrt(r^2 - (x - a)^2)
#   y = runif(n, -y_radius + b, y_radius + b)
#   return(cbind(x, y))
# }
# 
# Fireworks = function(num_fireworks = 10, num_points = 30, sleep_time = 0.1, color_alpha = 0.3) {
#   # 입력 매개변수 유효성 검사
#   if (num_fireworks <= 0 || num_points <= 0 || sleep_time < 0) {
#     stop("Invalid parameters: num_fireworks, num_points, and sleep_time must be positive.")
#   }
#   
#   # 폭죽 중심 및 반지름 벡터화 생성
#   centers = cbind(runif(num_fireworks, -1, 1), runif(num_fireworks, -1, 1))
#   radii = runif(num_fireworks, 0.1, 0.3)
#   
#   # 폭죽 점들 리스트 벡터화 생성
#   firework_points = lapply(1:num_fireworks, function(i) {
#     Unif_on_Circle(centers[i, 1], centers[i, 2], radii[i], num_points)
#   })
#   
#   
#   # 시각화 설정
#   par(pty = "s", bg = "black")
#   plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "")
#   colors = sample(rainbow(num_points, alpha = color_alpha))
#   
#   # 폭죽 그리기 (시각화 과정에서는 for문 사용)
#   for (i in 1:num_fireworks) {
#     for (j in 1:num_points) {
#       lines(x = c(centers[i, 1], firework_points[[i]][j, 1]),
#             y = c(centers[i, 2], firework_points[[i]][j, 2]), 
#             col = colors[j], lwd = 1)
#     }
#     if (i > 2) {
#       points(firework_points[[i - 2]], col = colors, cex = 0.5)
#     }
#     Sys.sleep(sleep_time)
#   }
#   
#   # 나머지 폭죽 처리
#   for (i in c(num_fireworks - 1, num_fireworks)) {
#     points(firework_points[[i]], col = colors, cex = 0.5)
#     Sys.sleep(sleep_time)
#   }
#   
#   # 마지막 폭죽 표시
#   plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "")
#   for (i in 1:num_fireworks) {
#     points(firework_points[[i]], col = colors, cex = 0.5)
#   }
# }
# 
# Fireworks(10, 30)


# ==============================================================================
# 원본 코드
# ==============================================================================
# # 중심이 (a, b)이고 반지금이 r 인 원 안에서 균등한 확률변수 생성
# Unif_on_Circle = function(a, b, r, n)
# {
#   # x는 균등분포
#   x = runif(n, a - r, a + r)
#   # y는 x가 주어졌을 때 조건부분포가 균등분포
#   y = NULL #빈 깡통
#   for(i in 1 : n)
#     y[i] = runif(1, - sqrt(r^2 - (x[i] - a)^2) + b, 
#                  sqrt(r^2 - (x[i] - a)^2) + b) # 원의방정식
#   return(cbind(x, y)) # xy 도출 원으ㅏ중심이랑 원 안의 점 랜덤으로 뽑음 n개의 점이 찍힘..cex 
# }
# 
# Fireworks = function(num_fire = 10, num_points= 30, sleep = 0.1)
# {
#   # num_fire : 폭죽의 개수
#   # num_points : 하나의 폭죽에서 그려지는 점의 개수
#   # sleep : 움직임을 위한 Sys.sleep 옵션
#   
#   # [-1, 1] x [-1, 1] 공간에서 num_fire 개의 원 중심 만들기
#   center_a = runif(num_fire, -1, 1) #중심의 x좌표
#   center_b = runif(num_fire, -1, 1)
#   center_ab = cbind(center_a, center_b) # 중심의 xy좌표
#   # num_fire 개의 반지름 [0.1, 0.3] 사이에서 생성
#   r_vec = runif(num_fire, 0.1, 0.3) # 반지름을 0.1-0.3까지 랜덤으로 설정
#   # points list
#   fire_points_list = list()
#   for(i in 1:num_fire)
#     fire_points_list[[i]] = Unif_on_Circle(center_ab[i, 1], center_ab[i, 2], # x y 반지름 다들 i번째 ㅇㅇ n 은 폭죽터지는갯수 ㅇㅇ 몇개로 터질지 존나 많은것들
#                                            r_vec[i], n = num_points) # 
#   # pty = "s" 는 그림을 정사각형으로 만들어줌
#   # bg 는 그림의 배경색
#   
#   # unif~~  원의중심이랑 반지름이 정해진 후 범위 안의 있는 점을 n개를 찍어준다..
#   par(pty = "s", bg = "black")
#   plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", bty = "n",
#        axes = F, xlab = "", ylab = "")
#   rain_col = sample(rainbow(num_points, alpha = 0.3))
#   for (i in 1 : num_fire)
#   {
#     for (j in 1 : num_points)
#       lines(x = c(center_ab[i, 1], fire_points_list[[i]][j, 1]),
#             y = c(center_ab[i, 2], fire_points_list[[i]][j, 2]), 
#             col = rain_col[j], lwd = 1)
#     # 연쇄적인 시각화를 위한 작업
#     if (i > 2)
#       points(fire_points_list[[i - 2]], 
#              col = rain_col, cex = 0.5)
#     Sys.sleep(sleep)
#   }
#   # 나머지 2개
#   for(i in c(num_fire - 1, num_fire))
#   {
#     points(fire_points_list[[i]], 
#            col = rain_col, cex = 0.5)
#     Sys.sleep(sleep)
#   }
#   # 마지막 점
#   plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", bty = "n", 
#        axes = F, xlab = "", ylab = "")
#   for(i in 1 : num_fire)
#   {
#     points(fire_points_list[[i]], 
#            col = rain_col, cex = 0.5)
#   }
#   # 각종 정보들 저장 후 output으로 저장
#   results = list()
#   results$center_ab = center_ab
#   results$r_vec = r_vec
#   results$fire_points_list = fire_points_list
#   results$rain_col = rain_col
#   return(results)
# }
# 
# Fireworks(10, 30,)