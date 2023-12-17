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
# R을 이용한 nloptr, lpSolve 패키지로부터 수치해석 알고리즘

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0534"

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
library(nloptr)

# Objective function and its gradient
eval_f <- function(x) {
  objective_value <- exp((x[1] + x[2])^8) * 1 / cos((x[1] + x[2])^4)
  gradient_value <- c(
    8 * exp((x[1] + x[2])^8) * (x[1] + x[2])^7 * 1 / cos((x[1] + x[2])^4) - 
      exp((x[1] + x[2])^8) * (x[1] + x[2])^4 * sin((x[1] + x[2])^4),
    8 * exp((x[1] + x[2])^8) * (x[1] + x[2])^7 * 1 / cos((x[1] + x[2])^4) -
      exp((x[1] + x[2])^8) * (x[1] + x[2])^4 * sin((x[1] + x[2])^4)
  )
  list("objective" = objective_value, "gradient" = gradient_value)
}

# Inequality constraints and their Jacobian
eval_g_ineq <- function(x) {
  constraints_value <- c(x[1]^2 + x[2]^2 - 1, exp(-x[1]) + abs(x[2]) - 1)
  jacobian_value <- rbind(
    c(2 * x[1], 2 * x[2]),
    c(-exp(-x[1]), ifelse(x[2] >= 0, 1, -1))
  )
  list("constraints" = constraints_value, "jacobian" = jacobian_value)
}

# Bounds for the variables
lb <- c(0.5, -1)
ub <- c(1, 1)

# Initial values
x_init <- c(0.5, -1)

# Optimization settings
opts <- list(
  "algorithm" = "NLOPT_LD_AUGLAG",
  "xtol_rel" = 1.0e-30,
  "maxeval" = 1000,
  "local_opts" = list(
    "algorithm" = "NLOPT_LD_LBFGS"
    , "xtol_rel" = 1.0e-30
    , "lower_bounds"=lb
    , "upper_bounds"=ub
    )
)

# Run the optimization
nlp <- nloptr(
  x0 = x_init,
  eval_f = eval_f,
  eval_g_ineq = eval_g_ineq,
  # lb = lb,
  # ub = ub,
  opts = opts
)

# Results
print(paste("Optimal objective value:", nlp$objective))
print(paste("Solution:", paste(nlp$solution, collapse = ", ")))




# 라이브러리 읽기
library(lpSolve)
library(gmp)

# Define the coefficients of the objective function
objective_coeffs <- sapply(1:200, function(i) sin(i)^2 + cos(i))

# Number of decision variables
num_vars <- 200

# Initialize the constraint matrix, direction, and right-hand side vector
const_mat <- matrix(0, nrow = num_vars, ncol = num_vars)
const_dir <- rep("<=", num_vars)
const_rhs <- rep(0, num_vars)

# Adding constraints for xi + xi+1 + xi+2 <= 2.758
for (i in 1:(num_vars - 2)) {
  const_mat[i, i:(i+2)] <- 1
  const_rhs[i] <- 2.758
}


# Apply prime number constraints
prime_indices = which(sapply(1:num_vars, function(i) gmp::isprime(n = i)) == 2)
for (i in prime_indices) {
  const_mat[i, i] <- 1
  const_rhs[i] <- 1.56
}

# Solve the LP problem
lp_solution <- lp("max", objective_coeffs, const_mat, const_dir, const_rhs)

# Output the solution and objective value
if (lp_solution$status == 0) {
  print(paste("Objective value:", lp_solution$objval))
  print("Solution:")
  print(lp_solution$solution)
} 





