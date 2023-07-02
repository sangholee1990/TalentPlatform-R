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
# R을 이용한 코로나 감염병 전파 시뮬레이션 및 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0417"

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

# ================================================
# 비즈니스 로직 수행
# ================================================
# 라이브러리 읽기
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(webr)
library(mgcv)

# dat = Sim1$prevalence
# timev = seq(1, length(dat), 1)
# aggregate = 2
# npts = 200
# plt = TRUR
# figtitle = ""

# function for running GAM and creating plots
DoubleTimeRate <- function(dat, timev = seq(1, length(dat), 1), aggregate = 2, npts = 200, plt = FALSE, figtitle = "") {
  kval <- floor(length(dat) / 20)
  res <- data.frame(sdt = rep(0, npts), sdtup = rep(0, npts), sdtlow = rep(0, npts))
  Tv <- timev[1:(length(timev))]
  datfull <- dat
  dat <- dat[1:(length(dat))]

  # MGAM <- gam(dat ~ s(Tv, bs = 'gp', k = kval))
  MGAM <- mgcv::gam(dat ~ s(Tv, bs = 'gp'))

  xv <- seq(min(Tv), max(Tv), length = npts)
  newd <- data.frame(Tv = xv) # data.frame(Tv=xv, DW=DW)
  X0 <- predict(MGAM, newd, type = "lpmatrix")
  #  p <- predict(MGAM,newd,  type = "link", se.fit = TRUE)
  # meanspline <- (exp(p$fit))

  eps <- 1e-7 ## finite difference interval
  xv <- seq(min(Tv), max(Tv), length = npts) + eps
  newd <- data.frame(Tv = xv) # data.frame(Tv=xv, DW=DW)
  X1 <- predict(MGAM, newd, type = "lpmatrix")
  Xp <- (X1 - X0) / eps ## maps coefficients to (fd approx.) derivatives

  Xi <- Xp * 0

  # Xi%*%coef(MGAM) = smooth deriv i
  # Xi[, 1:(kval - 1) + 1] <- Xp[, 1:(kval - 1) + 1]
  Xi[, 1:(ncol(Xp))] <- Xp[, 1:ncol(Xp)]

  # ith smooth derivative
  df <- Xi %*% coef(MGAM)

  # cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
  df.sd <- rowSums(Xi %*% MGAM$Vp * Xi)^.5

  res$sdt <- df
  res$sdtup <- df + 2 * df.sd
  res$sdtlow <- df - 2 * df.sd

  #  MGLM <- glm(dat~(Tv), family=quasipoisson)
  #  Doubling<-c(MGLM$coefficients[2],confint(MGLM)[2,1],confint(MGLM)[2,2])

  if (plt == TRUE) {
    par(mfrow = c(2, 1))

    plot(as.Date(c(timev), origin = "1899-12-30"), c(datfull), xlim = as.Date(c(min(timev), max(timev) + 2), origin = "1899-12-30"), pch = 16, col = 2, main = '', ylab = 'Number', xlab = 'Time', xaxt = 'n')
    axis.Date(1, at = as.Date(c('2020-03-01', '2020-04-01', '2020-05-01', '2020-06-01', '2020-07-01', '2020-08-01', '2020-09-01', '2020-10-01', '2020-11-01', '2020-12-01', '2021-01-01', '2021-02-01', '2021-03-01', '2021-04-01', '2021-05-01', '2021-06-01', '2021-07-01', '2021-08-01', '2021-09-01', '2021-10-01', '2021-11-01', '2021-12-01', '2022-01-01', '2022-02-01', '2022-03-01', '2022-04-01', '2022-05-01', '2022-06-01', '2022-07-01', '2022-08-01', '2022-09-01', '2022-10-01', '2022-11-01', '2022-12-01')), format = '%m-%y')

    # timev #seq(min(Tv),max(Tv), length=npts)+eps
    xv1 <- c(timev, max(timev) + 1:2)

    # dow <- weekdays(as.Date(xv1, origin = "1899-12-30"))
    # data.frame(Tv=xv, DW=DW)
    newd <- data.frame(Tv = xv1)
    p <- predict(MGAM, newd, type = "link", se.fit = TRUE)
    upr <- p$fit + (2 * p$se.fit)
    lwr <- p$fit - (2 * p$se.fit)
    upr <- MGAM$family$linkinv(upr)
    lwr <- MGAM$family$linkinv(lwr)
    #  abline(h=6, col='grey')
    abline(h = c(3, 6, 9), col = 'grey')
    lines(as.Date(xv1, origin = "1899-12-30"), (p$fit))
    lines(as.Date(xv1, origin = "1899-12-30"), upr, col = 1, lty = 2)
    lines(as.Date(xv1, origin = "1899-12-30"), lwr, col = 1, lty = 2)
    polygon(x = c(as.Date(xv1, origin = "1899-12-30"), rev(as.Date(xv1, origin = "1899-12-30"))),
            y = c(upr,
                  rev(lwr)),
            col = adjustcolor("blue", alpha.f = 0.10), border = NA)
    #   text(as.Date(min(xv1)+40, origin = "1899-12-30"), 3/4*max(datfull), pos=4,cex=0.75, paste('Projected new deaths \n in next 2 weeks:\n',round(sum(exp(p$fit)[length(p$fit)-13:0])),' (',
    #                                                                qnbinom(c(0.025*14), mu=sum(lwr[length(p$fit)-13:0]), size=MGAM$family$getTheta(TRUE)),',',
    #                                                               qnbinom(c(1-0.025*14), mu=sum(upr[length(p$fit)-13:0]), size=MGAM$family$getTheta(TRUE)),')', sep=''))
    #    res <- c(round(sum(exp(p$fit)[length(p$fit)-13:0])),
    #            qnbinom(c(0.025*14), mu=sum(lwr[length(p$fit)-13:0]), size=MGAM$family$getTheta(TRUE)),
    #           qnbinom(c(1-0.025*14), mu=sum(upr[length(p$fit)-13:0]), size=MGAM$family$getTheta(TRUE)))
    #    res <- cbind((rnbinom(1000, mu=sum(exp(p$fit)[length(p$fit)-(aggregate-1):0]), size=MGAM$family$getTheta(TRUE))),
    #                (rnbinom(1000, mu=sum(lwr[length(p$fit)-(aggregate-1):0]), size=MGAM$family$getTheta(TRUE))),
    #               (rnbinom(1000, mu=sum(upr[length(p$fit)-(aggregate-1):0]), size=MGAM$family$getTheta(TRUE))))
    #print((is.finite(res[,3])))
    # resprint <- c(quantile(res[,1], probs = 0.5),quantile(res[,2], probs = 0.025),
    #             ifelse(sum(is.finite(res[,3]))==0,NA,quantile(res[,3], probs = 0.975)))
    ##    text(as.Date(min(xv1)+70, origin = "1899-12-30"), 3/4*max(datfull), pos=4,cex=0.75, paste('Projected new deaths \n in next 2 weeks:\n',round(resprint[1],0),' (',round(resprint[2],0),',',round(resprint[3],0),')', sep=''))
    #                                                                qnbinom(c(0.025*14), mu=sum(lwr[length(p$fit)-13:0]), size=MGAM$family$getTheta(TRUE)),',',
    #                                                               qnbinom(c(1-0.025*14), mu=sum(upr[length(p$fit)-13:0]), size=MGAM$family$getTheta(TRUE)),')', sep=''))
    res <- list(splinederiv = res, splinedaymean = p$fit)

    plot(as.Date(xv, origin = "1899-12-30"), df, type = "l", ylim = range(c(df + 2 * df.sd, df - 2 * df.sd)), ylab = 'Instantaneous growth rate', xlab = 'Time', main = '', xaxt = 'n')
    axis.Date(1, at = as.Date(c('2020-03-01', '2020-04-01', '2020-05-01', '2020-06-01', '2020-07-01', '2020-08-01', '2020-09-01', '2020-10-01', '2020-11-01', '2020-12-01', '2021-01-01', '2021-02-01', '2021-03-01', '2021-04-01', '2021-05-01', '2021-06-01', '2021-07-01', '2021-08-01', '2021-09-01', '2021-10-01', '2021-11-01', '2021-12-01', '2022-01-01', '2022-02-01', '2022-03-01', '2022-04-01', '2022-05-01', '2022-06-01', '2022-07-01', '2022-08-01', '2022-09-01', '2022-10-01', '2022-11-01', '2022-12-01')), format = '%m-%y')
    polygon(x = c(c(as.Date("2020-03-23"), as.Date("2020-06-01")), rev(c(as.Date("2020-03-23"), as.Date("2020-06-01")))), y = c(0.5, 0.5, -0.5, -0.5), col = adjustcolor(rgb(0.8, 0.8, 0.8), alpha.f = 0.50), border = NA)
    polygon(x = c(c(as.Date("2020-10-31"), as.Date("2020-12-02")), rev(c(as.Date("2020-10-31"), as.Date("2020-12-02")))), y = c(0.5, 0.5, -0.5, -0.5), col = adjustcolor(rgb(0.8, 0.8, 0.8), alpha.f = 0.50), border = NA)
    polygon(x = c(c(as.Date("2021-01-04"), as.Date("2021-03-08")), rev(c(as.Date("2021-01-04"), as.Date("2021-03-08")))), y = c(0.5, 0.5, -0.5, -0.5), col = adjustcolor(rgb(0.8, 0.8, 0.8), alpha.f = 0.50), border = NA)
    polygon(x = c(c(as.Date("2020-12-20"), as.Date("2021-02-15")), rev(c(as.Date("2020-12-20"), as.Date("2021-02-15")))), y = c(0.5, 0.5, -0.5, -0.5), col = adjustcolor(rgb(1, 0.9, 0.9), alpha.f = 0.50), border = NA)
    polygon(x = c(c(as.Date("2021-03-16"), as.Date("2021-05-10")), rev(c(as.Date("2021-03-16"), as.Date("2021-05-10")))), y = c(0.5, 0.5, -0.5, -0.5), col = adjustcolor(rgb(1, 0.8, 0.8), alpha.f = 0.50), border = NA)
    lines(as.Date(xv, origin = "1899-12-30"), df + 2 * df.sd, lty = 2);
    lines(as.Date(xv, origin = "1899-12-30"), df - 2 * df.sd, lty = 2)
    polygon(x = c(as.Date(xv, origin = "1899-12-30"), rev(as.Date(xv, origin = "1899-12-30"))),
            y = c(df + 2 * df.sd,
                  rev(df - 2 * df.sd)),
            col = adjustcolor("blue", alpha.f = 0.10), border = NA)
    abline(h = 0, col = 4)
    axis(4, at = c(-0.1, -0.05, -0.033, -0.0247, 0, 0.0247, 0.033, 0.0495, 0.099, 0.173, 0.3466), labels = c(c(-7, -14, -21, -28) / 7, 'Infinite', c(28, 21, 14, 7, 4, 2) / 7))
    mtext(figtitle, outer = TRUE, cex = 1.5, line = -4)
    #    mtext(paste('Projected new deaths in next ', aggregate, ' days: ',round(resprint[1],0),' (',round(resprint[2],0),',',round(resprint[3],0),')', sep=''), side=3, line=2)
    if (df[npts] - 2 * df.sd[npts] > 0) {
      mtext(paste('Increasing recent trend', sep = ''), side = 3, line = 3)
    }else if (df[npts] + 2 * df.sd[npts] < 0) {
      mtext(paste('Decreasing recent trend', sep = ''), side = 3, line = 3)
    }else {
      if (df[npts] > 0) {
        mtext(paste('Plateauing recent trend, may be increasing', sep = ''), side = 3, line = 3)
      }else {
        mtext(paste('Plateauing recent trend, may be decreasing', sep = ''), side = 3, line = 3)
      }
    }
  }

  res
}

# omega <- 0.1
# rho <- 1.4
SIRdet <- function(rho = 1.4, omega = 0) {
  Xv <- array(0, c(50000, 3))
  I0 <- 0.0005
  dt <- 0.001

  Xv[1,] <- c(1 - I0 - omega, I0, omega)
  Imax <- 1 -
    omega -
    (1 + log(rho) + log(1 - I0 - omega)) / rho

  for (i in 1:(dim(Xv)[1] - 1)) {
    Xv[i + 1, 1] <- Xv[i, 1] - dt * rho * Xv[i, 1] * Xv[i, 2]
    Xv[i + 1, 2] <- Xv[i, 2] + dt * rho * Xv[i, 1] * Xv[i, 2] - dt * Xv[i, 2]
    Xv[i + 1, 3] <- Xv[i, 3] + dt * Xv[i, 2]
  }
  par(mfrow = c(1, 2))
  plot(dt * (0:(dim(Xv)[1] - 1)), Xv[, 2], type = 'l', xlab = 'Time', ylab = 'Frac. I')
  lines(dt * (0:(dim(Xv)[1] - 1)), rho * Xv[, 2] * Xv[, 1], col = 2)
  abline(h = Imax, col = 1, lty = 2)
  plot(dt * (0:(dim(Xv)[1] - 1)), Xv[, 1], col = 4, type = 'l', xlab = 'Time', ylim = c(0, 1), ylab = 'Frac. S & R')
  lines(dt * (0:(dim(Xv)[1] - 1)), Xv[, 3], col = 3)
  abline(h = 1 / rho, col = 4, lty = 2)

  c(Imax, max(rho * Xv[, 2] * Xv[, 1]))
  #abline(h=1/rho)
}

SEIRdet <- function(rho = 1.4, alpha = 1 / 2, sig = 2, omega = 0, simtime = 50) {
  dt <- 1000
  report <- dt * sig
  Xv <- array(0, c(simtime * dt, 4))
  I0 <- 0.0005

  #*exp((1+alpha)/2+sqrt((1-alpha)^2+(1-I0-omega)*alpha*rho)/2)
  E0 <- I0 / alpha
  dt <- 1 / dt

  Xv[1,] <- c(1 - I0 - omega, E0, I0, omega)
  Imax <- 1 -
    omega -
    (1 + log(rho) + log(1 - I0 - omega)) / rho

  for (i in 1:(dim(Xv)[1] - 1)) {
    Xv[i + 1, 1] <- Xv[i, 1] - dt * rho * Xv[i, 1] * Xv[i, 3]
    Xv[i + 1, 2] <- Xv[i, 2] + dt * rho * Xv[i, 1] * Xv[i, 3] - dt * alpha * Xv[i, 2]
    Xv[i + 1, 3] <- Xv[i, 3] + dt * alpha * Xv[i, 2] - dt * Xv[i, 3]
    Xv[i + 1, 4] <- Xv[i, 4] + dt * Xv[i, 3]
  }
  par(mfrow = c(1, 2))
  plot(dt * (0:(dim(Xv)[1] - 1)), Xv[, 2], type = 'l', xlab = 'Time', ylab = 'Frac. I')
  lines(dt * (0:(dim(Xv)[1] - 1)), Xv[, 3], col = 2)
  Cases <- Xv[, 4] + Xv[, 3]
  lines(dt * (report:(dim(Xv)[1] - 1)), diff(Cases, lag = report), col = 3)
  abline(h = Imax / (1 + alpha), col = 1, lty = 1)
  abline(h = Imax - (1 + alpha) / 2 / rho * (1 - sqrt(1 + 2 * rho * Imax * ((1 - alpha)) / (1 + alpha^2))), col = 3, lty = 1)
  #print(c(Imax,(1+alpha)/2/rho*(1-sqrt(1+2*rho*Imax*((1-alpha))/(1+alpha^2)))))
  abline(h = max(Xv[, 2]), col = 1, lty = 2)
  abline(h = max(Xv[, 3]), col = 2, lty = 2)
  abline(v = c(dt * which.max(Xv[, 2]), dt * which.min(abs(Xv[, 1] - 1 / rho)), dt * which.max(Xv[, 3])))
  plot(dt * (0:(dim(Xv)[1] - 1)), Xv[, 1], col = 4, type = 'l', xlab = 'Time', ylim = c(0, 1), ylab = 'Frac. S & R')
  lines(dt * (0:(dim(Xv)[1] - 1)), Xv[, 4], col = 3)
  lines(dt * (0:(dim(Xv)[1] - 1)), Xv[, 4] + Xv[, 3], col = 3, lty = 2)
  abline(h = 1 / rho, col = 4, lty = 2)
  print(c(dt * report + dt * which.max(diff(Cases, lag = report)), dt * which.max(Xv[, 2]), dt * which.min(abs(Xv[, 1] - 1 / rho)), dt * which.max(Xv[, 3])))
  print(dt * which.max(Xv[, 2]) + c(0, alpha / (1 + alpha), 1))

  res <- data.frame(time = dt * (0:(dim(Xv)[1] - 1)), instant_incidence = alpha * Xv[, 2], prevalence = Xv[, 3], accumulated_incidence = Xv[, 3] + Xv[, 4])
  res
}

Sim1 = SEIRdet()

DoubleTimeRate(Sim1$prevalence, plt = TRUE, figtitle = "function for running GAM and creating plots")


# # 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "K.TAEAN호설치전후평가_230405.xlsx"))
# data = openxlsx::read.xlsx(fileInfo, sheet = 1)
#
# dataL1 = data %>%
#   as.tibble() %>%
#   dplyr::filter(
#     ! is.na(before)
#     , ! is.na(after)
#   ) %>%
#   dplyr::mutate(
#     id = dplyr::row_number()
#   ) %>%
#   gather(-id, key = "key", value = "val")
#
# # *****************************************
# # 시각화
# # *****************************************
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 비교추이")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::ggscatter(
#   data = dataL1, x = "id", y = "val", color = "key"
#   , add = "reg.line", alpha = 0.3, palette = c("#00AFBB", "#E7B800")
# ) +
#   labs(title = NULL, x = "인덱스", y = "전력 사용량", color = NULL, subtitle = plotSubTitle) +
#   theme_bw() +
#   ggpubr::stat_regline_equation(aes(color = key), label.x.npc = 0.0, label.y.npc = 0.95, size = 6) +
#   ggpubr::stat_cor(aes(color = key), label.x.npc = 0.5, label.y.npc = 0.95, p.accuracy = 0.01, r.accuracy = 0.01, size = 6) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 밀도함수")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::ggdensity(
#   data = dataL1, x = "val", add = "mean", rug = TRUE,
#   color = "key", fill = "key", palette = c("#00AFBB", "#E7B800")
#   ) +
#   labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 빈도분포")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::gghistogram(
#   data = dataL1, x = "val", add = "mean", rug = TRUE,
#   color = "key", fill = "key", palette = c("#00AFBB", "#E7B800")
# ) +
#   labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 상자그림")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::ggboxplot(
#   dataL1, x = "key", y = "val", color = "key", palette =c("#00AFBB", "#E7B800"),
#   add = "jitter", shape = "key", alpah = 0.1
# ) +
#   ggpubr::stat_compare_means(comparisons = list( c("before", "after"))) +
#   labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
# # *****************************************
# # 통계 검정
# # *****************************************
# # F 검정
# fTest = var.test(val ~ key, data = dataL1, conf.level = 0.95)
# print(fTest)
#
# # F 검정에서 유의수준 p-value < 0.05 이하로서 귀무가설이 기각 (두 그룹은 분산 차이)
# mainTitle = sprintf("%s", "제품전후 간의 F 검정")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# plot(fTest) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
#
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
# # T 검정
# # 등분산 가정 O
# # tTest = t.test(val ~ key, data = dataL1, conf.level = 0.95, var.equal = TRUE, paired = FALSE)
#
# # 등분산 가정 X
# tTest = t.test(val ~ key, data = dataL1, conf.level = 0.95, var.equal = FALSE, paired = FALSE)
#
# # T 검정에서 유의수준 p-value는 0.01 이하로서 귀무가설 기각 (두 그룹은 평균 차이)
# print(tTest)
#
# mainTitle = sprintf("%s", "제품전후 간의 T 검정")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# plot(tTest) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
#
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")