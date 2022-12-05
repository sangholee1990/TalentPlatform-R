
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
# R을 이용한 강원도 미세먼지 일평균 캘린더 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0379"

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
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
}

#================================================
# 비즈니스 로직 수행
#================================================
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library(openair)
library(lattice)

#================================================
# 함수 정의
#================================================
makeOpenKeyLegend <- function(key, default.key, fun.name = "function") {
  # handle logicals and lists
  if (is.logical(key)) {
    legend <- if (key) default.key else NULL
  } else if (is.list(key)) {
    legend <- listUpdate(default.key, key)
  } else {
    if (!is.null(key)) {
      warning(
        paste(
          "In ", fun.name, "(...):\n unrecognised key not exported/applied\n",
          " [see ?drawOpenKey for key structure/options]",
          sep = ""
        ),
        call. = FALSE
      )
    }
    legend <- NULL
  }

  # structure like legend for drawOpenKey
  if (!is.null(legend)) {
    legend <- list(right = list(
      fun = drawOpenKey, args = list(key = legend),
      draw = FALSE
    ))
    if ("space" %in% names(legend$right$args$key)) {
      names(legend)[[1]] <- legend$right$args$key$space
    }
  }
  legend
}

listUpdate <- function(a, b, drop.dots = TRUE,
                       subset.a = NULL, subset.b = NULL) {
  if (drop.dots) {
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if (!is.null(subset.a)) {
    a <- a[names(a) %in% subset.a]
  }
  if (!is.null(subset.b)) {
    b <- b[names(b) %in% subset.b]
  }
  if (length(names(b) > 0)) {
    a <- modifyList(a, b)
  }
  a
}

## gives names of lattice strips
strip.fun <- function(results.grid, type, auto.text) {
  ## proper names of labelling ###################################################
  pol.name <- sapply(
    levels(factor(results.grid[[type[1]]])),
    function(x) quickText(x, auto.text)
  )
  strip <- strip.custom(factor.levels = pol.name)

  if (length(type) == 1) {
    strip.left <- FALSE
  } else { ## two conditioning variables

    pol.name <- sapply(
      levels(factor(results.grid[[type[2]]])),
      function(x) quickText(x, auto.text)
    )
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
  list(strip, strip.left, pol.name)
}


## from lattice
chooseFace <- function(fontface = NULL, font = 1) {
  if (is.null(fontface)) {
    font
  } else {
    fontface
  }
}


checkPrep <- function(mydata, Names, type, remove.calm = TRUE, remove.neg = TRUE,
                      strip.white = TRUE, wd = "wd") {

  ## deal with conditioning variable if present, if user-defined, must exist in data
  ## pre-defined types
  ## existing conditioning variables that only depend on date (which is checked)
  conds <- c(
    "default", "year", "hour", "month", "season", "weekday", "week",
    "weekend", "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
    "yearseason", "seasonyear"
  )
  all.vars <- unique(c(names(mydata), conds))

  varNames <- c(Names, type) ## names we want to be there
  matching <- varNames %in% all.vars

  if (any(!matching)) {
    ## not all variables are present
    stop(cat("Can't find the variable(s)", varNames[!matching], "\n"))
  }

  ## add type to names if not in pre-defined list
  if (any(type %in% conds == FALSE)) {
    ids <- which(type %in% conds == FALSE)
    Names <- c(Names, type[ids])
  }

  ## if type already present in data frame
  if (any(type %in% names(mydata))) {
    ids <- which(type %in% names(mydata))
    Names <- unique(c(Names, type[ids]))
  }

  ## just select data needed
  mydata <- mydata[, Names]

  ## if site is in the data set, check none are missing
  ## seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) { ## split by site

    ## remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id,]
    }
  }


  ## sometimes ratios are considered which can results in infinite values
  ## make sure all infinite values are set to NA
  mydata[] <- lapply(mydata, function(x) {
    replace(x, x == Inf | x == -Inf, NA)
  })

  if ("ws" %in% Names) {
    if ("ws" %in% Names & is.numeric(mydata$ws)) {

      ## check for negative wind speeds
      if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
        if (remove.neg) { ## remove negative ws only if TRUE
          warning("Wind speed <0; removing negative data")
          mydata$ws[mydata$ws < 0] <- NA
        }
      }
    }
  }

  ## round wd to make processing obvious
  ## data already rounded to nearest 10 degress will not be affected
  ## data not rounded will be rounded to nearest 10 degrees
  ## assumes 10 is average of 5-15 etc
  if (wd %in% Names) {
    if (wd %in% Names & is.numeric(mydata[, wd])) {

      ## check for wd <0 or > 360
      if (any(sign(mydata[[wd]][!is.na(mydata[[wd]])]) == -1 |
                mydata[[wd]][!is.na(mydata[[wd]])] > 360)) {
        warning("Wind direction < 0 or > 360; removing these data")
        mydata[[wd]][mydata[[wd]] < 0] <- NA
        mydata[[wd]][mydata[[wd]] > 360] <- NA
      }

      if (remove.calm) {
        if ("ws" %in% names(mydata)) {
          mydata[[wd]][mydata$ws == 0] <- NA ## set wd to NA where there are calms
          mydata$ws[mydata$ws == 0] <- NA ## remove calm ws
        }
        mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360

        ## round wd for use in functions - except windRose/pollutionRose
        mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
        mydata[[wd]][mydata[[wd]] == 0] <- 360 # angles <5 should be in 360 bin
      }
      mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
    }
  }


  ## make sure date is ordered in time if present
  if ("date" %in% Names) {
    if ("POSIXlt" %in% class(mydata$date)) {
      stop("date should be in POSIXct format not POSIXlt")
    }

    ## try and work with a factor date - but probably a problem in original data
    if (is.factor(mydata$date)) {
      warning("date field is a factor, check date format")
      mydata$date <- as.POSIXct(mydata$date, "GMT")
    }

    mydata <- arrange(mydata, date)

    ## make sure date is the first field
    if (names(mydata)[1] != "date") {
      mydata <- mydata[c("date", setdiff(names(mydata), "date"))]
    }

    ## check to see if there are any missing dates, stop if there are
    ids <- which(is.na(mydata$date))
    if (length(ids) > 0) {
      mydata <- mydata[-ids,]
      warning(paste(
        "Missing dates detected, removing",
        length(ids), "lines"
      ), call. = FALSE)
    }

    ## daylight saving time can cause terrible problems - best avoided!!

    if (any(dst(mydata$date))) {
      message("Detected data with Daylight Saving Time.")
    }
  }

  if (strip.white) {
    ## set panel strip to white
    suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))
  }

  ## return data frame
  return(mydata)
}


makeCalendarPlot = function(mydata, pollutant = "nox", year = 2003, month = 1:12,
                            type = "default", annotate = "date", statistic = "mean",
                            cols = "heat", limits = c(0, 100), lim = NULL, col.lim = c("grey30",
                                                                                       "black"), col.arrow = "black", font.lim = c(1, 2), cex.lim = c(0.6,
                                                                                                                                                      1), digits = 0, data.thresh = 0, labels = NA, breaks = NA,
                            w.shift = 0, remove.empty = TRUE, main = NULL, key.header = "",
                            key.footer = "", key.position = "right", key = TRUE, auto.text = TRUE,
                            plot = TRUE, ...)
{
  conc.mat <- NULL
  if (w.shift < 0 || w.shift > 6) {
    warning("w.shift should be between 0 and 6")
  }
  weekday.abb <- substr(format(ISOdate(2000, 1, 2:8), "%A"),
                        1, 1)[((6:12) + w.shift) %% 7 + 1]
  extra.args <- list(...)
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(fontsize = current.font))
  extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
    quickText(extra.args$xlab, auto.text)
  }
  else {
    quickText("", auto.text)
  }
  extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
    quickText(extra.args$ylab, auto.text)
  }
  else {
    quickText("", auto.text)
  }
  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }
  if (annotate %in% c("date", "value"))
    vars <- c("date", pollutant)
  if (annotate == "wd")
    vars <- c("wd", "ws", "date", pollutant)
  if (annotate == "ws")
    vars <- c("wd", "ws", "date", pollutant)
  if (!missing(year)) {
    mydata <- selectByDate(mydata, year = year)
  }
  if (!missing(month)) {
    mydata <- selectByDate(mydata, month = month)
  }
  if (nrow(mydata) == 0)
    stop("No data to plot - check year chosen")
  mydata <- checkPrep(mydata, vars, "default", remove.calm = FALSE)
  main <- quickText(main, auto.text)
  def.theme <- list(strip.background = list(col = "#ffe5cc"),
                    strip.border = list(col = "black"), axis.line = list(col = "black"),
                    par.strip.text = list(cex = 1))
  cal.theme <- list(strip.background = list(col = "grey90"),
                    strip.border = list(col = "transparent"), axis.line = list(col = "transparent"),
                    par.strip.text = list(cex = 0.8))
  lattice.options(default.theme = cal.theme)
  all.dates <- seq(as_date(floor_date(min(mydata$date), "month")),
                   as_date(ceiling_date(max(mydata$date), "month")) - 1,
                   by = "day")

  prepare.grid <- function(mydata, pollutant) {
    firstDay <- format(mydata$date[1], "%A")
    lastDay <- as.numeric(format(mydata$date[length(mydata$date)],
                                 "%d"))
    pad.start <- (as.numeric(format(mydata$date[1], "%w")) -
      w.shift) %% 7 + 1
    conc <- rev(mydata[[pollutant]])
    actual_date <- rev(mydata$date)
    theDates <- as.numeric(format(mydata$date, "%d"))
    theDates <- rev(theDates)
    daysAtEnd <- 42 - pad.start - nrow(mydata)
    conc <- c(rep(NA, daysAtEnd), conc)
    actual_date <- c(rep(NA, daysAtEnd), actual_date)
    endDates <- mydata$date[nrow(mydata)] + (1:daysAtEnd)
    endDates <- rev(as.numeric(format(endDates, "%d")))
    theDates <- c(endDates, theDates)
    beginDates <- -1 * (1:pad.start) + mydata$date[1]
    beginDates <- as.numeric(format(beginDates, "%d"))
    conc <- c(conc, rep(NA, pad.start))
    actual_date <- c(actual_date, rep(NA, pad.start))
    if (pad.start != 0)
      theDates <- c(theDates, beginDates)
    dateColour <- c(rep("grey70", daysAtEnd), rep("black",
                                                  nrow(mydata)), rep("grey70", pad.start))
    conc.mat <- matrix(conc, ncol = 7, byrow = TRUE)
    date.mat <- matrix(theDates, ncol = 7, byrow = TRUE)
    actual_date.mat <- matrix(actual_date, ncol = 7, byrow = TRUE)
    colour.mat <- matrix(dateColour, ncol = 7, byrow = TRUE)
    conc.mat <- as.vector(apply(conc.mat, 1, rev))
    date.mat <- as.vector(apply(date.mat, 1, rev))
    actual_date.mat <- as.vector(apply(actual_date.mat, 1,
                                       rev))
    colour.mat <- as.vector(apply(colour.mat, 1, rev))
    grid <- data.frame(expand.grid(x = 1:7, y = 1:6))
    results <- tibble(x = grid$x, y = grid$y, conc.mat, date.mat = date.mat,
                      dateColour = colour.mat, date = lubridate::as_date(actual_date.mat))
    results
  }

  mydata <- timeAverage(mydata, "day", statistic = statistic,
                        data.thresh = data.thresh)
  mydata$date <- as_date(mydata$date)
  type <- "cuts"
  mydata <- left_join(data.frame(date = all.dates), mydata,
                      by = "date")
  # mydata <- mutate(mydata, cuts = format(date, "%B-%Y"), cuts = ordered(cuts, levels = unique(cuts)))
  mydata <- mutate(mydata, cuts = format(date, "%Y월 %B"), cuts = ordered(cuts, levels = unique(cuts)))
  if (remove.empty) {
    mydata <- group_by(mydata, cuts) %>%
      mutate(empty = all(is.na(across(pollutant)))) %>%
      filter(empty == FALSE) %>%
      ungroup()
  }
  baseData <- mydata
  original_data <- mydata
  if (!missing(month)) {
    mydata <- selectByDate(mydata, month = month)
  }
  mydata <- mydata %>%
    group_by(across(type)) %>%
    do(prepare.grid(.,
                    pollutant)) %>%
    ungroup()
  mydata$value <- mydata$conc.mat
  strip.dat <- strip.fun(mydata, type, auto.text)
  strip <- strip.dat[[1]]
  category <- FALSE
  if (!anyNA(labels) && !anyNA(breaks)) {
    category <- TRUE
    mydata <- mutate(mydata, conc.mat = cut(conc.mat, breaks = breaks,
                                            labels = labels))
  }
  if (annotate == "wd") {
    baseData$wd <- baseData$wd * 2 * pi / 360
    wd <- baseData %>%
      group_by(across(type)) %>%
      do(prepare.grid(.,
                      "wd")) %>%
      ungroup()
    wd$value <- wd$conc.mat
  }
  if (annotate == "ws") {
    baseData$wd <- baseData$wd * 2 * pi / 360
    ws <- baseData %>%
      group_by(across(type)) %>%
      do(prepare.grid(.,
                      "ws")) %>%
      ungroup()
    wd <- baseData %>%
      group_by(across(type)) %>%
      do(prepare.grid(.,
                      "wd")) %>%
      ungroup()
    ws$conc.mat <- ws$conc.mat / max(ws$conc.mat, na.rm = TRUE)
    ws$value <- ws$conc.mat
    wd$value <- wd$conc.mat
  }
  if (category) {
    if (length(labels) + 1 != length(breaks))
      stop("Need one more break than labels")
    n <- length(levels(mydata$conc.mat))
    col <- openColours(cols, n)
    legend <- list(col = col, space = key.position, auto.text = auto.text,
                   labels = levels(mydata$conc.mat), footer = key.footer,
                   header = key.header, height = 0.8, width = 1.5, fit = "scale",
                   plot.style = "other")
    col.scale <- breaks
    legend <- makeOpenKeyLegend(key, legend, "windRose")
  }
  else {
    nlev <- 200
    if (missing(limits)) {
      breaks <- pretty(mydata$value, n = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
    }
    else {
      breaks <- pretty(limits, n = nlev)
      labs <- pretty(breaks, 7)
      labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
      if (max(limits) < max(mydata$value, na.rm = TRUE)) {
        id <- which(mydata$value > max(limits))
        mydata$value[id] <- max(limits)
        labs <- pretty(breaks, 7)
        labs <- labs[labs >= min(breaks) & labs <= max(breaks)]
        labs[length(labs)] <- paste(">", labs[length(labs)])
      }
    }
    nlev2 <- length(breaks)
    col <- openColours(cols, (nlev2 - 1))
    col.scale <- breaks
    legend <- list(col = col, at = col.scale, labels = list(labels = labs),
                   space = key.position, auto.text = auto.text, footer = key.footer,
                   header = key.header, height = 1, width = 1.5, fit = "all")
    legend <- makeOpenKeyLegend(key, legend, "calendarPlot")
  }
  lv.args <- list(x = value ~ x * y | cuts, data = mydata,
                  par.settings = cal.theme, main = main, strip = strip,
                  par.strip.text = list(cex = 0.9), at = col.scale, col.regions = col,
                  as.table = TRUE, scales = list(y = list(draw = FALSE),
                                                 x = list(at = 1:7, labels = weekday.abb, tck = 0),
                                                 par.strip.text = list(cex = 0.8), alternating = 1,
                                                 relation = "free"), aspect = 6 / 7, between = list(x = 1),
                  colorkey = FALSE, legend = legend, panel = function(x,
                                                                      y, subscripts, ...) {
      panel.levelplot(x, y, subscripts, ...)
      panel.abline(v = c(0.5:7.5), col = "grey90")
      panel.abline(h = c(0.5:7.5), col = "grey90")
      if (annotate == "date") {
        ltext(x[subscripts], y[subscripts], labels = mydata$date.mat[subscripts],
              cex = 0.6, col = as.character(mydata$dateColour[subscripts]))
      }
      if (annotate == "value") {
        date.col <- as.character(mydata$dateColour[subscripts])
        ids <- which(date.col == "black")
        date.col[ids] <- "transparent"
        ltext(x[subscripts], y[subscripts], labels = mydata$date.mat[subscripts],
              cex = 0.6, col = date.col)
        concs <- mydata$value[subscripts]
        ids <- seq_along(concs)
        the.cols <- rep(col.lim[1], length(ids))
        the.font <- rep(font.lim[1], length(ids))
        the.cex <- rep(cex.lim[1], length(ids))
        if (!is.null(lim)) {
          ids <- which(concs >= lim)
          the.cols[ids] <- col.lim[2]
          the.font[ids] <- font.lim[2]
          the.cex[ids] <- cex.lim[2]
        }
        the.labs <- round(concs, digits = digits)
        id <- which(is.na(the.labs))
        if (length(id) > 0) {
          the.labs <- as.character(the.labs)
          the.labs[id] <- ""
        }
        ltext(x[subscripts], y[subscripts], labels = the.labs,
              cex = the.cex, font = the.font, col = the.cols)
      }
      if (annotate == "wd") {
        larrows(x + 0.5 * sin(wd$value[subscripts]),
                y + 0.5 * cos(wd$value[subscripts]), x + -0.5 *
            sin(wd$value[subscripts]), y + -0.5 * cos(wd$value[subscripts]),
                angle = 20, length = 0.07, lwd = 0.5, col = col.arrow)
      }
      if (annotate == "ws") {
        larrows(x + (0.5 *
          sin(wd$value[subscripts]) *
          ws$value[subscripts]), y + (0.5 *
          cos(wd$value[subscripts]) *
          ws$value[subscripts]), x + (-0.5 *
          sin(wd$value[subscripts]) *
          ws$value[subscripts]), y + (-0.5 *
          cos(wd$value[subscripts]) *
          ws$value[subscripts]), angle = 20, length = 0.07,
                lwd = 0.5, col = col.arrow)
      }
    })
  lv.args <- listUpdate(lv.args, extra.args)
  if (plot) {
    print(do.call(levelplot, lv.args))
  }
  lattice.options(default.theme = def.theme)
  plt <- trellis.last.object()
  newdata <- left_join(mydata, original_data %>% select(any_of(c("date",
                                                                 "ws", "wd"))), by = "date")
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}


# *****************************************************
# 2022년 강원도 미세먼지 일평균 캘린더
# *****************************************************
Sys.setlocale("LC_ALL", "ko_KR.UTF-8")

# inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "강원권_미세먼지_달력.xlsx"))
inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "강원권_미세먼지_달력_221205.xlsx"))

data = openxlsx::read.xlsx(inpFile, sheet = 1) %>%
  as.tibble() %>%
  dplyr::mutate(
    date = readr::parse_datetime(sDate, "%Y-%m-%d")
  ) %>%
  dplyr::rename(
    "pm25" = "강원권.PM2.5"
  )

summary(data)

# data %>%
#   dplyr::filter(date == as.Date("2022-09-06"))

# plotSubTitle = sprintf("%s", "2022년 강원도 미세먼지 일평균 캘린더 시각화")
plotSubTitle = sprintf("%s", "강원권 PM2.5 농도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
# ata, cuts = format(date, "%B-%Y"),

makeCalendarPlot(
  data
  , pollutant = "pm25"
  , year = 2021:2022
  , month = 1:12
  , annotate = "value"
  , breaks = c(0, 16, 36, 76, 500)
  , labels = c("좋음 (0~15)", "보통 (16~35)", "나쁨 (36~75)", "매우 나쁨 (76~)")
  , statistic = "mean"
  , cols = c("#518EF8", "#1CEE37", "#FFE81A", "#F13B61")
  , key.position = "bottom"
  , main = plotSubTitle
  , names = 'aa'
  , w.shift = 1
  , cuts = format(date, "%Y-%B")
  ) # +
 # ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

dev.off()

