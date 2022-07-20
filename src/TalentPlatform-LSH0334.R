
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
# R을 이용한 docx 워드 파일에서 정보 추출 및 특성별 CEO 발화 분류

# Text Sample 각 워드 문서가 "PRESENTATION" 부분과 "QUESTIONS AND ANSWERS" 부분으로 구성되어 있는데, 
# 각 부분으로 문서를 나눈 후 "ceo"라고 이름이 나와있는 부분의 텍스트만 따로 모아서 
# 새로운 문서를 구성하려고 합니다. 
# 하나의 문서를 PRESENTATION 부분에서 ceo의 텍스트 모음, 
# Q&A 부분에서 ceo의 텍스트 모음 이렇게 두 문서로 나누려고 합니다.

# 우선 감사합니다. 그런데 각 기업-연도별로 구분된 문서가 필요한데 이 부분이 가능할까요?
# 즉, A 기업 2011년 presentation section의 CEO 발화, A 기업 2011년 Q&A section의 CEO 발화 /
# A 기업 2012년 presentation section의 CEO 발화, A 기업 2012년 Q&A section의 CEO 발화 / ...
# B 기업 2011년 presentation section의 CEO 발화, B 기업 2011년 Q&A section의 CEO 발화 /
# B 기업 2012년 presentation section의 CEO 발화, B 기업 2012년 Q&A section의 CEO 발화
# 이런 형태의 파일이 필요합니다..

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0334"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

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

# rm(list=ls())

# install.packages("stringr")
# install.packages("dplyr")
# install.packages("readtext")

library(stringr)
library(readtext)
library(dplyr)
library(officer)

# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "Earnings Call Transcript*.docx"))
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "전체데이터/Earnings Call Transcript*.docx"))

# ******************************************************************************
# 자료 전처리
# ******************************************************************************
preContData = tibble::tibble()
qnaContData = tibble::tibble()

# fileInfo = fileList[1]
for (fileInfo in fileList) {
  
  splitData = readtext::readtext(fileInfo) %>% 
    tidyr::separate_rows(text, sep = "QUESTIONS AND ANSWERS") %>% 
    tidyr::spread(-doc_id, key="text") %>% 
    magrittr::set_colnames(c("fileInfo", "preCont", "qnaCont"))
  
  if (ncol(splitData) != 3) { 
    cat(sprintf("[CHECK] File Exception : %s (ncol = %s) ", splitData$fileInfo, ncol(splitData)), "\n")
    next 
  }
  
  # qua 정보
  tryCatch(
    expr = {
      
      tmpQnaData = splitData %>%
        dplyr::select(fileInfo, qnaCont) %>%
        tidyr::separate_rows(qnaCont, sep = "─────────────────────────────────────────────────────────────────────────────────────") %>%
        dplyr::mutate(
          isFlag = stringr::str_detect(qnaCont, regex("CEO"))
        )
      
      qnaContData = dplyr::bind_rows(qnaContData, tmpQnaData)
    }
    
    , warning = function(warning) {
      cat(sprintf("[WARN] File Not Found (qna) : %s", splitData$fileInfo), "\n")
    }
    
    , error = function(error) {
      cat(sprintf("[ERROR] File Not Found (qna) : %s", splitData$fileInfo), "\n")
    }
  )
  
  
  # presentation 정보
  tryCatch(
    expr = {
      
      tmpPreData = splitData %>% 
        dplyr::select(fileInfo, preCont) %>% 
        tidyr::separate_rows(preCont, sep = "─────────────────────────────────────────────────────────────────────────────────────") %>% 
        dplyr::mutate(
          isFlag = stringr::str_detect(preCont, regex("CEO"))
        )
      
      preContData = dplyr::bind_rows(preContData, tmpPreData)
    }
    
    , warning = function(warning) {
      cat(sprintf("[WARN] File Not Found (presentation) : %s", splitData$fileInfo), "\n")
    }
    
    , error = function(error) {
      cat(sprintf("[ERROR] File Not Found (presentation) : %s", splitData$fileInfo), "\n")
    }
  )
  

}

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "preContData_prep.csv")
readr::write_csv(preContData, file = saveFile)

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "qnaContData_prep.csv")
readr::write_csv(qnaContData, file = saveFile)

preContDataL1 = preContData %>% 
  dplyr::filter(isFlag == TRUE) %>% 
  dplyr::mutate(
    qnaCont = stringr::str_replace_all(preCont, "\\.", ". ") %>% 
      stringr::str_replace_all("CEO", "CEO ")
  )


qnaContDataL1 = qnaContData %>% 
  dplyr::filter(isFlag == TRUE) %>% 
  dplyr::mutate(
    qnaCont = stringr::str_replace_all(qnaCont, "\\.", ". ") %>% 
      stringr::str_replace_all("CEO", "CEO ")
  )


# ******************************************************************************
# MS 워드 저장
# ******************************************************************************
# presentation section의 CEO 발화
fileList = preContDataL1$fileInfo %>% unique() %>% sort()

fileName = fileList[1]
for (fileName in fileList) {
  
  splitInfo = stringr::str_split(fileName, pattern = "_|\\.| ") %>% unlist()
  
  preContDataL2 = preContDataL1 %>% 
    dplyr::filter(fileInfo == fileName)
  
  if (nrow(preContDataL2) < 1) { next }
  
  saveFile = sprintf("%s/%s_%s 기업 %s년 %s", globalVar$outPath, serviceName, splitInfo[4], splitInfo[5], "presentation section의 CEO 발화.docx")
  
  # officer::body_add_par(value = paste(preContDataL2$preCont, "(LineBreak)(LineBreak)", collapse = "")) %>% 
  doc = officer::read_docx() %>%
    officer::body_add_par(value = paste(preContDataL2$preCont, "
                                        
                                        
                                        ", collapse = "")) %>% 
    print(target = saveFile)
}


# Q&A section의 CEO 발화
fileList = qnaContDataL1$fileInfo %>% unique() %>% sort()

for (fileName in fileList) {
  
  splitInfo = stringr::str_split(fileName, pattern = "_|\\.| ") %>% unlist()
  
  qnaContDataL2 = qnaContDataL1 %>% 
    dplyr::filter(fileInfo == fileName)
  
  if (nrow(qnaContDataL2) < 1) { next }
  
  saveFile = sprintf("%s/%s_%s 기업 %s년 %s", globalVar$outPath, serviceName, splitInfo[4], splitInfo[5], "Q&A section의 CEO 발화.docx")
  
  # officer::body_add_par(value = paste(qnaContDataL2$qnaCont, "(LineBreak)(LineBreak)", collapse = "")) %>%
  doc = officer::read_docx() %>%
    officer::body_add_par(value = paste(qnaContDataL2$qnaCont, "                                            
                                                                      
                                        
                                        "
                                        , collapse = "")) %>%
    print(target = saveFile)
  
}
