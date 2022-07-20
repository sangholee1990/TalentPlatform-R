
# 파일 찾기 및 CSV 파일 읽기
stuBasInfoData = read.csv("student_basic_info.csv")

stuCouInfoData = read.csv("student_course_info.csv")


# Student_num, Name 컬럼을 기준으로 좌측 조인
data = merge(stuBasInfoData, stuCouInfoData, by = c("Student_num" = "Student_num", "Name" = "Name"))

head(data)
# Student_num   Name       Major Entrance_year Number_of_semester
# 1   143211001   Mike     Physics          2011                  7
# 2   143211002 Robert Mathematics          2012                  6
# 3   143211003 Sophia   Chemistry          2013                  5
# 4   143211004  James     Biology          2013                  4
# 5   143211005  Jacob Mathematics          2015                  3
# 6   143211006 Austin   Chemistry          2011                  6
# Total_course_credits        Course_taken Grade Course_semester
# 1                   30        Basic_course  3.52          Spring
# 2                   51        Major_course  4.21            Fall
# 3                   15 Liberal_arts_course  2.83            Fall
# 4                   27        Basic_course  3.75          Spring
# 5                   12 Liberal_arts_course  3.28            Fall
# 6                   33        Major_course  4.37          Spring

# ********************************************************************************************
# Q1. 전공(Major)이 Chemistry인 모든 학생들의 Student_num, Major, Name, Entrance_year, 
# Number_of_semester, Total_course_credits을 Console 창에 출력하는 코드 작성하시오.
# ********************************************************************************************
dataL1 = subset(data, Major %in% c("Chemistry"))
dataL2 = subset(dataL1, select = c(Student_num, Major, Name, Entrance_year, Number_of_semester, Total_course_credits))

# Student_num     Major    Name Entrance_year Number_of_semester
# 3    143211003 Chemistry  Sophia          2013                  5
# 6    143211006 Chemistry  Austin          2011                  6
# 10   143211010 Chemistry  Amelia          2015                  5
# 12   143211012 Chemistry   Henry          2014                  5
# 16   143211016 Chemistry  Eliana          2015                  6
# 19   143211019 Chemistry    Eric          2011                  8
# 21   143211021 Chemistry Michael          2015                  5
# 27   143211027 Chemistry    Anna          2014                  2
# Total_course_credits
# 3                    15
# 6                    33
# 10                   48
# 12                   36
# 16                   69
# 19                   78
# 21                   51
# 27                   48

# ********************************************************************************************
# Q2. 입학년도(Entrance_year)가 2013년을 포함하며 그리고 2013년 이후에 입학한 모든 학생들의 
# Student_num, Major, Name, Entrance_year. Number_of_semester, Total_course_credits 
# Console 창에 출력하는 코드 작성하시오..
# ********************************************************************************************
dataL1 = subset(data, Entrance_year >= 2013)
dataL2 = subset(dataL1, select = c(Student_num, Major, Name, Entrance_year, Number_of_semester, Total_course_credits))

data %>% 
  dplyr::filter(Entrance_year >= 2013) %>% 
  dplyr::select(Student_num, Major, Name, Entrance_year, Number_of_semester, Total_course_credits) 

# Student_num       Major      Name Entrance_year Number_of_semester
# 3    143211003   Chemistry    Sophia          2013                  5
# 4    143211004     Biology     James          2013                  4
# 5    143211005 Mathematics     Jacob          2015                  3
# 7    143211007     Biology    Olivia          2013                  4
# 9    143211009 Mathematics     Colin          2014                  4
# 10   143211010   Chemistry    Amelia          2015                  5
# 12   143211012   Chemistry     Henry          2014                  5
# 13   143211013     Biology    Justin          2013                  3
# 15   143211015 Mathematics   Brandon          2013                  4
# 16   143211016   Chemistry    Eliana          2015                  6
# 18   143211018     Physics     Kevin          2014                  6
# 21   143211021   Chemistry   Michael          2015                  5
# 22   143211022 Mathematics      Nick          2013                  5
# 24   143211024     Biology Elizabeth          2014                  5
# 25   143211025     Physics     Dilan          2015                  2
# 27   143211027   Chemistry      Anna          2014                  2
# 29   143211029     Physics    Audrey          2013                  3
# 30   143211030     Biology      Tony          2015                  2
# Total_course_credits
# 3                    15
# 4                    27
# 5                    12
# 7                    18
# 9                    36
# 10                   48
# 12                   36
# 13                   51
# 15                   60
# 16                   69
# 18                   81
# 21                   51
# 22                   51
# 24                   12
# 25                   24
# 27                   48
# 29                   51
# 30                   81


# ********************************************************************************************
# Q3. 수강한 과목(Course_taken)으로 Major_course 를 수강한 모든 학생들의 
# Student_num, Major. Name. Course_taken. Grade, Course_semester를 
# Console 창에 출력하는 코드 작성 하시오..
# ********************************************************************************************
dataL1 = subset(data, Course_taken %in% c("Major_course"))
dataL2 = subset(dataL1, select = c(Student_num, Major, Name, Course_taken, Grade, Course_semester))

# Student_num       Major   Name Course_taken Grade Course_semester
# 2    143211002 Mathematics Robert Major_course  4.21            Fall
# 6    143211006   Chemistry Austin Major_course  4.37          Spring
# 11   143211011 Mathematics  Floyd Major_course  2.74          Spring
# 19   143211019   Chemistry   Eric Major_course  3.82            Fall
# 23   143211023     Biology   Paul Major_course  3.55          Spring
# 25   143211025     Physics  Dilan Major_course  2.92            Fall
# 28   143211028     Biology   Mark Major_course  3.76          Spring

# ********************************************************************************************
# Q4. 수강한 과목(Course_taken)으로 기초과목(Basic_course)을 수강한 학생들 중에서 
# 입학년도(Entrance_year)가 2013년을 포함하며 그리고 
# 2013년 이후에 입학한 학생들의 전공 (Major)별 총 이수학점 (Total_course_credits) 평균을 
# Console 창에 출력하는 코드 작성하시
# ********************************************************************************************
dataL1 = subset(data, Course_taken %in% c("Basic_course") & Entrance_year >= 2013)
dataL2 = aggregate(dataL1$Total_course_credits, list(dataL1$Major), FUN = mean)

# Group.1    x
# 1     Biology 34.5
# 2   Chemistry 51.0
# 3 Mathematics 36.0
# 4     Physics 81.0


# ********************************************************************************************
# Q5. 봄(Spring)에 수강한 모든 학생들의 전공(Major)별 학점 (Grade) 평균을 구한 후에 
# 전공(Major) 학점 (Grade) 평균이 두 번째로 높은 전공(Major)을 
# Console 창에 출력하는 코드 작성하시오.
# ********************************************************************************************
dataL1 = subset(data, Course_semester %in% c("Spring"))
dataL2 = aggregate(dataL1$Grade, list(dataL1$Major), FUN = mean)
dataL3 = dataL2[order(dataL2$x, decreasing = TRUE), ][2, ]

# Group.1     x
# 4 Physics 3.524


# ********************************************************************************************
# Q6. 기초과목 (Basic_course)에서 최고 학점 (Grade)을 받은 학생이 속한 전공 (Major)의 
# 소속 학생들 중에서 교양과목 (Liberal_arts_course)을 수강한 학생들의 학점 (Grade) 평균 
# Consale 창에 출력하는 코드 작성하시오..
# ********************************************************************************************
# ********************************************************************************************
dataL1 = subset(data, Course_taken %in% c("Basic_course"))
maxGrade = max(dataL1$Grade, na.rm = TRUE)

dataL2 = subset(data, Course_taken %in% c("Basic_course") & Grade == maxGrade)
#    Student_num Name     Major Entrance_year Number_of_semester Total_course_credits
# 27   143211027 Anna Chemistry          2014                  2                   48
# Course_taken Grade Course_semester
# 27 Basic_course  4.35            Fall

dataL3 = subset(data, Major %in% c(dataL2$Major) & Course_taken %in% c("Liberal_arts_course"))
meanGrade = mean(dataL3$Grade, na.rm = TRUE)
# > meanGrade
# [1] 3.49