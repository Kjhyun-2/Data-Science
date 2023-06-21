#디렉토리 변경
getwd()
setwd("C:/Users/wlgus/Desktop/2학년 1학기/데이터사이언스기초/기말고사 프로젝트 데이터")
# 필요한 패키지 불러오기
install.packages("tidyverse")
install.packages("gapminder")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gapminder)
library(readr)
###############################서울 연도별 교통량에따른 데이터 분석###############################
# csv 파일 읽기
train <- read_csv("교량지점별+교통량_20230530173242.csv",col_names = TRUE, na = "")
train

#연도 데이터 추출
year_cols <- names(train)[3:ncol(train)]
years <- as.numeric(year_cols)
# 데이터 재구조화
train_save <- train %>%
  select("교량지점별(1)", "교량지점별(2)", year_cols) %>%
  mutate(across(starts_with("교량지점별"), as.character)) %>%
  mutate(across(year_cols, as.character)) %>%
  pivot_longer(cols = -c("교량지점별(1)", "교량지점별(2)"), names_to = "연도", values_to = "교통량") %>%
  mutate(연도 = as.numeric(연도))

train_save$`교량지점별(2)` <- as.character(train_save$`교량지점별(2)`)

# 범주형 데이터인 "교통량"을 숫자형 데이터로 변환
train_save$교통량 <- as.numeric(as.character(train_save$교통량))

# 교통량에서 NA값을 제외한 최소값과 최대값 구하기
min_value <- min(na.omit(train_save$교통량))
min_value
max_value <- 230000 # 각 교량 지점별 교통량에 y축 최대치

# 그래프 그리기 
# 분할하지 않고 교량지점별을 한번에 표시해서 그래프를 그릴경우
#ggplot(train_save, aes(x = 연도, y = 교통량, color = `교량지점별(2)`)) +
#  geom_line() +
#  labs(x = "연도", y = "교통량", color = "교량지점별") +
#  scale_color_manual(values = rainbow(length(unique(train_save$`교량지점별(2)`)))) +
#  theme_minimal() +
#  geom_point() +
#  scale_y_continuous(limits = c(min_value, max_value), breaks = seq(min_value, max_value, by = 50000)) +
#  x11()

# 교량지점별로 그래프를 분할하여 그리기
ggplot(train_save, aes(x = 연도, y = 교통량, color = `교량지점별(2)`)) +
  geom_line() +
  labs(x = "연도", y = "교통량", color = "교량지점별") +
  scale_color_manual(values = rainbow(length(unique(train_save$`교량지점별(2)`)))) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(limits = c(min_value, max_value), breaks = seq(min_value, max_value, by = 50000)) +
  facet_wrap(~ `교량지점별(2)`, nrow = 5) +
  x11()

#총 합계에 대한 max 값이 나옴
max_value <- max(na.omit(train_save$교통량))
max_value
# 소계(총합)에 대한 그래프 그리기
ggplot(train_save, aes(x = 연도, y = 교통량, color = `교량지점별(2)`)) +
  geom_line() +
  labs(x = "연도", y = "교통량", color = "교량지점별") +
  scale_color_manual(values = rainbow(length(unique(train_save$`교량지점별(2)`)))) +
  theme_minimal() +
  geom_point() +
  scale_y_continuous(limits = c(600000, max_value), breaks = seq(min_value, max_value, by = 100000)) +
  x11()

#연도별 교통량 종합 예측

# 도로명 합계에 해당하는 데이터 추출
train_sum_data <- train_save[train_save$`교량지점별(2)` == "소계", ]

# 3차 모델 구축
model <- lm(교통량 ~ poly(연도, 3), data = train_sum_data)

# 모델 요약 통계 출력
summary(model)

# 데이터의 산점도 그리기
plot(train_sum_data$연도, train_sum_data$교통량, xlab = "연도", ylab = "교통량", main = "교통량 예측")

# 3차 모델 예측선 그리기
lines(train_sum_data$연도, predict(model, newdata = train_sum_data), col = "red")

# 2025년에 대한 예측 수행
new_data <- data.frame(연도 = 2025)
predicted <- predict(model, newdata = new_data)
predicted_value <- predicted[length(predicted)]
predicted_value

##############서울 2021년 장소별(고속도로)교통량에따른 데이터##############
location_train <- read_csv("서울도시고속도로 월간교통소통 통계정보_2021년 (1).csv", col_names = TRUE, na = "", locale = locale(encoding = "UTF-8"))
location_train

# 월별 데이터 추출을 위해 열 이름을 `평일 평균 교통량 (대표구간)`에서 `평일_교통량`으로 변경
colnames(location_train)[4] <- "교통량"

# 데이터 전처리 - 도로명과 교통량합 추출
location_sum <- location_train %>%
  group_by(도로명) %>%
  summarise(교통량_합 = sum(as.integer(교통량), na.rm = TRUE)) %>% 
  na.omit()
location_sum

# 숫자를 일반 형식으로 변환하는 사용자 정의 함수
change_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

# 그래프 생성
ggplot(location_sum, aes(x = 도로명, y = 교통량_합, fill = 도로명)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "도로명", y = "교통량 합", fill = "도로명") +
  ggtitle("도로명별 교통량 합") +
  scale_y_continuous(labels = change_number) +
  coord_cartesian(ylim = c(min(location_sum$교통량_합), max(location_sum$교통량_합))) +
  x11()


##########################서울 2021년 월별 교통량에따른 데이터 분석##########################
#csv파일 읽기
month_train <- read_csv("서울도시고속도로 월간교통소통 통계정보_2021년 (1).csv", col_names = TRUE, na = "", locale = locale(encoding = "UTF-8"))
month_train
# 월별 데이터 추출을 위해 열 이름을 `평일 평균 교통량 (대표구간)`에서 `교통량`으로 변경
colnames(month_train)[4] <- "교통량"

# 월별 데이터 추출
monthly_data <- month_train %>%
  mutate(Month = as.integer(substr(월, 1, 2))) %>%
  group_by(Month, 도로명) %>%
  summarise(monthsum = sum(교통량, na.rm = TRUE)) %>%
  na.omit()

# 그래프 생성 - 도로명 별로 분리하여 그리기

ggplot(monthly_data, aes(x = factor(Month, levels = 1:12), y = monthsum, fill = 도로명)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(x = "월", y = "교통량", fill = "도로명") +
  ggtitle("월별 도로명 교통량") +
  facet_wrap(~ 도로명, nrow = 2) +
  x11()

# 월별 도로별 교통량 합계에 대한 그래프 그리기
monthly_sum <- monthly_data %>%
  group_by(Month) %>%
  summarise(total_sum = sum(monthsum))

min_month_sum <- min(monthly_sum$total_sum)
max_month_sum <- max(monthly_sum$total_sum)

ggplot(monthly_sum, aes(x = factor(Month), y = total_sum, fill = "합계")) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(x = "월", y = "교통량", fill = "도로명 전체") +
  ggtitle("월별 교통량 합계") +
  coord_cartesian(ylim = c(min(monthly_sum$total_sum), max(monthly_sum$total_sum))) +
  x11()

#월별 교통량 종합 예측
# 3차 모델로 예측하기
new_data <- data.frame(Month = 13:24)

model_3 <- lm(monthly_sum$total_sum ~ poly(monthly_sum$Month, degree = 3), data = monthly_sum)
current_month <- predict(model_3, newdata = new_data)

# 예측 결과를 데이터프레임으로 정리
predicted_data_3 <- data.frame(Month = new_data$Month, Predicted_3 = current_month)

# 다음해 월별 교통량 예측값을 저장
predicted_data_3$total_sum <- monthly_sum$total_sum
# 3차 모델 예측선 그리기기
plot(predicted_data_3$Month, predicted_data_3$Predicted_3, type = "l", col = "blue", xlab = "다음해 월", ylab = "월별 교통량", main = "다음해 월별 교통량 예측 그래프")
points(predicted_data_3$Month, predicted_data_3$total_sum, col = "red", pch = 16)

