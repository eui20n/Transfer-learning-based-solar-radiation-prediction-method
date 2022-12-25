# 사용 라이브러리
library(dplyr)

# 경로
setwd("C:\\Users\\kimeu\\Desktop\\졸업논문")
getwd() # "C:/Users/kimeu/Desktop/졸업논문"

# 초기화
rm(list = ls())

#-------------------------------------------------------------------------------
# 데이터 불러오기
Busan <- read.csv("부산.csv")
Daegu <- read.csv("대구.csv")
Daejeon <- read.csv("대전.csv")
Gwangju <- read.csv("광주.csv")
Incheon <- read.csv("인천.csv")
Seoul <- read.csv("서울.csv")

#-------------------------------------------------------------------------------
# 입맛에 맞게 데이터 바꾸기

file_name <- c("Busan", "Daegu", "Daejeon", "Gwangju", "Incheon", "Seoul")

# 데이터 추출 함수 정의
split_data <- function(city_name){
  train_data <- data.frame()
  test_data <- data.frame()
  
  print("각각의 도시의 크기 - 예측 도시 제외")
  
  for(idx in 1:6) {
    if (file_name[idx] == city_name) {
      test_data <- test_data %>%
        rbind(get(file_name[idx]) %>% filter(Year >= 2021))
      next
    }
    
    train_data <- train_data %>%
      rbind(get(file_name[idx]) %>% filter(Year <= 2020))
    print(file_name[[idx]])
    print(get(file_name[idx]) %>% filter(Year <= 2020) %>% nrow())
  }
  
  print("예측해야 하는 도시의 크기")
  print(city_name)
  print(test_data %>% nrow())
  
  return(list(train = train_data, test = test_data))
}

# 대전을 제외한 데이터를 합함
result <- split_data("Daejeon")
train_data <- result$train
test_data <- result$test

train_data %>% nrow() # 47859
test_data %>% nrow() # 4621

loopnum <- nrow(test_data) - 10 # 4611

dataPred01 <- matrix(ncol=ncol(test_data))
dataPred02 <- matrix(ncol=ncol(test_data))
dataPred03 <- matrix(ncol=ncol(test_data))
dataPred04 <- matrix(ncol=ncol(test_data))
dataPred05 <- matrix(ncol=ncol(test_data))
dataPred06 <- matrix(ncol=ncol(test_data))
dataPred07 <- matrix(ncol=ncol(test_data))
dataPred08 <- matrix(ncol=ncol(test_data))
dataPred09 <- matrix(ncol=ncol(test_data))
dataPred10 <- matrix(ncol=ncol(test_data))
dataPred11 <- matrix(ncol=ncol(test_data))

train_data <- train_data %>% select(-c("X", "Year"))
test_data <- test_data %>% select(-c("X", "Year"))

sum(is.na(train_data)) # 0
sum(is.na(test_data)) # 0

# write.csv(train_data, "C:\\Users\\kimeu\\Desktop\\졸업논문\\train.csv")
# write.csv(test_data, "C:\\Users\\kimeu\\Desktop\\졸업논문\\test.csv")

#-------------------------------------------------------------------------------
# 모델 학습

# 시간 측정
start_time <- Sys.time()

# 11시점을 예측하기 위해서 11 만큼만 봐줌
for(i in 1:loopnum){
  i %>% print()
  trainData <- rbind(train_data, test_data[i : (i + 10),])
  model <- ranger(
    Solar~., data = trainData, num.tree = 128, mtry = 3, importance = "impurity"
  )
  
  # method 1
  # for(idx in 1:data.pred %>% length()){
  #   get(data.pred[idx])[i] <- predict(model.ranger, data = test.data[i+idx-1,])
  # }
  
  # method 2
  dataPred01[i] <- predict(model, data = test_data[i + 11,])
  dataPred02[i] <- predict(model, data = test_data[i + 12,])
  dataPred03[i] <- predict(model, data = test_data[i + 13,])
  dataPred04[i] <- predict(model, data = test_data[i + 14,])
  dataPred05[i] <- predict(model, data = test_data[i + 15,])
  dataPred06[i] <- predict(model, data = test_data[i + 16,])
  dataPred07[i] <- predict(model, data = test_data[i + 17,])
  dataPred08[i] <- predict(model, data = test_data[i + 18,])
  dataPred09[i] <- predict(model, data = test_data[i + 19,])
  dataPred10[i] <- predict(model, data = test_data[i + 20,])
  dataPred11[i] <- predict(model, data = test_data[i + 21,])
}

end_time <- Sys.time()
end_time - start_time %>% print()
# 시간이 조금 밖에 안걸리면, 모든 도시에 대해서 하기

rsq <- function (x, y) cor(x, y) ^ 2

Actual <- as.numeric(test_data$Solar[12:4612])
Forecast <- as.numeric(dataPred01)
Error <- Actual - Forecast
R2_01 <- rsq(Actual, Forecast)
RMSE_01 <- sqrt(mean(Error^2))
MAE_01 <- mean(abs(Error))
NRMSE_01 <- (RMSE_01/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_01, RMSE_01, MAE_01, NRMSE_01)
write.csv(Results, "predict_01.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[13:4613]))
Forecast <- na.omit(as.numeric(dataPred02))
Error <- Actual - Forecast
R2_02 <- rsq(Actual, Forecast)
RMSE_02 <- sqrt(mean(Error^2))
MAE_02 <- mean(abs(Error))
NRMSE_02 <- (RMSE_02/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_02, RMSE_02, MAE_02, NRMSE_02)
write.csv(Results, "predict_02.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[14:4614]))
Forecast <- na.omit(as.numeric(dataPred03))
Error <- Actual - Forecast
R2_03 <- rsq(Actual, Forecast)
RMSE_03 <- sqrt(mean(Error^2))
MAE_03 <- mean(abs(Error))
NRMSE_03 <- (RMSE_03/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_03, RMSE_03, MAE_03, NRMSE_03)
write.csv(Results, "predict_03.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[15:4615]))
Forecast <- na.omit(as.numeric(dataPred04))
Error <- Actual - Forecast
R2_04 <- rsq(Actual, Forecast)
RMSE_04 <- sqrt(mean(Error^2))
MAE_04 <- mean(abs(Error))
NRMSE_04 <- (RMSE_04/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_04, RMSE_04, MAE_04, NRMSE_04)
write.csv(Results, "predict_04.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[16:4616]))
Forecast <- na.omit(as.numeric(dataPred05))
Error <- Actual - Forecast
R2_05 <- rsq(Actual, Forecast)
RMSE_05 <- sqrt(mean(Error^2))
MAE_05 <- mean(abs(Error))
NRMSE_05 <- (RMSE_05/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_05, RMSE_05, MAE_05, NRMSE_05)
write.csv(Results, "predict_05.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[17:4617]))
Forecast <- na.omit(as.numeric(dataPred06))
Error <- Actual - Forecast
R2_06 <- rsq(Actual, Forecast)
RMSE_06 <- sqrt(mean(Error^2))
MAE_06 <- mean(abs(Error))
NRMSE_06 <- (RMSE_06/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_06, RMSE_06, MAE_06, NRMSE_06)
write.csv(Results, "predict_06.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[18:4618]))
Forecast <- na.omit(as.numeric(dataPred07))
Error <- Actual - Forecast
R2_07 <- rsq(Actual, Forecast)
RMSE_07 <- sqrt(mean(Error^2))
MAE_07 <- mean(abs(Error))
NRMSE_07 <- (RMSE_07/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_07, RMSE_07, MAE_07, NRMSE_07)
write.csv(Results, "predict_07.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[19:4619]))
Forecast <- na.omit(as.numeric(dataPred08))
Error <- Actual - Forecast
R2_08 <- rsq(Actual, Forecast)
RMSE_08 <- sqrt(mean(Error^2))
MAE_08 <- mean(abs(Error))
NRMSE_08 <- (RMSE_08/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_08, RMSE_08, MAE_08, NRMSE_08)
write.csv(Results, "predict_08.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[20:4620]))
Forecast <- na.omit(as.numeric(dataPred09))
Error <- Actual - Forecast
R2_09 <- rsq(Actual, Forecast)
RMSE_09 <- sqrt(mean(Error^2))
MAE_09 <- mean(abs(Error))
NRMSE_09 <- (RMSE_09/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_09, RMSE_09, MAE_09, NRMSE_09)
write.csv(Results, "predict_09.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[21:4621]))
Forecast <- na.omit(as.numeric(dataPred10))
Error <- Actual - Forecast
R2_10 <- rsq(Actual, Forecast)
RMSE_10 <- sqrt(mean(Error^2))
MAE_10 <- mean(abs(Error))
NRMSE_10 <- (RMSE_10/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_10, RMSE_10, MAE_10, NRMSE_10)
write.csv(Results, "predict_10.csv", row.names = FALSE)

Actual <- na.omit(as.numeric(test_data$Solar[22:4622]))
Forecast <- na.omit(as.numeric(dataPred11))
Error <- Actual - Forecast
R2_11 <- rsq(Actual, Forecast)
RMSE_11 <- sqrt(mean(Error^2))
MAE_11 <- mean(abs(Error))
NRMSE_11 <- (RMSE_11/mean(Actual))*100
Results <- data.frame(Actual, Forecast, Error, R2_11, RMSE_11, MAE_11, NRMSE_11)
write.csv(Results, "predict_11.csv", row.names = FALSE)
