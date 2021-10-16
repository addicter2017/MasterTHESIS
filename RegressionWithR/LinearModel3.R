library(psych)
library(report)
library(stargazer)
library(tidyverse)
library(broom)
library(dummy)
data = read.csv('data_all.csv')
names(data) = c(
  'destinations',
  'length',
  'width',
  'height',
  'volume',
  'weight',
  'commodity',
  'fragile',
  'iflammable',
  'fits_rate',
  'types'
)


# outlier test and remove
data_pre = data
# deal with categorical variables
#des_col = model.matrix(~ destinations, data_pre)[,-1]
#com_col = model.matrix(~ commodity, data_pre)[,-1]
com_col = dummy(data_pre)[, c(14:25)]
des_col = dummy(data_pre)[, c(1:6, 8:13)]
# combine new columns and delete old categorical columns
 


write.csv(data_combine,
          file = "data_combine.csv",
          quote = F,
          row.names = F)

## begin here!!!

data_combine = read.csv("data_combine.csv")


hist(data_combine[,1],xlab = 'Length',main = 'Histogram of Length (cm)')
hist(data_combine[,2],xlab = 'Width',main = 'Histogram of Width (cm)')
hist(data_combine[,3],xlab = 'Height',main = 'Histogram of Height (cm)')
hist(data_combine[,4],xlab = 'Volume',main = 'Histogram of Volume (cm)')



# This function will save the model to a csv file
saveModel <- function(file_addr, model) {
  ## save the result
  tidy_model = tidy(model)
  write.csv(tidy_model, file = file_addr)
  
}


# This function will draw the scatter plot and generate a Rmse
drawScatter <- function(model, colume_id) {
  colume_id = as.integer(colume_id)
  xlabel = 1:length(data_combine[, 1])
  pred = predict(model, data_combine, type = 'response')
  Rmse = sqrt(mean((pred - data_combine[, colume_id]) ^ 2))
  pred = sort(pred)
  plot(xlabel, pred)
  plot(
    xlabel,
    pred,
    xlab = "",
    ylab = "",
    col = "red",
    type = "b",
    pch = 4
  )
  lines(xlabel,
        sort(data_combine[, colume_id]),
        col = "blue",
        type = "b")
  legend(
    "topleft",
    inset = .05,
    c("prediction", "raw data"),
    lty = c(1, 2),
    pch = c(4, 1),
    col = c("red", "blue")
  )
  
  return(Rmse)
  
}

#This function will predict the one_year data and save it according to the file name
OneYearPred = function(model, dataset, file_addr) {
  oneYearpredict = predict(model, dataset, type = 'response')
  write.csv(
    oneYearpredict,
    file = file_addr,
    quote = F,
    row.names = F
  )
}


## Deal with one year dataset, code removed when it is done
## predicting volume for one year data
#data_one_year = read.csv('data_oneyear.csv')
#names(data_one_year) = c('destinations','length','width','height','volume','weight','commodity','fragile','iflammable','fits_rate','types')
# deal with categorical variables
#com_col = dummy(data_one_year)[, c(14:22)]
#des_col = dummy(data_one_year)[, c(1:6, 8:13)]
# # combine new columns and delete old categorical columns
#data_oneyear_combine = cbind(data_one_year[,-c(1,7)],des_col,com_col)
#write.csv(data_oneyear_combine,file="data1year_combine.csv",quote=F,row.names = F)



# GLM model for Volume
fit_volume = glm(
  data_combine[, 4] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13] + data_combine[, 14] + data_combine[, 15] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23] + data_combine[, 24] +
    data_combine[, 25] + data_combine[, 26] + data_combine[, 27] +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30] +
    data_combine[, 31] + data_combine[, 32] + data_combine[, 33] +
    data_combine[, 10] * data_combine[, 22] + data_combine[, 11] * data_combine[, 22] +
    data_combine[, 12] * data_combine[, 22] + data_combine[, 13] * data_combine[, 22] +
    data_combine[, 14] * data_combine[, 22] + data_combine[, 15] * data_combine[, 22] +
    data_combine[, 16] * data_combine[, 22] + data_combine[, 17] * data_combine[, 22] +
    data_combine[, 18] * data_combine[, 22] + data_combine[, 19] * data_combine[, 22] +
    data_combine[, 20] * data_combine[, 22] + data_combine[, 21] * data_combine[, 22] +
    data_combine[, 10] * data_combine[, 23] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] + data_combine[, 13] * data_combine[, 23] +
    data_combine[, 14] * data_combine[, 23] + data_combine[, 15] * data_combine[, 23] +
    data_combine[, 16] * data_combine[, 23] + data_combine[, 17] * data_combine[, 23] +
    data_combine[, 18] * data_combine[, 23] + data_combine[, 19] * data_combine[, 23] +
    data_combine[, 20] * data_combine[, 23] + data_combine[, 21] * data_combine[, 23] +
    data_combine[, 10] * data_combine[, 24] + data_combine[, 11] * data_combine[, 24] +
    data_combine[, 12] * data_combine[, 24] + data_combine[, 13] * data_combine[, 24] +
    data_combine[, 14] * data_combine[, 24] + data_combine[, 15] * data_combine[, 24] +
    data_combine[, 16] * data_combine[, 24] + data_combine[, 17] * data_combine[, 24] +
    data_combine[, 18] * data_combine[, 24] + data_combine[, 19] * data_combine[, 24] +
    data_combine[, 20] * data_combine[, 24] + data_combine[, 21] * data_combine[, 24] +
    data_combine[, 10] * data_combine[, 25] + data_combine[, 11] * data_combine[, 25] +
    data_combine[, 12] * data_combine[, 25] + data_combine[, 13] * data_combine[, 25] +
    data_combine[, 14] * data_combine[, 25] + data_combine[, 15] * data_combine[, 25] +
    data_combine[, 16] * data_combine[, 25] + data_combine[, 17] * data_combine[, 25] +
    data_combine[, 18] * data_combine[, 25] + data_combine[, 19] * data_combine[, 25] +
    data_combine[, 20] * data_combine[, 25] + data_combine[, 21] * data_combine[, 25] +
    data_combine[, 10] * data_combine[, 26] + data_combine[, 11] * data_combine[, 26] +
    data_combine[, 12] * data_combine[, 26] + data_combine[, 13] * data_combine[, 26] +
    data_combine[, 14] * data_combine[, 26] + data_combine[, 15] * data_combine[, 26] +
    data_combine[, 16] * data_combine[, 26] + data_combine[, 17] * data_combine[, 26] +
    data_combine[, 18] * data_combine[, 26] + data_combine[, 19] * data_combine[, 26] +
    data_combine[, 20] * data_combine[, 26] + data_combine[, 21] * data_combine[, 26] +
    data_combine[, 10] * data_combine[, 27] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] + data_combine[, 13] * data_combine[, 27] +
    data_combine[, 14] * data_combine[, 27] + data_combine[, 15] * data_combine[, 27] +
    data_combine[, 16] * data_combine[, 27] + data_combine[, 17] * data_combine[, 27] +
    data_combine[, 18] * data_combine[, 27] + data_combine[, 19] * data_combine[, 27] +
    data_combine[, 20] * data_combine[, 27] + data_combine[, 21] * data_combine[, 27] +
    data_combine[, 10] * data_combine[, 28] + data_combine[, 11] * data_combine[, 28] +
    data_combine[, 12] * data_combine[, 28] + data_combine[, 13] * data_combine[, 28] +
    data_combine[, 14] * data_combine[, 28] + data_combine[, 15] * data_combine[, 28] +
    data_combine[, 16] * data_combine[, 28] + data_combine[, 17] * data_combine[, 28] +
    data_combine[, 18] * data_combine[, 28] + data_combine[, 19] * data_combine[, 28] +
    data_combine[, 20] * data_combine[, 28] + data_combine[, 21] * data_combine[, 28] +
    data_combine[, 10] * data_combine[, 29] + data_combine[, 11] * data_combine[, 29] +
    data_combine[, 12] * data_combine[, 29] + data_combine[, 13] * data_combine[, 29] +
    data_combine[, 14] * data_combine[, 29] + data_combine[, 15] * data_combine[, 29] +
    data_combine[, 16] * data_combine[, 29] + data_combine[, 17] * data_combine[, 29] +
    data_combine[, 18] * data_combine[, 29] + data_combine[, 19] * data_combine[, 29] +
    data_combine[, 20] * data_combine[, 29] + data_combine[, 21] * data_combine[, 29] +
    data_combine[, 10] * data_combine[, 30] + data_combine[, 11] * data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] + data_combine[, 13] * data_combine[, 30] +
    data_combine[, 14] * data_combine[, 30] + data_combine[, 15] * data_combine[, 30] +
    data_combine[, 16] * data_combine[, 30] + data_combine[, 17] * data_combine[, 30] +
    data_combine[, 18] * data_combine[, 30] + data_combine[, 19] * data_combine[, 30] +
    data_combine[, 20] * data_combine[, 30] + data_combine[, 21] * data_combine[, 30] +
    data_combine[, 10] * data_combine[, 31] + data_combine[, 11] * data_combine[, 31] +
    data_combine[, 12] * data_combine[, 31] + data_combine[, 13] * data_combine[, 31] +
    data_combine[, 14] * data_combine[, 31] + data_combine[, 15] * data_combine[, 31] +
    data_combine[, 16] * data_combine[, 31] + data_combine[, 17] * data_combine[, 31] +
    data_combine[, 18] * data_combine[, 31] + data_combine[, 19] * data_combine[, 31] +
    data_combine[, 20] * data_combine[, 31] + data_combine[, 21] * data_combine[, 31] +
    data_combine[, 10] * data_combine[, 32] + data_combine[, 11] * data_combine[, 32] +
    data_combine[, 12] * data_combine[, 32] + data_combine[, 13] * data_combine[, 32] +
    data_combine[, 14] * data_combine[, 32] + data_combine[, 15] * data_combine[, 32] +
    data_combine[, 16] * data_combine[, 32] + data_combine[, 17] * data_combine[, 32] +
    data_combine[, 18] * data_combine[, 32] + data_combine[, 19] * data_combine[, 32] +
    data_combine[, 20] * data_combine[, 32] + data_combine[, 21] * data_combine[, 32] +
    data_combine[, 10] * data_combine[, 33] + data_combine[, 11] * data_combine[, 33] +
    data_combine[, 12] * data_combine[, 33] + data_combine[, 13] * data_combine[, 33] +
    data_combine[, 14] * data_combine[, 33] + data_combine[, 15] * data_combine[, 33] +
    data_combine[, 16] * data_combine[, 33] + data_combine[, 17] * data_combine[, 33] +
    data_combine[, 18] * data_combine[, 33] + data_combine[, 19] * data_combine[, 33] +
    data_combine[, 20] * data_combine[, 33] + data_combine[, 21] * data_combine[, 33] +
    data_combine[, 10] * data_combine[, 9] + data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 9] + data_combine[, 23] * data_combine[, 9] +
    data_combine[, 12] * data_combine[, 9] + data_combine[, 24] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 9] + data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 9] + data_combine[, 26] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 9] + data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 9] + data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 9] + data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 9] + data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 9] + data_combine[, 31] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 9] + data_combine[, 32] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 9] + data_combine[, 33] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 22] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 22] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 15] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 19] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 22] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 14] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 18] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 23] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 10] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 24] + data_combine[, 12] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 13] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 24] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 17] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 24] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 21] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 25] + data_combine[, 12] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 25] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 16] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 25] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 20] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 15] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 19] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 26] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 14] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 27] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 18] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 27] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 10] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 28] + data_combine[, 12] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 13] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 28] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 17] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 21] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 29] + data_combine[, 12] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 29] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 16] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 29] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 20] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 30] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 30] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 16] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 19] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 30] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 10] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 11] * data_combine[, 31] + data_combine[, 12] * data_combine[, 31] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 31] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 16] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 19] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 31] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 10] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 32] + data_combine[, 12] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 13] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 32] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 17] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 21] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 33] + data_combine[, 12] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 33] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 16] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 33] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 20] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 33] * data_combine[, 9] ,
  family = Gamma(link = "log"),
  data = data_combine
)

drawScatter(model = fit_v_up, 4)




## Plot the prediction versus original data
fit_v = step(fit_volume, direction = 'both')

fit_v_up = glm(
  formula = data_combine[, 4] ~ data_combine[, 9]
  + data_combine[, 13]
  + data_combine[, 15] + data_combine[, 16] + data_combine[, 17]
  
  + data_combine[, 21] + data_combine[, 22] + data_combine[, 23]
  + data_combine[, 25]
  + data_combine[, 29] + data_combine[, 30]
  + data_combine[, 10]:data_combine[, 25]
  + data_combine[, 14]:data_combine[, 25] + data_combine[, 15]:data_combine[, 25]
  + data_combine[, 18]:data_combine[, 25] + data_combine[, 19]:data_combine[, 25]
  
  + data_combine[, 15]:data_combine[, 30]
  ,
  family = Gamma(link = "log"),
  data = data_combine
)
summary(fit_v_up)

## save the result
saveModel("tidyVolumeGamma.csv", model = fit_v_up)



## prediction for one year
data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
OneYearPred(fit_v, data_combine, "VolumeOneYear.csv")
data_combine = data_medium




## nomal model for length,
# Length
fit_length_linear = lm(
  data_combine[, 1] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13] + data_combine[, 14] + data_combine[, 15] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23] + data_combine[, 24] +
    data_combine[, 25] + data_combine[, 26] + data_combine[, 27] +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30] +
    data_combine[, 31] + data_combine[, 32] + data_combine[, 33] +
    data_combine[, 10] * data_combine[, 22] + data_combine[, 11] * data_combine[, 22] +
    data_combine[, 12] * data_combine[, 22] + data_combine[, 13] * data_combine[, 22] +
    data_combine[, 14] * data_combine[, 22] + data_combine[, 15] * data_combine[, 22] +
    data_combine[, 16] * data_combine[, 22] + data_combine[, 17] * data_combine[, 22] +
    data_combine[, 18] * data_combine[, 22] + data_combine[, 19] * data_combine[, 22] +
    data_combine[, 20] * data_combine[, 22] + data_combine[, 21] * data_combine[, 22] +
    data_combine[, 10] * data_combine[, 23] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] + data_combine[, 13] * data_combine[, 23] +
    data_combine[, 14] * data_combine[, 23] + data_combine[, 15] * data_combine[, 23] +
    data_combine[, 16] * data_combine[, 23] + data_combine[, 17] * data_combine[, 23] +
    data_combine[, 18] * data_combine[, 23] + data_combine[, 19] * data_combine[, 23] +
    data_combine[, 20] * data_combine[, 23] + data_combine[, 21] * data_combine[, 23] +
    data_combine[, 10] * data_combine[, 24] + data_combine[, 11] * data_combine[, 24] +
    data_combine[, 12] * data_combine[, 24] + data_combine[, 13] * data_combine[, 24] +
    data_combine[, 14] * data_combine[, 24] + data_combine[, 15] * data_combine[, 24] +
    data_combine[, 16] * data_combine[, 24] + data_combine[, 17] * data_combine[, 24] +
    data_combine[, 18] * data_combine[, 24] + data_combine[, 19] * data_combine[, 24] +
    data_combine[, 20] * data_combine[, 24] + data_combine[, 21] * data_combine[, 24] +
    data_combine[, 10] * data_combine[, 25] + data_combine[, 11] * data_combine[, 25] +
    data_combine[, 12] * data_combine[, 25] + data_combine[, 13] * data_combine[, 25] +
    data_combine[, 14] * data_combine[, 25] + data_combine[, 15] * data_combine[, 25] +
    data_combine[, 16] * data_combine[, 25] + data_combine[, 17] * data_combine[, 25] +
    data_combine[, 18] * data_combine[, 25] + data_combine[, 19] * data_combine[, 25] +
    data_combine[, 20] * data_combine[, 25] + data_combine[, 21] * data_combine[, 25] +
    data_combine[, 10] * data_combine[, 26] + data_combine[, 11] * data_combine[, 26] +
    data_combine[, 12] * data_combine[, 26] + data_combine[, 13] * data_combine[, 26] +
    data_combine[, 14] * data_combine[, 26] + data_combine[, 15] * data_combine[, 26] +
    data_combine[, 16] * data_combine[, 26] + data_combine[, 17] * data_combine[, 26] +
    data_combine[, 18] * data_combine[, 26] + data_combine[, 19] * data_combine[, 26] +
    data_combine[, 20] * data_combine[, 26] + data_combine[, 21] * data_combine[, 26] +
    data_combine[, 10] * data_combine[, 27] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] + data_combine[, 13] * data_combine[, 27] +
    data_combine[, 14] * data_combine[, 27] + data_combine[, 15] * data_combine[, 27] +
    data_combine[, 16] * data_combine[, 27] + data_combine[, 17] * data_combine[, 27] +
    data_combine[, 18] * data_combine[, 27] + data_combine[, 19] * data_combine[, 27] +
    data_combine[, 20] * data_combine[, 27] + data_combine[, 21] * data_combine[, 27] +
    data_combine[, 10] * data_combine[, 28] + data_combine[, 11] * data_combine[, 28] +
    data_combine[, 12] * data_combine[, 28] + data_combine[, 13] * data_combine[, 28] +
    data_combine[, 14] * data_combine[, 28] + data_combine[, 15] * data_combine[, 28] +
    data_combine[, 16] * data_combine[, 28] + data_combine[, 17] * data_combine[, 28] +
    data_combine[, 18] * data_combine[, 28] + data_combine[, 19] * data_combine[, 28] +
    data_combine[, 20] * data_combine[, 28] + data_combine[, 21] * data_combine[, 28] +
    data_combine[, 10] * data_combine[, 29] + data_combine[, 11] * data_combine[, 29] +
    data_combine[, 12] * data_combine[, 29] + data_combine[, 13] * data_combine[, 29] +
    data_combine[, 14] * data_combine[, 29] + data_combine[, 15] * data_combine[, 29] +
    data_combine[, 16] * data_combine[, 29] + data_combine[, 17] * data_combine[, 29] +
    data_combine[, 18] * data_combine[, 29] + data_combine[, 19] * data_combine[, 29] +
    data_combine[, 20] * data_combine[, 29] + data_combine[, 21] * data_combine[, 29] +
    data_combine[, 10] * data_combine[, 30] + data_combine[, 11] * data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] + data_combine[, 13] * data_combine[, 30] +
    data_combine[, 14] * data_combine[, 30] + data_combine[, 15] * data_combine[, 30] +
    data_combine[, 16] * data_combine[, 30] + data_combine[, 17] * data_combine[, 30] +
    data_combine[, 18] * data_combine[, 30] + data_combine[, 19] * data_combine[, 30] +
    data_combine[, 20] * data_combine[, 30] + data_combine[, 21] * data_combine[, 30] +
    data_combine[, 10] * data_combine[, 31] + data_combine[, 11] * data_combine[, 31] +
    data_combine[, 12] * data_combine[, 31] + data_combine[, 13] * data_combine[, 31] +
    data_combine[, 14] * data_combine[, 31] + data_combine[, 15] * data_combine[, 31] +
    data_combine[, 16] * data_combine[, 31] + data_combine[, 17] * data_combine[, 31] +
    data_combine[, 18] * data_combine[, 31] + data_combine[, 19] * data_combine[, 31] +
    data_combine[, 20] * data_combine[, 31] + data_combine[, 21] * data_combine[, 31] +
    data_combine[, 10] * data_combine[, 32] + data_combine[, 11] * data_combine[, 32] +
    data_combine[, 12] * data_combine[, 32] + data_combine[, 13] * data_combine[, 32] +
    data_combine[, 14] * data_combine[, 32] + data_combine[, 15] * data_combine[, 32] +
    data_combine[, 16] * data_combine[, 32] + data_combine[, 17] * data_combine[, 32] +
    data_combine[, 18] * data_combine[, 32] + data_combine[, 19] * data_combine[, 32] +
    data_combine[, 20] * data_combine[, 32] + data_combine[, 21] * data_combine[, 32] +
    data_combine[, 10] * data_combine[, 33] + data_combine[, 11] * data_combine[, 33] +
    data_combine[, 12] * data_combine[, 33] + data_combine[, 13] * data_combine[, 33] +
    data_combine[, 14] * data_combine[, 33] + data_combine[, 15] * data_combine[, 33] +
    data_combine[, 16] * data_combine[, 33] + data_combine[, 17] * data_combine[, 33] +
    data_combine[, 18] * data_combine[, 33] + data_combine[, 19] * data_combine[, 33] +
    data_combine[, 20] * data_combine[, 33] + data_combine[, 21] * data_combine[, 33] +
    data_combine[, 10] * data_combine[, 9] + data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 9] + data_combine[, 23] * data_combine[, 9] +
    data_combine[, 12] * data_combine[, 9] + data_combine[, 24] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 9] + data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 9] + data_combine[, 26] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 9] + data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 9] + data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 9] + data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 9] + data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 9] + data_combine[, 31] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 9] + data_combine[, 32] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 9] + data_combine[, 33] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 22] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 22] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 15] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 19] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 22] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 14] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 18] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 23] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 10] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 24] + data_combine[, 12] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 13] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 24] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 17] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 24] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 21] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 25] + data_combine[, 12] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 25] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 16] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 25] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 20] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 15] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 19] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 26] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 14] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 27] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 18] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 27] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 10] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 28] + data_combine[, 12] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 13] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 28] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 17] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 21] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 29] + data_combine[, 12] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 29] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 16] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 29] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 20] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 30] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 30] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 16] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 19] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 30] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 10] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 11] * data_combine[, 31] + data_combine[, 12] * data_combine[, 31] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 31] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 16] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 19] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 31] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 10] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 32] + data_combine[, 12] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 13] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 32] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 17] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 21] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 33] + data_combine[, 12] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 33] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 16] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 33] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 20] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 33] * data_combine[, 9]
  ,
  data = data_combine
)
## Length plot
fit_l_linear = step(fit_length_linear)
fit_l_linear_up = lm(
  data_combine[, 1] ~  data_combine[, 13] + data_combine[, 15]  + data_combine[, 17] + data_combine[, 18] +  data_combine[, 23] + data_combine[, 24] +
    data_combine[, 28] + data_combine[, 29]  +
    data_combine[, 13]:data_combine[, 22]   + data_combine[, 15]:data_combine[, 28] + data_combine[, 18]:data_combine[, 28] +
    data_combine[, 10]:data_combine[, 29] +
    data_combine[, 16]:data_combine[, 29]    + data_combine[, 10]:data_combine[, 33],
  data_combine
)


summary(fit_l_linear_up)
# plot
RMSEL.Linear = drawScatter(fit_l_linear_up, 1)
plot(fit_l_linear_up)
## save the result
saveModel("tidylengthlinear.csv", model = fit_l_linear_up)

# predicting for one year
data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
OneYearPred(fit_l_linear_up, data_combine, "LlinearOneYear.csv")
data_combine = data_medium




## Length Gamma Regression
fit_length_gamma = glm(
  data_combine[, 1] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13] + data_combine[, 14] + data_combine[, 15] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23] + data_combine[, 24] +
    data_combine[, 25] + data_combine[, 26] + data_combine[, 27] +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30] +
    data_combine[, 31] + data_combine[, 32] + data_combine[, 33] +
    data_combine[, 10] * data_combine[, 22] + data_combine[, 11] * data_combine[, 22] +
    data_combine[, 12] * data_combine[, 22] + data_combine[, 13] * data_combine[, 22] +
    data_combine[, 14] * data_combine[, 22] + data_combine[, 15] * data_combine[, 22] +
    data_combine[, 16] * data_combine[, 22] + data_combine[, 17] * data_combine[, 22] +
    data_combine[, 18] * data_combine[, 22] + data_combine[, 19] * data_combine[, 22] +
    data_combine[, 20] * data_combine[, 22] + data_combine[, 21] * data_combine[, 22] +
    data_combine[, 10] * data_combine[, 23] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] + data_combine[, 13] * data_combine[, 23] +
    data_combine[, 14] * data_combine[, 23] + data_combine[, 15] * data_combine[, 23] +
    data_combine[, 16] * data_combine[, 23] + data_combine[, 17] * data_combine[, 23] +
    data_combine[, 18] * data_combine[, 23] + data_combine[, 19] * data_combine[, 23] +
    data_combine[, 20] * data_combine[, 23] + data_combine[, 21] * data_combine[, 23] +
    data_combine[, 10] * data_combine[, 24] + data_combine[, 11] * data_combine[, 24] +
    data_combine[, 12] * data_combine[, 24] + data_combine[, 13] * data_combine[, 24] +
    data_combine[, 14] * data_combine[, 24] + data_combine[, 15] * data_combine[, 24] +
    data_combine[, 16] * data_combine[, 24] + data_combine[, 17] * data_combine[, 24] +
    data_combine[, 18] * data_combine[, 24] + data_combine[, 19] * data_combine[, 24] +
    data_combine[, 20] * data_combine[, 24] + data_combine[, 21] * data_combine[, 24] +
    data_combine[, 10] * data_combine[, 25] + data_combine[, 11] * data_combine[, 25] +
    data_combine[, 12] * data_combine[, 25] + data_combine[, 13] * data_combine[, 25] +
    data_combine[, 14] * data_combine[, 25] + data_combine[, 15] * data_combine[, 25] +
    data_combine[, 16] * data_combine[, 25] + data_combine[, 17] * data_combine[, 25] +
    data_combine[, 18] * data_combine[, 25] + data_combine[, 19] * data_combine[, 25] +
    data_combine[, 20] * data_combine[, 25] + data_combine[, 21] * data_combine[, 25] +
    data_combine[, 10] * data_combine[, 26] + data_combine[, 11] * data_combine[, 26] +
    data_combine[, 12] * data_combine[, 26] + data_combine[, 13] * data_combine[, 26] +
    data_combine[, 14] * data_combine[, 26] + data_combine[, 15] * data_combine[, 26] +
    data_combine[, 16] * data_combine[, 26] + data_combine[, 17] * data_combine[, 26] +
    data_combine[, 18] * data_combine[, 26] + data_combine[, 19] * data_combine[, 26] +
    data_combine[, 20] * data_combine[, 26] + data_combine[, 21] * data_combine[, 26] +
    data_combine[, 10] * data_combine[, 27] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] + data_combine[, 13] * data_combine[, 27] +
    data_combine[, 14] * data_combine[, 27] + data_combine[, 15] * data_combine[, 27] +
    data_combine[, 16] * data_combine[, 27] + data_combine[, 17] * data_combine[, 27] +
    data_combine[, 18] * data_combine[, 27] + data_combine[, 19] * data_combine[, 27] +
    data_combine[, 20] * data_combine[, 27] + data_combine[, 21] * data_combine[, 27] +
    data_combine[, 10] * data_combine[, 28] + data_combine[, 11] * data_combine[, 28] +
    data_combine[, 12] * data_combine[, 28] + data_combine[, 13] * data_combine[, 28] +
    data_combine[, 14] * data_combine[, 28] + data_combine[, 15] * data_combine[, 28] +
    data_combine[, 16] * data_combine[, 28] + data_combine[, 17] * data_combine[, 28] +
    data_combine[, 18] * data_combine[, 28] + data_combine[, 19] * data_combine[, 28] +
    data_combine[, 20] * data_combine[, 28] + data_combine[, 21] * data_combine[, 28] +
    data_combine[, 10] * data_combine[, 29] + data_combine[, 11] * data_combine[, 29] +
    data_combine[, 12] * data_combine[, 29] + data_combine[, 13] * data_combine[, 29] +
    data_combine[, 14] * data_combine[, 29] + data_combine[, 15] * data_combine[, 29] +
    data_combine[, 16] * data_combine[, 29] + data_combine[, 17] * data_combine[, 29] +
    data_combine[, 18] * data_combine[, 29] + data_combine[, 19] * data_combine[, 29] +
    data_combine[, 20] * data_combine[, 29] + data_combine[, 21] * data_combine[, 29] +
    data_combine[, 10] * data_combine[, 30] + data_combine[, 11] * data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] + data_combine[, 13] * data_combine[, 30] +
    data_combine[, 14] * data_combine[, 30] + data_combine[, 15] * data_combine[, 30] +
    data_combine[, 16] * data_combine[, 30] + data_combine[, 17] * data_combine[, 30] +
    data_combine[, 18] * data_combine[, 30] + data_combine[, 19] * data_combine[, 30] +
    data_combine[, 20] * data_combine[, 30] + data_combine[, 21] * data_combine[, 30] +
    data_combine[, 10] * data_combine[, 31] + data_combine[, 11] * data_combine[, 31] +
    data_combine[, 12] * data_combine[, 31] + data_combine[, 13] * data_combine[, 31] +
    data_combine[, 14] * data_combine[, 31] + data_combine[, 15] * data_combine[, 31] +
    data_combine[, 16] * data_combine[, 31] + data_combine[, 17] * data_combine[, 31] +
    data_combine[, 18] * data_combine[, 31] + data_combine[, 19] * data_combine[, 31] +
    data_combine[, 20] * data_combine[, 31] + data_combine[, 21] * data_combine[, 31] +
    data_combine[, 10] * data_combine[, 32] + data_combine[, 11] * data_combine[, 32] +
    data_combine[, 12] * data_combine[, 32] + data_combine[, 13] * data_combine[, 32] +
    data_combine[, 14] * data_combine[, 32] + data_combine[, 15] * data_combine[, 32] +
    data_combine[, 16] * data_combine[, 32] + data_combine[, 17] * data_combine[, 32] +
    data_combine[, 18] * data_combine[, 32] + data_combine[, 19] * data_combine[, 32] +
    data_combine[, 20] * data_combine[, 32] + data_combine[, 21] * data_combine[, 32] +
    data_combine[, 10] * data_combine[, 33] + data_combine[, 11] * data_combine[, 33] +
    data_combine[, 12] * data_combine[, 33] + data_combine[, 13] * data_combine[, 33] +
    data_combine[, 14] * data_combine[, 33] + data_combine[, 15] * data_combine[, 33] +
    data_combine[, 16] * data_combine[, 33] + data_combine[, 17] * data_combine[, 33] +
    data_combine[, 18] * data_combine[, 33] + data_combine[, 19] * data_combine[, 33] +
    data_combine[, 20] * data_combine[, 33] + data_combine[, 21] * data_combine[, 33] +
    data_combine[, 10] * data_combine[, 9] + data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 9] + data_combine[, 23] * data_combine[, 9] +
    data_combine[, 12] * data_combine[, 9] + data_combine[, 24] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 9] + data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 9] + data_combine[, 26] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 9] + data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 9] + data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 9] + data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 9] + data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 9] + data_combine[, 31] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 9] + data_combine[, 32] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 9] + data_combine[, 33] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 22] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 22] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 15] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 19] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 22] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 14] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 18] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 23] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 10] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 24] + data_combine[, 12] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 13] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 24] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 17] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 24] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 21] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 25] + data_combine[, 12] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 25] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 16] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 25] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 20] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 15] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 19] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 26] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 14] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 27] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 18] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 27] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 10] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 28] + data_combine[, 12] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 13] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 28] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 17] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 21] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 29] + data_combine[, 12] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 29] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 16] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 29] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 20] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 30] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 30] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 16] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 19] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 30] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 10] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 11] * data_combine[, 31] + data_combine[, 12] * data_combine[, 31] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 31] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 16] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 19] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 31] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 10] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 32] + data_combine[, 12] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 13] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 32] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 17] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 21] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 33] + data_combine[, 12] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 33] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 16] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 33] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 20] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 33] * data_combine[, 9],
  family = Gamma(link = "identity"),
  data = data_combine
)

fit_l_gamma = step(fit_length_gamma)
fit_l_gamma_up = glm(
  formula = data_combine[, 1] ~  data_combine[, 13] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 23] + data_combine[, 24]  + data_combine[, 28] + data_combine[, 29] + data_combine[, 30] + data_combine[, 13]:data_combine[, 22] +
    data_combine[, 16]:data_combine[, 25] + data_combine[, 15]:data_combine[, 28] + data_combine[, 18]:data_combine[, 28] +
    data_combine[, 10]:data_combine[, 29]  + data_combine[, 15]:data_combine[, 30] +
    data_combine[, 10]:data_combine[, 33]  +
    data_combine[, 9]:data_combine[, 15],
  family = Gamma(link = "identity"),
  data = data_combine
)
summary(fit_l_gamma_up)

##plot
xlabel = 1:length(data_combine[, 1])
pred = predict(fit_l_gamma_up, data_combine, type = 'response')
RMsel.Gamma.Id = sqrt(mean((pred - data_combine[, 1]) ^ 2))
pred = sort(pred)
predlgammaid = pred
plot(xlabel, pred)
plot(
  xlabel,
  pred,
  xlab = "",
  ylab = "",
  col = "red",
  type = "b",
  pch = 4
)
lines(xlabel,
      sort(data_combine[, 1]),
      col = "blue",
      type = "b")
legend(
  "topleft",
  inset = .05,
  c("prediction", "raw data"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue")
)

# save the result
saveModel("tidyL_GammaId.csv", model = fit_l_gamma_up)

# prediction for one year

data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
OneYearPred(fit_l_gamma_up, data_combine, "LGammaIdlinearOneYear.csv")
data_combine = data_medium


# Gamma function with log fun
fit_length_gamma_log = glm(
  data_combine[, 1] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13] + data_combine[, 14] + data_combine[, 15] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23] + data_combine[, 24] +
    data_combine[, 25] + data_combine[, 26] + data_combine[, 27] +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30] +
    data_combine[, 31] + data_combine[, 32] + data_combine[, 33] +
    data_combine[, 10] * data_combine[, 22] + data_combine[, 11] * data_combine[, 22] +
    data_combine[, 12] * data_combine[, 22] + data_combine[, 13] * data_combine[, 22] +
    data_combine[, 14] * data_combine[, 22] + data_combine[, 15] * data_combine[, 22] +
    data_combine[, 16] * data_combine[, 22] + data_combine[, 17] * data_combine[, 22] +
    data_combine[, 18] * data_combine[, 22] + data_combine[, 19] * data_combine[, 22] +
    data_combine[, 20] * data_combine[, 22] + data_combine[, 21] * data_combine[, 22] +
    data_combine[, 10] * data_combine[, 23] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] + data_combine[, 13] * data_combine[, 23] +
    data_combine[, 14] * data_combine[, 23] + data_combine[, 15] * data_combine[, 23] +
    data_combine[, 16] * data_combine[, 23] + data_combine[, 17] * data_combine[, 23] +
    data_combine[, 18] * data_combine[, 23] + data_combine[, 19] * data_combine[, 23] +
    data_combine[, 20] * data_combine[, 23] + data_combine[, 21] * data_combine[, 23] +
    data_combine[, 10] * data_combine[, 24] + data_combine[, 11] * data_combine[, 24] +
    data_combine[, 12] * data_combine[, 24] + data_combine[, 13] * data_combine[, 24] +
    data_combine[, 14] * data_combine[, 24] + data_combine[, 15] * data_combine[, 24] +
    data_combine[, 16] * data_combine[, 24] + data_combine[, 17] * data_combine[, 24] +
    data_combine[, 18] * data_combine[, 24] + data_combine[, 19] * data_combine[, 24] +
    data_combine[, 20] * data_combine[, 24] + data_combine[, 21] * data_combine[, 24] +
    data_combine[, 10] * data_combine[, 25] + data_combine[, 11] * data_combine[, 25] +
    data_combine[, 12] * data_combine[, 25] + data_combine[, 13] * data_combine[, 25] +
    data_combine[, 14] * data_combine[, 25] + data_combine[, 15] * data_combine[, 25] +
    data_combine[, 16] * data_combine[, 25] + data_combine[, 17] * data_combine[, 25] +
    data_combine[, 18] * data_combine[, 25] + data_combine[, 19] * data_combine[, 25] +
    data_combine[, 20] * data_combine[, 25] + data_combine[, 21] * data_combine[, 25] +
    data_combine[, 10] * data_combine[, 26] + data_combine[, 11] * data_combine[, 26] +
    data_combine[, 12] * data_combine[, 26] + data_combine[, 13] * data_combine[, 26] +
    data_combine[, 14] * data_combine[, 26] + data_combine[, 15] * data_combine[, 26] +
    data_combine[, 16] * data_combine[, 26] + data_combine[, 17] * data_combine[, 26] +
    data_combine[, 18] * data_combine[, 26] + data_combine[, 19] * data_combine[, 26] +
    data_combine[, 20] * data_combine[, 26] + data_combine[, 21] * data_combine[, 26] +
    data_combine[, 10] * data_combine[, 27] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] + data_combine[, 13] * data_combine[, 27] +
    data_combine[, 14] * data_combine[, 27] + data_combine[, 15] * data_combine[, 27] +
    data_combine[, 16] * data_combine[, 27] + data_combine[, 17] * data_combine[, 27] +
    data_combine[, 18] * data_combine[, 27] + data_combine[, 19] * data_combine[, 27] +
    data_combine[, 20] * data_combine[, 27] + data_combine[, 21] * data_combine[, 27] +
    data_combine[, 10] * data_combine[, 28] + data_combine[, 11] * data_combine[, 28] +
    data_combine[, 12] * data_combine[, 28] + data_combine[, 13] * data_combine[, 28] +
    data_combine[, 14] * data_combine[, 28] + data_combine[, 15] * data_combine[, 28] +
    data_combine[, 16] * data_combine[, 28] + data_combine[, 17] * data_combine[, 28] +
    data_combine[, 18] * data_combine[, 28] + data_combine[, 19] * data_combine[, 28] +
    data_combine[, 20] * data_combine[, 28] + data_combine[, 21] * data_combine[, 28] +
    data_combine[, 10] * data_combine[, 29] + data_combine[, 11] * data_combine[, 29] +
    data_combine[, 12] * data_combine[, 29] + data_combine[, 13] * data_combine[, 29] +
    data_combine[, 14] * data_combine[, 29] + data_combine[, 15] * data_combine[, 29] +
    data_combine[, 16] * data_combine[, 29] + data_combine[, 17] * data_combine[, 29] +
    data_combine[, 18] * data_combine[, 29] + data_combine[, 19] * data_combine[, 29] +
    data_combine[, 20] * data_combine[, 29] + data_combine[, 21] * data_combine[, 29] +
    data_combine[, 10] * data_combine[, 30] + data_combine[, 11] * data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] + data_combine[, 13] * data_combine[, 30] +
    data_combine[, 14] * data_combine[, 30] + data_combine[, 15] * data_combine[, 30] +
    data_combine[, 16] * data_combine[, 30] + data_combine[, 17] * data_combine[, 30] +
    data_combine[, 18] * data_combine[, 30] + data_combine[, 19] * data_combine[, 30] +
    data_combine[, 20] * data_combine[, 30] + data_combine[, 21] * data_combine[, 30] +
    data_combine[, 10] * data_combine[, 31] + data_combine[, 11] * data_combine[, 31] +
    data_combine[, 12] * data_combine[, 31] + data_combine[, 13] * data_combine[, 31] +
    data_combine[, 14] * data_combine[, 31] + data_combine[, 15] * data_combine[, 31] +
    data_combine[, 16] * data_combine[, 31] + data_combine[, 17] * data_combine[, 31] +
    data_combine[, 18] * data_combine[, 31] + data_combine[, 19] * data_combine[, 31] +
    data_combine[, 20] * data_combine[, 31] + data_combine[, 21] * data_combine[, 31] +
    data_combine[, 10] * data_combine[, 32] + data_combine[, 11] * data_combine[, 32] +
    data_combine[, 12] * data_combine[, 32] + data_combine[, 13] * data_combine[, 32] +
    data_combine[, 14] * data_combine[, 32] + data_combine[, 15] * data_combine[, 32] +
    data_combine[, 16] * data_combine[, 32] + data_combine[, 17] * data_combine[, 32] +
    data_combine[, 18] * data_combine[, 32] + data_combine[, 19] * data_combine[, 32] +
    data_combine[, 20] * data_combine[, 32] + data_combine[, 21] * data_combine[, 32] +
    data_combine[, 10] * data_combine[, 33] + data_combine[, 11] * data_combine[, 33] +
    data_combine[, 12] * data_combine[, 33] + data_combine[, 13] * data_combine[, 33] +
    data_combine[, 14] * data_combine[, 33] + data_combine[, 15] * data_combine[, 33] +
    data_combine[, 16] * data_combine[, 33] + data_combine[, 17] * data_combine[, 33] +
    data_combine[, 18] * data_combine[, 33] + data_combine[, 19] * data_combine[, 33] +
    data_combine[, 20] * data_combine[, 33] + data_combine[, 21] * data_combine[, 33] +
    data_combine[, 10] * data_combine[, 9] + data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 9] + data_combine[, 23] * data_combine[, 9] +
    data_combine[, 12] * data_combine[, 9] + data_combine[, 24] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 9] + data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 9] + data_combine[, 26] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 9] + data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 9] + data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 9] + data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 9] + data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 9] + data_combine[, 31] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 9] + data_combine[, 32] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 9] + data_combine[, 33] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 22] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 22] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 15] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 19] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 22] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 14] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 18] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 23] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 10] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 24] + data_combine[, 12] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 13] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 24] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 17] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 24] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 21] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 25] + data_combine[, 12] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 25] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 16] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 25] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 20] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 15] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 19] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 26] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 14] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 27] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 18] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 27] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 10] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 28] + data_combine[, 12] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 13] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 28] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 17] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 21] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 29] + data_combine[, 12] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 29] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 16] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 29] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 20] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 30] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 30] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 16] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 19] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 30] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 10] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 11] * data_combine[, 31] + data_combine[, 12] * data_combine[, 31] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 31] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 16] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 19] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 31] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 10] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 32] + data_combine[, 12] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 13] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 32] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 17] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 21] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 33] + data_combine[, 12] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 33] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 16] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 33] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 20] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 33] * data_combine[, 9],
  family = Gamma(link = "log"),
  data = data_combine
)

fit_l_gamma_log = step(fit_length_gamma_log)
fit_l_gamma_log_up = glm(
  formula = data_combine[, 1] ~ data_combine[, 9] + data_combine[, 10] +
    data_combine[, 12] + data_combine[, 13] + data_combine[, 14] +
    data_combine[, 15] + data_combine[, 16] + data_combine[, 17] +
    data_combine[, 18] + data_combine[, 22]  +
    data_combine[, 24] + data_combine[, 25]  +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30]
  + data_combine[, 33] + data_combine[, 13]:data_combine[, 22] +
    data_combine[, 14]:data_combine[, 25] + data_combine[, 15]:data_combine[, 25] +
    data_combine[, 18]:data_combine[, 25] + data_combine[, 15]:data_combine[, 28] +
    data_combine[, 18]:data_combine[, 28] + data_combine[, 10]:data_combine[, 29] +
    data_combine[, 16]:data_combine[, 29] + data_combine[, 15]:data_combine[, 30] +
    data_combine[, 10]:data_combine[, 33] + data_combine[, 9]:data_combine[, 25] +
    data_combine[, 9]:data_combine[, 14] + data_combine[, 9]:data_combine[, 15],
  family = Gamma(link = "log"),
  data = data_combine
  
)
summary(fit_l_gamma_log_up)

##plot
xlabel = 1:length(data_combine[, 1])
pred = predict(fit_l_gamma_log_up, data_combine, type = 'response')
RMsel.Gamma.log = sqrt(mean((pred - data_combine[, 1]) ^ 2))
pred = sort(pred)
predlgammalog = pred
plot(xlabel, pred)
plot(
  xlabel,
  pred,
  xlab = "",
  ylab = "",
  col = "red",
  type = "b",
  pch = 4
)
lines(xlabel,
      sort(data_combine[, 1]),
      col = "blue",
      type = "b")
legend(
  "topleft",
  inset = .05,
  c("prediction", "raw data"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue")
)

# save the result
saveModel("tidyL_GammaLog.csv", model = fit_l_gamma_log_up)

# prediction for one year

data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
OneYearPred(fit_l_gamma_log_up,
            data_combine,
            "LGammaLoglinearOneYear.csv")
data_combine = data_medium


## comparison for length
xlabel = 1:length(data_combine[, 1])

plot(xlabel, predlgammaid)
plot(
  xlabel,
  predlgammaid,
  xlab = "",
  ylab = "",
  col = "red",
  type = "b",
  pch = 4
)
lines(xlabel,
      sort(data_combine[, 1]),
      col = "blue",
      type = "b")

lines(xlabel,
      predlgammalog,
      col = "green",
      type = "b")

legend(
  "topleft",
  inset = .05,
  c("Gamma(Id)","raw data" ,"Gamma(log)"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue","green")
)


























### MODEL FOR WIDTH
# Linear with other term
fit_width_linear = lm(
  data_combine[, 2] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13] + data_combine[, 14] + data_combine[, 15] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23] + data_combine[, 24] +
    data_combine[, 25] + data_combine[, 26] + data_combine[, 27] +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30] +
    data_combine[, 31] + data_combine[, 32] + data_combine[, 33] +
    data_combine[, 10] * data_combine[, 22] + data_combine[, 11] * data_combine[, 22] +
    data_combine[, 12] * data_combine[, 22] + data_combine[, 13] * data_combine[, 22] +
    data_combine[, 14] * data_combine[, 22] + data_combine[, 15] * data_combine[, 22] +
    data_combine[, 16] * data_combine[, 22] + data_combine[, 17] * data_combine[, 22] +
    data_combine[, 18] * data_combine[, 22] + data_combine[, 19] * data_combine[, 22] +
    data_combine[, 20] * data_combine[, 22] + data_combine[, 21] * data_combine[, 22] +
    data_combine[, 10] * data_combine[, 23] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] + data_combine[, 13] * data_combine[, 23] +
    data_combine[, 14] * data_combine[, 23] + data_combine[, 15] * data_combine[, 23] +
    data_combine[, 16] * data_combine[, 23] + data_combine[, 17] * data_combine[, 23] +
    data_combine[, 18] * data_combine[, 23] + data_combine[, 19] * data_combine[, 23] +
    data_combine[, 20] * data_combine[, 23] + data_combine[, 21] * data_combine[, 23] +
    data_combine[, 10] * data_combine[, 24] + data_combine[, 11] * data_combine[, 24] +
    data_combine[, 12] * data_combine[, 24] + data_combine[, 13] * data_combine[, 24] +
    data_combine[, 14] * data_combine[, 24] + data_combine[, 15] * data_combine[, 24] +
    data_combine[, 16] * data_combine[, 24] + data_combine[, 17] * data_combine[, 24] +
    data_combine[, 18] * data_combine[, 24] + data_combine[, 19] * data_combine[, 24] +
    data_combine[, 20] * data_combine[, 24] + data_combine[, 21] * data_combine[, 24] +
    data_combine[, 10] * data_combine[, 25] + data_combine[, 11] * data_combine[, 25] +
    data_combine[, 12] * data_combine[, 25] + data_combine[, 13] * data_combine[, 25] +
    data_combine[, 14] * data_combine[, 25] + data_combine[, 15] * data_combine[, 25] +
    data_combine[, 16] * data_combine[, 25] + data_combine[, 17] * data_combine[, 25] +
    data_combine[, 18] * data_combine[, 25] + data_combine[, 19] * data_combine[, 25] +
    data_combine[, 20] * data_combine[, 25] + data_combine[, 21] * data_combine[, 25] +
    data_combine[, 10] * data_combine[, 26] + data_combine[, 11] * data_combine[, 26] +
    data_combine[, 12] * data_combine[, 26] + data_combine[, 13] * data_combine[, 26] +
    data_combine[, 14] * data_combine[, 26] + data_combine[, 15] * data_combine[, 26] +
    data_combine[, 16] * data_combine[, 26] + data_combine[, 17] * data_combine[, 26] +
    data_combine[, 18] * data_combine[, 26] + data_combine[, 19] * data_combine[, 26] +
    data_combine[, 20] * data_combine[, 26] + data_combine[, 21] * data_combine[, 26] +
    data_combine[, 10] * data_combine[, 27] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] + data_combine[, 13] * data_combine[, 27] +
    data_combine[, 14] * data_combine[, 27] + data_combine[, 15] * data_combine[, 27] +
    data_combine[, 16] * data_combine[, 27] + data_combine[, 17] * data_combine[, 27] +
    data_combine[, 18] * data_combine[, 27] + data_combine[, 19] * data_combine[, 27] +
    data_combine[, 20] * data_combine[, 27] + data_combine[, 21] * data_combine[, 27] +
    data_combine[, 10] * data_combine[, 28] + data_combine[, 11] * data_combine[, 28] +
    data_combine[, 12] * data_combine[, 28] + data_combine[, 13] * data_combine[, 28] +
    data_combine[, 14] * data_combine[, 28] + data_combine[, 15] * data_combine[, 28] +
    data_combine[, 16] * data_combine[, 28] + data_combine[, 17] * data_combine[, 28] +
    data_combine[, 18] * data_combine[, 28] + data_combine[, 19] * data_combine[, 28] +
    data_combine[, 20] * data_combine[, 28] + data_combine[, 21] * data_combine[, 28] +
    data_combine[, 10] * data_combine[, 29] + data_combine[, 11] * data_combine[, 29] +
    data_combine[, 12] * data_combine[, 29] + data_combine[, 13] * data_combine[, 29] +
    data_combine[, 14] * data_combine[, 29] + data_combine[, 15] * data_combine[, 29] +
    data_combine[, 16] * data_combine[, 29] + data_combine[, 17] * data_combine[, 29] +
    data_combine[, 18] * data_combine[, 29] + data_combine[, 19] * data_combine[, 29] +
    data_combine[, 20] * data_combine[, 29] + data_combine[, 21] * data_combine[, 29] +
    data_combine[, 10] * data_combine[, 30] + data_combine[, 11] * data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] + data_combine[, 13] * data_combine[, 30] +
    data_combine[, 14] * data_combine[, 30] + data_combine[, 15] * data_combine[, 30] +
    data_combine[, 16] * data_combine[, 30] + data_combine[, 17] * data_combine[, 30] +
    data_combine[, 18] * data_combine[, 30] + data_combine[, 19] * data_combine[, 30] +
    data_combine[, 20] * data_combine[, 30] + data_combine[, 21] * data_combine[, 30] +
    data_combine[, 10] * data_combine[, 31] + data_combine[, 11] * data_combine[, 31] +
    data_combine[, 12] * data_combine[, 31] + data_combine[, 13] * data_combine[, 31] +
    data_combine[, 14] * data_combine[, 31] + data_combine[, 15] * data_combine[, 31] +
    data_combine[, 16] * data_combine[, 31] + data_combine[, 17] * data_combine[, 31] +
    data_combine[, 18] * data_combine[, 31] + data_combine[, 19] * data_combine[, 31] +
    data_combine[, 20] * data_combine[, 31] + data_combine[, 21] * data_combine[, 31] +
    data_combine[, 10] * data_combine[, 32] + data_combine[, 11] * data_combine[, 32] +
    data_combine[, 12] * data_combine[, 32] + data_combine[, 13] * data_combine[, 32] +
    data_combine[, 14] * data_combine[, 32] + data_combine[, 15] * data_combine[, 32] +
    data_combine[, 16] * data_combine[, 32] + data_combine[, 17] * data_combine[, 32] +
    data_combine[, 18] * data_combine[, 32] + data_combine[, 19] * data_combine[, 32] +
    data_combine[, 20] * data_combine[, 32] + data_combine[, 21] * data_combine[, 32] +
    data_combine[, 10] * data_combine[, 33] + data_combine[, 11] * data_combine[, 33] +
    data_combine[, 12] * data_combine[, 33] + data_combine[, 13] * data_combine[, 33] +
    data_combine[, 14] * data_combine[, 33] + data_combine[, 15] * data_combine[, 33] +
    data_combine[, 16] * data_combine[, 33] + data_combine[, 17] * data_combine[, 33] +
    data_combine[, 18] * data_combine[, 33] + data_combine[, 19] * data_combine[, 33] +
    data_combine[, 20] * data_combine[, 33] + data_combine[, 21] * data_combine[, 33] +
    data_combine[, 10] * data_combine[, 9] + data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 9] + data_combine[, 23] * data_combine[, 9] +
    data_combine[, 12] * data_combine[, 9] + data_combine[, 24] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 9] + data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 9] + data_combine[, 26] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 9] + data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 9] + data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 9] + data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 9] + data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 9] + data_combine[, 31] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 9] + data_combine[, 32] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 9] + data_combine[, 33] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 22] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 22] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 15] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 19] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 22] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 14] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 18] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 23] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 10] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 24] + data_combine[, 12] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 13] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 24] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 17] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 24] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 21] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 25] + data_combine[, 12] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 25] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 16] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 25] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 20] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 15] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 19] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 26] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 14] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 27] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 18] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 27] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 10] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 28] + data_combine[, 12] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 13] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 28] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 17] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 21] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 29] + data_combine[, 12] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 29] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 16] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 29] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 20] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 30] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 30] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 16] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 19] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 30] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 10] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 11] * data_combine[, 31] + data_combine[, 12] * data_combine[, 31] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 31] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 16] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 19] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 31] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 10] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 32] + data_combine[, 12] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 13] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 32] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 17] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 21] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 33] + data_combine[, 12] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 33] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 16] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 33] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 20] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 33] * data_combine[, 9]
  ,
  data = data_combine
)
## plot
fit_w_linear = step(fit_width_linear)
fit_w_linear_up = lm(
  formula = data_combine[, 2] ~ 
    data_combine[, 16] + data_combine[, 18]  + 
    data_combine[, 23] + data_combine[, 24]  + data_combine[, 28] + 
    
    data_combine[, 16]:data_combine[, 25] + 

    data_combine[, 15]:data_combine[, 28] + data_combine[, 18]:data_combine[, 28] + 
    
     data_combine[, 10]:data_combine[, 33] + 
    data_combine[, 16]:data_combine[, 33]  + 
     data_combine[, 9]:data_combine[, 15] + 
    data_combine[, 9]:data_combine[, 29]  + 
    data_combine[, 9]:data_combine[, 15]:data_combine[, 29],
  data = data_combine
)


summary(fit_w_linear_up)
# plot
RMSEW.Linear = drawScatter(fit_w_linear_up, 2)

## save the result
saveModel("tidywidtylinear.csv", model = fit_w_linear_up)

# predicting for one year
data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
OneYearPred(fit_w_linear_up, data_combine, "WlinearOneYear.csv")
data_combine = data_medium



# Width contains only length
estimated_length = predict(fit_l_gamma_up, data_combine, type = 'response')
data_combine[, 1] = replace(data_combine[, 1], values = estimated_length)
fit_width_l_linear = lm(data_combine[, 2] ~   data_combine[, 1],
                        data = data_combine)

## plot
summary(fit_width_l_linear)
# plot
RMSEW.Linear.length = drawScatter(fit_width_l_linear, 2)

## save the result
saveModel("tidywidtylinear_L.csv", model = fit_width_l_linear)

# predicting for one year
data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
#OneYearPred(fit_width_l_linear, data_combine, "W_LlinearOneYear.csv")
#estimated_length_oneYear = predict(fit_l_gamma_up,data_combine,type = "response")
pred_w_oneyear = predict(fit_width_l_linear, data_combine, type = "response")
write.csv(pred_w_oneyear,
          file = "wlinearLOneYear.csv",
          quote = F,
          row.names = F)
data_combine = data_medium


# Gamma Model
## width Gamma Regression Identity link
fit_width_gamma_Id = glm(
  data_combine[, 2] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13] + data_combine[, 14] + data_combine[, 15] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23] + data_combine[, 24] +
    data_combine[, 25] + data_combine[, 26] + data_combine[, 27] +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30] +
    data_combine[, 31] + data_combine[, 32] + data_combine[, 33] +
    data_combine[, 10] * data_combine[, 22] + data_combine[, 11] * data_combine[, 22] +
    data_combine[, 12] * data_combine[, 22] + data_combine[, 13] * data_combine[, 22] +
    data_combine[, 14] * data_combine[, 22] + data_combine[, 15] * data_combine[, 22] +
    data_combine[, 16] * data_combine[, 22] + data_combine[, 17] * data_combine[, 22] +
    data_combine[, 18] * data_combine[, 22] + data_combine[, 19] * data_combine[, 22] +
    data_combine[, 20] * data_combine[, 22] + data_combine[, 21] * data_combine[, 22] +
    data_combine[, 10] * data_combine[, 23] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] + data_combine[, 13] * data_combine[, 23] +
    data_combine[, 14] * data_combine[, 23] + data_combine[, 15] * data_combine[, 23] +
    data_combine[, 16] * data_combine[, 23] + data_combine[, 17] * data_combine[, 23] +
    data_combine[, 18] * data_combine[, 23] + data_combine[, 19] * data_combine[, 23] +
    data_combine[, 20] * data_combine[, 23] + data_combine[, 21] * data_combine[, 23] +
    data_combine[, 10] * data_combine[, 24] + data_combine[, 11] * data_combine[, 24] +
    data_combine[, 12] * data_combine[, 24] + data_combine[, 13] * data_combine[, 24] +
    data_combine[, 14] * data_combine[, 24] + data_combine[, 15] * data_combine[, 24] +
    data_combine[, 16] * data_combine[, 24] + data_combine[, 17] * data_combine[, 24] +
    data_combine[, 18] * data_combine[, 24] + data_combine[, 19] * data_combine[, 24] +
    data_combine[, 20] * data_combine[, 24] + data_combine[, 21] * data_combine[, 24] +
    data_combine[, 10] * data_combine[, 25] + data_combine[, 11] * data_combine[, 25] +
    data_combine[, 12] * data_combine[, 25] + data_combine[, 13] * data_combine[, 25] +
    data_combine[, 14] * data_combine[, 25] + data_combine[, 15] * data_combine[, 25] +
    data_combine[, 16] * data_combine[, 25] + data_combine[, 17] * data_combine[, 25] +
    data_combine[, 18] * data_combine[, 25] + data_combine[, 19] * data_combine[, 25] +
    data_combine[, 20] * data_combine[, 25] + data_combine[, 21] * data_combine[, 25] +
    data_combine[, 10] * data_combine[, 26] + data_combine[, 11] * data_combine[, 26] +
    data_combine[, 12] * data_combine[, 26] + data_combine[, 13] * data_combine[, 26] +
    data_combine[, 14] * data_combine[, 26] + data_combine[, 15] * data_combine[, 26] +
    data_combine[, 16] * data_combine[, 26] + data_combine[, 17] * data_combine[, 26] +
    data_combine[, 18] * data_combine[, 26] + data_combine[, 19] * data_combine[, 26] +
    data_combine[, 20] * data_combine[, 26] + data_combine[, 21] * data_combine[, 26] +
    data_combine[, 10] * data_combine[, 27] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] + data_combine[, 13] * data_combine[, 27] +
    data_combine[, 14] * data_combine[, 27] + data_combine[, 15] * data_combine[, 27] +
    data_combine[, 16] * data_combine[, 27] + data_combine[, 17] * data_combine[, 27] +
    data_combine[, 18] * data_combine[, 27] + data_combine[, 19] * data_combine[, 27] +
    data_combine[, 20] * data_combine[, 27] + data_combine[, 21] * data_combine[, 27] +
    data_combine[, 10] * data_combine[, 28] + data_combine[, 11] * data_combine[, 28] +
    data_combine[, 12] * data_combine[, 28] + data_combine[, 13] * data_combine[, 28] +
    data_combine[, 14] * data_combine[, 28] + data_combine[, 15] * data_combine[, 28] +
    data_combine[, 16] * data_combine[, 28] + data_combine[, 17] * data_combine[, 28] +
    data_combine[, 18] * data_combine[, 28] + data_combine[, 19] * data_combine[, 28] +
    data_combine[, 20] * data_combine[, 28] + data_combine[, 21] * data_combine[, 28] +
    data_combine[, 10] * data_combine[, 29] + data_combine[, 11] * data_combine[, 29] +
    data_combine[, 12] * data_combine[, 29] + data_combine[, 13] * data_combine[, 29] +
    data_combine[, 14] * data_combine[, 29] + data_combine[, 15] * data_combine[, 29] +
    data_combine[, 16] * data_combine[, 29] + data_combine[, 17] * data_combine[, 29] +
    data_combine[, 18] * data_combine[, 29] + data_combine[, 19] * data_combine[, 29] +
    data_combine[, 20] * data_combine[, 29] + data_combine[, 21] * data_combine[, 29] +
    data_combine[, 10] * data_combine[, 30] + data_combine[, 11] * data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] + data_combine[, 13] * data_combine[, 30] +
    data_combine[, 14] * data_combine[, 30] + data_combine[, 15] * data_combine[, 30] +
    data_combine[, 16] * data_combine[, 30] + data_combine[, 17] * data_combine[, 30] +
    data_combine[, 18] * data_combine[, 30] + data_combine[, 19] * data_combine[, 30] +
    data_combine[, 20] * data_combine[, 30] + data_combine[, 21] * data_combine[, 30] +
    data_combine[, 10] * data_combine[, 31] + data_combine[, 11] * data_combine[, 31] +
    data_combine[, 12] * data_combine[, 31] + data_combine[, 13] * data_combine[, 31] +
    data_combine[, 14] * data_combine[, 31] + data_combine[, 15] * data_combine[, 31] +
    data_combine[, 16] * data_combine[, 31] + data_combine[, 17] * data_combine[, 31] +
    data_combine[, 18] * data_combine[, 31] + data_combine[, 19] * data_combine[, 31] +
    data_combine[, 20] * data_combine[, 31] + data_combine[, 21] * data_combine[, 31] +
    data_combine[, 10] * data_combine[, 32] + data_combine[, 11] * data_combine[, 32] +
    data_combine[, 12] * data_combine[, 32] + data_combine[, 13] * data_combine[, 32] +
    data_combine[, 14] * data_combine[, 32] + data_combine[, 15] * data_combine[, 32] +
    data_combine[, 16] * data_combine[, 32] + data_combine[, 17] * data_combine[, 32] +
    data_combine[, 18] * data_combine[, 32] + data_combine[, 19] * data_combine[, 32] +
    data_combine[, 20] * data_combine[, 32] + data_combine[, 21] * data_combine[, 32] +
    data_combine[, 10] * data_combine[, 33] + data_combine[, 11] * data_combine[, 33] +
    data_combine[, 12] * data_combine[, 33] + data_combine[, 13] * data_combine[, 33] +
    data_combine[, 14] * data_combine[, 33] + data_combine[, 15] * data_combine[, 33] +
    data_combine[, 16] * data_combine[, 33] + data_combine[, 17] * data_combine[, 33] +
    data_combine[, 18] * data_combine[, 33] + data_combine[, 19] * data_combine[, 33] +
    data_combine[, 20] * data_combine[, 33] + data_combine[, 21] * data_combine[, 33] +
    data_combine[, 10] * data_combine[, 9] + data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 9] + data_combine[, 23] * data_combine[, 9] +
    data_combine[, 12] * data_combine[, 9] + data_combine[, 24] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 9] + data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 9] + data_combine[, 26] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 9] + data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 9] + data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 9] + data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 9] + data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 9] + data_combine[, 31] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 9] + data_combine[, 32] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 9] + data_combine[, 33] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 22] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 22] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 15] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 19] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 22] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 14] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 18] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 23] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 10] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 24] + data_combine[, 12] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 13] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 24] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 17] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 24] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 21] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 25] + data_combine[, 12] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 25] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 16] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 25] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 20] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 15] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 19] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 26] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 14] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 27] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 18] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 27] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 10] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 28] + data_combine[, 12] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 13] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 28] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 17] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 21] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 29] + data_combine[, 12] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 29] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 16] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 29] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 20] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 30] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 30] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 16] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 19] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 30] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 10] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 11] * data_combine[, 31] + data_combine[, 12] * data_combine[, 31] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 31] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 16] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 19] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 31] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 10] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 32] + data_combine[, 12] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 13] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 32] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 17] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 21] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 33] + data_combine[, 12] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 33] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 16] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 33] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 20] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 33] * data_combine[, 9],
  family = Gamma(link = "identity"),
  data = data_combine
)

fit_w_gamma_Id = step(fit_width_gamma_Id)
fit_w_gamma_Id_up = glm(formula = data_combine[, 2] ~  
                           data_combine[,14] + data_combine[, 15] + data_combine[, 16] + 
                           data_combine[,18]  + data_combine[, 23] + 
                           data_combine[, 25] +data_combine[, 28] + data_combine[, 29] + 
                           data_combine[, 10]:data_combine[, 25] + 
                           data_combine[,16]:data_combine[, 25] + data_combine[, 19]:data_combine[, 25] + 
                            data_combine[,15]:data_combine[, 28] + 
                           data_combine[, 18]:data_combine[, 28] + data_combine[, 15]:data_combine[, 29] + 
                           data_combine[,10]:data_combine[, 30] + 
                           data_combine[, 10]:data_combine[, 33] + data_combine[,16]:data_combine[, 33] + 
                           data_combine[, 9]:data_combine[, 25] + data_combine[, 9]:data_combine[, 14] + 
                           data_combine[, 9]:data_combine[, 15] + data_combine[, 9]:data_combine[, 29] + 
                           
                           data_combine[, 9]:data_combine[, 15]:data_combine[, 29], family = Gamma(link = "identity"), 
                             data = data_combine)
summary(fit_w_gamma_Id_up)

##plot
RMSEWId.Gamma = drawScatter(fit_w_gamma_Id_up, 2)

# save the result
saveModel("tidywidthGammaId.csv", model = fit_w_gamma_Id_up)

# prediction for one year

data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
OneYearPred(fit_w_gamma_Id_up, data_combine, "WGammaIdOneYear.csv")
data_combine = data_medium












## width Gamma Regression log link
fit_width_gamma_log = glm(
  data_combine[, 2] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13] + data_combine[, 14] + data_combine[, 15] +
    data_combine[, 16] + data_combine[, 17] + data_combine[, 18]  +
    data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23] + data_combine[, 24] +
    data_combine[, 25] + data_combine[, 26] + data_combine[, 27] +
    data_combine[, 28] + data_combine[, 29] + data_combine[, 30] +
    data_combine[, 31] + data_combine[, 32] + data_combine[, 33] +
    data_combine[, 10] * data_combine[, 22] + data_combine[, 11] * data_combine[, 22] +
    data_combine[, 12] * data_combine[, 22] + data_combine[, 13] * data_combine[, 22] +
    data_combine[, 14] * data_combine[, 22] + data_combine[, 15] * data_combine[, 22] +
    data_combine[, 16] * data_combine[, 22] + data_combine[, 17] * data_combine[, 22] +
    data_combine[, 18] * data_combine[, 22] + data_combine[, 19] * data_combine[, 22] +
    data_combine[, 20] * data_combine[, 22] + data_combine[, 21] * data_combine[, 22] +
    data_combine[, 10] * data_combine[, 23] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] + data_combine[, 13] * data_combine[, 23] +
    data_combine[, 14] * data_combine[, 23] + data_combine[, 15] * data_combine[, 23] +
    data_combine[, 16] * data_combine[, 23] + data_combine[, 17] * data_combine[, 23] +
    data_combine[, 18] * data_combine[, 23] + data_combine[, 19] * data_combine[, 23] +
    data_combine[, 20] * data_combine[, 23] + data_combine[, 21] * data_combine[, 23] +
    data_combine[, 10] * data_combine[, 24] + data_combine[, 11] * data_combine[, 24] +
    data_combine[, 12] * data_combine[, 24] + data_combine[, 13] * data_combine[, 24] +
    data_combine[, 14] * data_combine[, 24] + data_combine[, 15] * data_combine[, 24] +
    data_combine[, 16] * data_combine[, 24] + data_combine[, 17] * data_combine[, 24] +
    data_combine[, 18] * data_combine[, 24] + data_combine[, 19] * data_combine[, 24] +
    data_combine[, 20] * data_combine[, 24] + data_combine[, 21] * data_combine[, 24] +
    data_combine[, 10] * data_combine[, 25] + data_combine[, 11] * data_combine[, 25] +
    data_combine[, 12] * data_combine[, 25] + data_combine[, 13] * data_combine[, 25] +
    data_combine[, 14] * data_combine[, 25] + data_combine[, 15] * data_combine[, 25] +
    data_combine[, 16] * data_combine[, 25] + data_combine[, 17] * data_combine[, 25] +
    data_combine[, 18] * data_combine[, 25] + data_combine[, 19] * data_combine[, 25] +
    data_combine[, 20] * data_combine[, 25] + data_combine[, 21] * data_combine[, 25] +
    data_combine[, 10] * data_combine[, 26] + data_combine[, 11] * data_combine[, 26] +
    data_combine[, 12] * data_combine[, 26] + data_combine[, 13] * data_combine[, 26] +
    data_combine[, 14] * data_combine[, 26] + data_combine[, 15] * data_combine[, 26] +
    data_combine[, 16] * data_combine[, 26] + data_combine[, 17] * data_combine[, 26] +
    data_combine[, 18] * data_combine[, 26] + data_combine[, 19] * data_combine[, 26] +
    data_combine[, 20] * data_combine[, 26] + data_combine[, 21] * data_combine[, 26] +
    data_combine[, 10] * data_combine[, 27] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] + data_combine[, 13] * data_combine[, 27] +
    data_combine[, 14] * data_combine[, 27] + data_combine[, 15] * data_combine[, 27] +
    data_combine[, 16] * data_combine[, 27] + data_combine[, 17] * data_combine[, 27] +
    data_combine[, 18] * data_combine[, 27] + data_combine[, 19] * data_combine[, 27] +
    data_combine[, 20] * data_combine[, 27] + data_combine[, 21] * data_combine[, 27] +
    data_combine[, 10] * data_combine[, 28] + data_combine[, 11] * data_combine[, 28] +
    data_combine[, 12] * data_combine[, 28] + data_combine[, 13] * data_combine[, 28] +
    data_combine[, 14] * data_combine[, 28] + data_combine[, 15] * data_combine[, 28] +
    data_combine[, 16] * data_combine[, 28] + data_combine[, 17] * data_combine[, 28] +
    data_combine[, 18] * data_combine[, 28] + data_combine[, 19] * data_combine[, 28] +
    data_combine[, 20] * data_combine[, 28] + data_combine[, 21] * data_combine[, 28] +
    data_combine[, 10] * data_combine[, 29] + data_combine[, 11] * data_combine[, 29] +
    data_combine[, 12] * data_combine[, 29] + data_combine[, 13] * data_combine[, 29] +
    data_combine[, 14] * data_combine[, 29] + data_combine[, 15] * data_combine[, 29] +
    data_combine[, 16] * data_combine[, 29] + data_combine[, 17] * data_combine[, 29] +
    data_combine[, 18] * data_combine[, 29] + data_combine[, 19] * data_combine[, 29] +
    data_combine[, 20] * data_combine[, 29] + data_combine[, 21] * data_combine[, 29] +
    data_combine[, 10] * data_combine[, 30] + data_combine[, 11] * data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] + data_combine[, 13] * data_combine[, 30] +
    data_combine[, 14] * data_combine[, 30] + data_combine[, 15] * data_combine[, 30] +
    data_combine[, 16] * data_combine[, 30] + data_combine[, 17] * data_combine[, 30] +
    data_combine[, 18] * data_combine[, 30] + data_combine[, 19] * data_combine[, 30] +
    data_combine[, 20] * data_combine[, 30] + data_combine[, 21] * data_combine[, 30] +
    data_combine[, 10] * data_combine[, 31] + data_combine[, 11] * data_combine[, 31] +
    data_combine[, 12] * data_combine[, 31] + data_combine[, 13] * data_combine[, 31] +
    data_combine[, 14] * data_combine[, 31] + data_combine[, 15] * data_combine[, 31] +
    data_combine[, 16] * data_combine[, 31] + data_combine[, 17] * data_combine[, 31] +
    data_combine[, 18] * data_combine[, 31] + data_combine[, 19] * data_combine[, 31] +
    data_combine[, 20] * data_combine[, 31] + data_combine[, 21] * data_combine[, 31] +
    data_combine[, 10] * data_combine[, 32] + data_combine[, 11] * data_combine[, 32] +
    data_combine[, 12] * data_combine[, 32] + data_combine[, 13] * data_combine[, 32] +
    data_combine[, 14] * data_combine[, 32] + data_combine[, 15] * data_combine[, 32] +
    data_combine[, 16] * data_combine[, 32] + data_combine[, 17] * data_combine[, 32] +
    data_combine[, 18] * data_combine[, 32] + data_combine[, 19] * data_combine[, 32] +
    data_combine[, 20] * data_combine[, 32] + data_combine[, 21] * data_combine[, 32] +
    data_combine[, 10] * data_combine[, 33] + data_combine[, 11] * data_combine[, 33] +
    data_combine[, 12] * data_combine[, 33] + data_combine[, 13] * data_combine[, 33] +
    data_combine[, 14] * data_combine[, 33] + data_combine[, 15] * data_combine[, 33] +
    data_combine[, 16] * data_combine[, 33] + data_combine[, 17] * data_combine[, 33] +
    data_combine[, 18] * data_combine[, 33] + data_combine[, 19] * data_combine[, 33] +
    data_combine[, 20] * data_combine[, 33] + data_combine[, 21] * data_combine[, 33] +
    data_combine[, 10] * data_combine[, 9] + data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 9] + data_combine[, 23] * data_combine[, 9] +
    data_combine[, 12] * data_combine[, 9] + data_combine[, 24] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 9] + data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 9] + data_combine[, 26] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 9] + data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 9] + data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 9] + data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 9] + data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 9] + data_combine[, 31] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 9] + data_combine[, 32] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 9] + data_combine[, 33] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 22] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 22] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 15] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9] + data_combine[, 19] * data_combine[, 22] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 22] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 11] * data_combine[, 23] +
    data_combine[, 12] * data_combine[, 23] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 14] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 18] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 23] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 10] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 24] + data_combine[, 12] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 13] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 24] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 17] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 24] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 21] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 25] + data_combine[, 12] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 25] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 16] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 25] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 25] * data_combine[, 9] + data_combine[, 20] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 15] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 26] * data_combine[, 9] + data_combine[, 19] * data_combine[, 26] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 21] * data_combine[, 26] * data_combine[, 9] + data_combine[, 10] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 11] * data_combine[, 27] +
    data_combine[, 12] * data_combine[, 27] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 14] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 15] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 27] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 18] * data_combine[, 27] *
    data_combine[, 9] + data_combine[, 19] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 27] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 27] * data_combine[, 9] + data_combine[, 10] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 28] + data_combine[, 12] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 13] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 28] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 17] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9] + data_combine[, 21] * data_combine[, 28] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 29] + data_combine[, 12] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 29] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 16] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 29] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 29] * data_combine[, 9] + data_combine[, 20] * data_combine[, 29] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30] +
    data_combine[, 12] * data_combine[, 30] * data_combine[, 9] + data_combine[, 13] *
    data_combine[, 30] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 30] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 16] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 19] * data_combine[, 30] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 30] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 30] * data_combine[, 9] + data_combine[, 10] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 11] * data_combine[, 31] + data_combine[, 12] * data_combine[, 31] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 31] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 16] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 19] * data_combine[, 31] *
    data_combine[, 9] +
    data_combine[, 20] * data_combine[, 31] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 31] * data_combine[, 9] + data_combine[, 10] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 32] + data_combine[, 12] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 13] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 15] * data_combine[, 32] * data_combine[, 9] + data_combine[, 16] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 17] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9] + data_combine[, 21] * data_combine[, 32] *
    data_combine[, 9] + data_combine[, 10] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 33] + data_combine[, 12] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 33] * data_combine[, 9] + data_combine[, 15] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 16] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 33] * data_combine[, 9] +
    data_combine[, 18] * data_combine[, 33] * data_combine[, 9] + data_combine[, 19] *
    data_combine[, 33] * data_combine[, 9] + data_combine[, 20] * data_combine[, 33] *
    data_combine[, 9] + data_combine[, 21] * data_combine[, 33] * data_combine[, 9],
  family = Gamma(link = "log"),
  data = data_combine
)

fit_w_gamma_log = step(fit_width_gamma_log)
fit_w_gamma_log_up = glm(formula = data_combine[, 2] ~ 
                            data_combine[,14] + data_combine[, 15] + data_combine[, 16] + 
                           data_combine[,18] + data_combine[, 23] + data_combine[,24] + 
                           data_combine[, 25] + data_combine[,28] + data_combine[, 29] + 
                            data_combine[, 10]:data_combine[, 25] + 
                           data_combine[,16]:data_combine[, 25] + data_combine[,19]:data_combine[, 25] + 
                            data_combine[,15]:data_combine[, 28] + 
                           data_combine[,18]:data_combine[, 28] + data_combine[,15]:data_combine[, 29] + 
                           data_combine[,10]:data_combine[, 30]  + 
                           data_combine[,10]:data_combine[, 33] + data_combine[,16]:data_combine[, 33] + 
                           data_combine[,9]:data_combine[,25] + data_combine[,9]:data_combine[, 14] + 
                           data_combine[,9]:data_combine[, 15] + data_combine[,9]:data_combine[,29] + 
                           
                           data_combine[,9]:data_combine[, 15]:data_combine[, 29], family = Gamma(link = "log"), 
                             data = data_combine)
summary(fit_w_gamma_log_up)

##plot
RMSEWlog.Gamma = drawScatter(fit_w_gamma_log_up, 2)
predwgammalog = predict(fit_w_gamma_log_up,data_combine)
predwgammaid = predict(fit_w_gamma_Id_up,data_combine)

## comparison for width
xlabel = 1:length(data_combine[, 1])

plot(
  xlabel,
  sort(predwgammaid),
  xlab = "",
  ylab = "",
  col = "red",
  type = "b",
  pch = 4
)
lines(xlabel,
      sort(data_combine[, 2]),
      col = "blue",
      type = "b")

lines(xlabel,
      sort(predwgammalog),
      col = "green",
      type = "b")

legend(
  "topleft",
  inset = .05,
  c("Gamma(Id)","raw data" ,"Gamma(log)"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue","green")
)

















# save the result
saveModel("tidywidthGammaLog.csv", model = fit_w_gamma_log_up)

# prediction for one year

data_c1year = read.csv('data1year_combine.csv', header = T)
data_medium = data_combine
data_combine = data_c1year
OneYearPred(fit_w_gamma_log_up, data_combine, "WGammalogOneYear.csv")
data_combine = data_medium


## histogram and statistic analysis of the outcomes

## Volume only one model
#VolumeOneYear.csv
volumePre = read.csv("VolumeOneYear.csv")
## Length: two model, general linear model and gamma regression model
#LlinearOneYear.csv
#LGammaIdlinearOneYear.csv
#LGammaLoglinearOneYear.csv

LlinearPre = read.csv("LlinearOneYear.csv")
LlinearPre = as.vector(LlinearPre)
LGammaIdOneYearPre = read.csv("LGammaIdlinearOneYear.csv")
LGammaLog = read.csv("LGammaLoglinearOneYear.csv")

xlabel = 1:length(LlinearPre$x)
plot(xlabel, sort(LlinearPre$x))
plot(
  xlabel,
  pred,
  xlab = "",
  ylab = "",
  col = "red",
  type = "b",
  pch = 4
)
lines(xlabel,
      sort(LlinearPre$x),
      col = "blue",
      type = "b")
lines(xlabel,
      sort(LGammaIdOneYearPre$x),
      col = "green",
      type = "b")
lines(xlabel,
      sort(LGammaLog$x),
      col = "red",
      type = "b")

legend(
  "topleft",
  inset = .05,
  c("Linear", "Gamma(Id)","Gamma(Log)"),
  lty = c(1, 2),
  #pch = c(4, 1),
  col = c("blue", "green","red")
)





## Width : three model, general linear model, general linear model with length as the estimated term

#WlinearOneYear.csv
#wlinearLOneYear.csv
#WGammaIdOneYear.csv
#WGammalogOneYear.csv


WlinearPre = read.csv("WlinearOneYear.csv")
WlinearLPre = read.csv("wlinearLOneYear.csv")
WGammaIdPre = read.csv("WGammaIdOneYear.csv")
WGammaLogPre = read.csv("WGammalogOneYear.csv")


plot(xlabel,
      sort(WlinearPre$x),
      col = "blue",
      type = "b")
lines(xlabel,
      sort(WlinearLPre$x),
      col = "green",
      type = "b")
lines(xlabel,
      sort(WGammaIdPre$x),
      col = "red",
      type = "b")
lines(xlabel,
     sort(WGammaLogPre$x),type = "b"
    )


legend(
  "topleft",
  inset = .05,
  c("Linear","Linear(L)", "Gamma(Id)","Gamma(Log)"),
  lty = c(1, 2),
  #pch = c(4, 1),
  col = c("blue", "green","red","black")
)

### HEIGHT
## obtain height data one year
heightOneYear = read.csv("Height1Year.csv")
summary(heightOneYear)

xlabel = 1:length(heightOneYear$锘縃eight..cm.)
plot(xlabel,
     sort(heightOneYear$锘縃eight..cm.),
     col = "blue",
     ylab = "Prediction of height in one year",
     type = "b")
