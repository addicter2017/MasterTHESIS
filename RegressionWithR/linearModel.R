library(psych)
library(report)
library(stargazer)
library(tidyverse)
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
des_col = model.matrix( ~ destinations, data_pre)[, -1]
com_col = model.matrix( ~ commodity, data_pre)[, -1]

# combine new columns and delete old categorical columns
data_combine = cbind(data_pre[, -c(1, 7)], des_col, com_col)
# sp = boxplot(data_combine[,4],boxwex = 0.7)
# outlier_location = sapply(data_combine,function(x){which(x%in%sp$out)})
# outlier_index = outlier_location$volume

# data_combine = data_combine[-outlier_index,]
write.csv(data_combine,
          file = "data_combine.csv",
          quote = F,
          row.names = F)
fit_volume = glm(
  data_combine[, 4] ~ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13]
  + data_combine[, 14] + data_combine[, 16] + data_combine[, 17] +
    data_combine[, 18]
  + data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23]
  + data_combine[, 24] + data_combine[, 25] + data_combine[, 26] +
    data_combine[, 27] + data_combine[, 28]
  + data_combine[, 29] + data_combine[, 30] + data_combine[, 31] +
    data_combine[, 32]
  + data_combine[, 10] * data_combine[, 22] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22]
  + data_combine[, 13] * data_combine[, 22] + data_combine[, 14] *
    data_combine[, 22]
  + data_combine[, 16] * data_combine[, 22] + data_combine[, 17] *
    data_combine[, 22] + data_combine[, 18] * data_combine[, 22]
  + data_combine[, 19] * data_combine[, 22] + data_combine[, 20] *
    data_combine[, 22] + data_combine[, 21] * data_combine[, 22]
  + data_combine[, 10] * data_combine[, 23] + data_combine[, 11] *
    data_combine[, 23] + data_combine[, 12] * data_combine[, 23]
  + data_combine[, 13] * data_combine[, 23] + data_combine[, 14] *
    data_combine[, 23]
  + data_combine[, 16] * data_combine[, 23] + data_combine[, 17] *
    data_combine[, 23] + data_combine[, 18] * data_combine[, 23]
  + data_combine[, 19] * data_combine[, 23] + data_combine[, 20] *
    data_combine[, 23] + data_combine[, 21] * data_combine[, 23]
  + data_combine[, 10] * data_combine[, 24] + data_combine[, 11] *
    data_combine[, 24] + data_combine[, 12] * data_combine[, 24]
  + data_combine[, 13] * data_combine[, 24] + data_combine[, 14] *
    data_combine[, 24]
  + data_combine[, 16] * data_combine[, 24] + data_combine[, 17] *
    data_combine[, 24] + data_combine[, 18] * data_combine[, 24]
  + data_combine[, 19] * data_combine[, 24] + data_combine[, 20] *
    data_combine[, 24] + data_combine[, 21] * data_combine[, 24]
  + data_combine[, 10] * data_combine[, 25] + data_combine[, 11] *
    data_combine[, 25] + data_combine[, 12] * data_combine[, 25]
  + data_combine[, 13] * data_combine[, 25] + data_combine[, 14] *
    data_combine[, 25]
  + data_combine[, 16] * data_combine[, 25] + data_combine[, 17] *
    data_combine[, 25] + data_combine[, 18] * data_combine[, 25]
  + data_combine[, 19] * data_combine[, 25] + data_combine[, 20] *
    data_combine[, 25] + data_combine[, 21] * data_combine[, 25]
  + data_combine[, 10] * data_combine[, 26] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26]
  + data_combine[, 13] * data_combine[, 26] + data_combine[, 14] *
    data_combine[, 26]
  + data_combine[, 16] * data_combine[, 26] + data_combine[, 17] *
    data_combine[, 26] + data_combine[, 18] * data_combine[, 26]
  + data_combine[, 19] * data_combine[, 26] + data_combine[, 20] *
    data_combine[, 26] + data_combine[, 21] * data_combine[, 26]
  + data_combine[, 10] * data_combine[, 27] + data_combine[, 11] *
    data_combine[, 27] + data_combine[, 12] * data_combine[, 27]
  + data_combine[, 13] * data_combine[, 27] + data_combine[, 14] *
    data_combine[, 27]
  + data_combine[, 16] * data_combine[, 27] + data_combine[, 17] *
    data_combine[, 27] + data_combine[, 18] * data_combine[, 27]
  + data_combine[, 19] * data_combine[, 27] + data_combine[, 20] *
    data_combine[, 27] + data_combine[, 21] * data_combine[, 27]
  + data_combine[, 10] * data_combine[, 28] + data_combine[, 11] *
    data_combine[, 28] + data_combine[, 12] * data_combine[, 28]
  + data_combine[, 13] * data_combine[, 28] + data_combine[, 14] *
    data_combine[, 28]
  + data_combine[, 16] * data_combine[, 28] + data_combine[, 17] *
    data_combine[, 28] + data_combine[, 18] * data_combine[, 28]
  + data_combine[, 19] * data_combine[, 28] + data_combine[, 20] *
    data_combine[, 28] + data_combine[, 21] * data_combine[, 28]
  + data_combine[, 10] * data_combine[, 29] + data_combine[, 11] *
    data_combine[, 29] + data_combine[, 12] * data_combine[, 29]
  + data_combine[, 13] * data_combine[, 29] + data_combine[, 14] *
    data_combine[, 29]
  + data_combine[, 16] * data_combine[, 29] + data_combine[, 17] *
    data_combine[, 29] + data_combine[, 18] * data_combine[, 29]
  + data_combine[, 19] * data_combine[, 29] + data_combine[, 20] *
    data_combine[, 29] + data_combine[, 21] * data_combine[, 29]
  + data_combine[, 10] * data_combine[, 30] + data_combine[, 11] *
    data_combine[, 30] + data_combine[, 12] * data_combine[, 30]
  + data_combine[, 13] * data_combine[, 30] + data_combine[, 14] *
    data_combine[, 30]
  + data_combine[, 16] * data_combine[, 30] + data_combine[, 17] *
    data_combine[, 30] + data_combine[, 18] * data_combine[, 30]
  + data_combine[, 19] * data_combine[, 30] + data_combine[, 20] *
    data_combine[, 30] + data_combine[, 21] * data_combine[, 30]
  + data_combine[, 10] * data_combine[, 31] + data_combine[, 11] *
    data_combine[, 31] + data_combine[, 12] * data_combine[, 31]
  + data_combine[, 13] * data_combine[, 31] + data_combine[, 14] *
    data_combine[, 31]
  + data_combine[, 16] * data_combine[, 31] + data_combine[, 17] *
    data_combine[, 31] + data_combine[, 18] * data_combine[, 31]
  + data_combine[, 19] * data_combine[, 31] + data_combine[, 20] *
    data_combine[, 31] + data_combine[, 21] * data_combine[, 31]
  + data_combine[, 10] * data_combine[, 32] + data_combine[, 11] *
    data_combine[, 32] + data_combine[, 12] * data_combine[, 32]
  + data_combine[, 13] * data_combine[, 32] + data_combine[, 14] *
    data_combine[, 32]
  + data_combine[, 16] * data_combine[, 32] + data_combine[, 17] *
    data_combine[, 32] + data_combine[, 18] * data_combine[, 32]
  + data_combine[, 19] * data_combine[, 32] + data_combine[, 20] *
    data_combine[, 32] + data_combine[, 21] * data_combine[, 32]
  + data_combine[, 10] * data_combine[, 9] + data_combine[, 22] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 9]
  + data_combine[, 23] * data_combine[, 9] + data_combine[, 12] *
    data_combine[, 9] + data_combine[, 24] * data_combine[, 9]
  + data_combine[, 13] * data_combine[, 9] + data_combine[, 25] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 9]
  + data_combine[, 26] * data_combine[, 9] + data_combine[, 27] *
    data_combine[, 9]
  + data_combine[, 16] * data_combine[, 9] + data_combine[, 28] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 9]
  + data_combine[, 29] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 9] + data_combine[, 30] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 9] + data_combine[, 31] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 9]
  + data_combine[, 32] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 22] + data_combine[, 12] * data_combine[, 22] *
    data_combine[, 9]
  + data_combine[, 13] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 22] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 22] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 10] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 23] + data_combine[, 12] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 23] * data_combine[, 9]
  + data_combine[, 14] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 23] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 21] * data_combine[, 23] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 24] + data_combine[, 12] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 24] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 24] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 24] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 24] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 10] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 25]
  + data_combine[, 12] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 25] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 25] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 25] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26]
  + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 26] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 27] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 27]
  + data_combine[, 12] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 27] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 27] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 27] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 28] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 28]
  + data_combine[, 12] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 28] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 28] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 29] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 29]
  + data_combine[, 12] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 29] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 29] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 29] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30]
  + data_combine[, 12] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 30] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 30] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 31] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 31]
  + data_combine[, 12] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 31] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 31] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 32] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 32]
  + data_combine[, 12] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 32] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 32] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 32] * data_combine[, 9],
  family = Gamma(link = "log"),
  data = data_combine
)


## Plot the prediction versus original data
fit_v = step(fit_volume, direction = 'both')

fit_v_up = glm(
  formula = data_combine[, 4] ~ data_combine[, 9] + data_combine[, 10] + data_combine[, 11]
  + data_combine[, 13]  + data_combine[, 16] + data_combine[, 17] 
  + data_combine[, 20] + data_combine[, 21] + data_combine[, 22] 
  + data_combine[, 24] +  data_combine[, 27] + data_combine[, 28] 
  + data_combine[, 13]:data_combine[, 24] 
  +  data_combine[, 18]:data_combine[, 24] 
  + data_combine[, 14]:data_combine[, 27] + data_combine[, 18]:data_combine[, 27] 
  + data_combine[, 10]:data_combine[, 29]
  + data_combine[, 9]:data_combine[, 24] 
  + data_combine[, 9]:data_combine[, 13] + data_combine[,9]:data_combine[, 14] 
  + data_combine[, 9]:data_combine[,28] + data_combine[, 9]:data_combine[, 14]:data_combine[,24],
  family = Gamma(link = "log"),
  data = data_combine
)

summary(fit_v_up)
xlabel = 1:length(data_combine[, 1])
pred = predict(fit_v_up, data_combine, type = 'response')
Rmsev = sqrt(mean((pred - data_combine[, 4]) ^ 2))
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
lines(xlabel, sort(data_combine[, 4]), col = "blue", type = "b")
legend(
  "topleft",
  inset = .05,
  c("prediction", "raw data"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue")
)

## save the result
tidy_Gmv = tidy(fit_v_up)
write.csv(tidy_Gmv, file = "FGMv.csv")
## predicting volume for one year data
# data_one_year = read.csv('data_oneyear.csv')
# names(data_one_year) = c('destinations','length','width','height','volume','weight','commodity','fragile','iflammable','fits_rate','types')
# # deal with categorical variables
# dest_col = model.matrix(~destinations, data_one_year)[,-1]
# commod_col = model.matrix(~commodity, data_one_year)[,-1]
#
# # combine new columns and delete old categorical columns
# data_oneyear_combine = cbind(data_one_year[,-c(1,7)],dest_col,commod_col)
# write.csv(data_oneyear_combine,file="data1year_combine.csv",quote=F,row.names = F)
data_c1year = read.csv('data1year_combine.csv')
data_medium = data_combine
data_combine = data_c1year
oneYearpredict = predict(fit_v_up, data_combine, type = 'response')
write.csv(oneYearpredict,
          file = "Fvolume1year.csv",
          quote = F,
          row.names = F)
data_combine = data_medium


# Length
fit_length = glm(
  data_combine[, 1] ~ data_combine[, 4] + data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13]
  + data_combine[, 14] + data_combine[, 16] + data_combine[, 17] +
    data_combine[, 18]
  + data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23]
  + data_combine[, 24] + data_combine[, 25] + data_combine[, 26] +
    data_combine[, 27] + data_combine[, 28]
  + data_combine[, 29] + data_combine[, 30] + data_combine[, 31] +
    data_combine[, 32]
  + data_combine[, 10] * data_combine[, 22] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22]
  + data_combine[, 13] * data_combine[, 22] + data_combine[, 14] *
    data_combine[, 22]
  + data_combine[, 16] * data_combine[, 22] + data_combine[, 17] *
    data_combine[, 22] + data_combine[, 18] * data_combine[, 22]
  + data_combine[, 19] * data_combine[, 22] + data_combine[, 20] *
    data_combine[, 22] + data_combine[, 21] * data_combine[, 22]
  + data_combine[, 10] * data_combine[, 23] + data_combine[, 11] *
    data_combine[, 23] + data_combine[, 12] * data_combine[, 23]
  + data_combine[, 13] * data_combine[, 23] + data_combine[, 14] *
    data_combine[, 23]
  + data_combine[, 16] * data_combine[, 23] + data_combine[, 17] *
    data_combine[, 23] + data_combine[, 18] * data_combine[, 23]
  + data_combine[, 19] * data_combine[, 23] + data_combine[, 20] *
    data_combine[, 23] + data_combine[, 21] * data_combine[, 23]
  + data_combine[, 10] * data_combine[, 24] + data_combine[, 11] *
    data_combine[, 24] + data_combine[, 12] * data_combine[, 24]
  + data_combine[, 13] * data_combine[, 24] + data_combine[, 14] *
    data_combine[, 24]
  + data_combine[, 16] * data_combine[, 24] + data_combine[, 17] *
    data_combine[, 24] + data_combine[, 18] * data_combine[, 24]
  + data_combine[, 19] * data_combine[, 24] + data_combine[, 20] *
    data_combine[, 24] + data_combine[, 21] * data_combine[, 24]
  + data_combine[, 10] * data_combine[, 25] + data_combine[, 11] *
    data_combine[, 25] + data_combine[, 12] * data_combine[, 25]
  + data_combine[, 13] * data_combine[, 25] + data_combine[, 14] *
    data_combine[, 25]
  + data_combine[, 16] * data_combine[, 25] + data_combine[, 17] *
    data_combine[, 25] + data_combine[, 18] * data_combine[, 25]
  + data_combine[, 19] * data_combine[, 25] + data_combine[, 20] *
    data_combine[, 25] + data_combine[, 21] * data_combine[, 25]
  + data_combine[, 10] * data_combine[, 26] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26]
  + data_combine[, 13] * data_combine[, 26] + data_combine[, 14] *
    data_combine[, 26]
  + data_combine[, 16] * data_combine[, 26] + data_combine[, 17] *
    data_combine[, 26] + data_combine[, 18] * data_combine[, 26]
  + data_combine[, 19] * data_combine[, 26] + data_combine[, 20] *
    data_combine[, 26] + data_combine[, 21] * data_combine[, 26]
  + data_combine[, 10] * data_combine[, 27] + data_combine[, 11] *
    data_combine[, 27] + data_combine[, 12] * data_combine[, 27]
  + data_combine[, 13] * data_combine[, 27] + data_combine[, 14] *
    data_combine[, 27]
  + data_combine[, 16] * data_combine[, 27] + data_combine[, 17] *
    data_combine[, 27] + data_combine[, 18] * data_combine[, 27]
  + data_combine[, 19] * data_combine[, 27] + data_combine[, 20] *
    data_combine[, 27] + data_combine[, 21] * data_combine[, 27]
  + data_combine[, 10] * data_combine[, 28] + data_combine[, 11] *
    data_combine[, 28] + data_combine[, 12] * data_combine[, 28]
  + data_combine[, 13] * data_combine[, 28] + data_combine[, 14] *
    data_combine[, 28]
  + data_combine[, 16] * data_combine[, 28] + data_combine[, 17] *
    data_combine[, 28] + data_combine[, 18] * data_combine[, 28]
  + data_combine[, 19] * data_combine[, 28] + data_combine[, 20] *
    data_combine[, 28] + data_combine[, 21] * data_combine[, 28]
  + data_combine[, 10] * data_combine[, 29] + data_combine[, 11] *
    data_combine[, 29] + data_combine[, 12] * data_combine[, 29]
  + data_combine[, 13] * data_combine[, 29] + data_combine[, 14] *
    data_combine[, 29]
  + data_combine[, 16] * data_combine[, 29] + data_combine[, 17] *
    data_combine[, 29] + data_combine[, 18] * data_combine[, 29]
  + data_combine[, 19] * data_combine[, 29] + data_combine[, 20] *
    data_combine[, 29] + data_combine[, 21] * data_combine[, 29]
  + data_combine[, 10] * data_combine[, 30] + data_combine[, 11] *
    data_combine[, 30] + data_combine[, 12] * data_combine[, 30]
  + data_combine[, 13] * data_combine[, 30] + data_combine[, 14] *
    data_combine[, 30]
  + data_combine[, 16] * data_combine[, 30] + data_combine[, 17] *
    data_combine[, 30] + data_combine[, 18] * data_combine[, 30]
  + data_combine[, 19] * data_combine[, 30] + data_combine[, 20] *
    data_combine[, 30] + data_combine[, 21] * data_combine[, 30]
  + data_combine[, 10] * data_combine[, 31] + data_combine[, 11] *
    data_combine[, 31] + data_combine[, 12] * data_combine[, 31]
  + data_combine[, 13] * data_combine[, 31] + data_combine[, 14] *
    data_combine[, 31]
  + data_combine[, 16] * data_combine[, 31] + data_combine[, 17] *
    data_combine[, 31] + data_combine[, 18] * data_combine[, 31]
  + data_combine[, 19] * data_combine[, 31] + data_combine[, 20] *
    data_combine[, 31] + data_combine[, 21] * data_combine[, 31]
  + data_combine[, 10] * data_combine[, 32] + data_combine[, 11] *
    data_combine[, 32] + data_combine[, 12] * data_combine[, 32]
  + data_combine[, 13] * data_combine[, 32] + data_combine[, 14] *
    data_combine[, 32]
  + data_combine[, 16] * data_combine[, 32] + data_combine[, 17] *
    data_combine[, 32] + data_combine[, 18] * data_combine[, 32]
  + data_combine[, 19] * data_combine[, 32] + data_combine[, 20] *
    data_combine[, 32] + data_combine[, 21] * data_combine[, 32]
  + data_combine[, 10] * data_combine[, 9] + data_combine[, 22] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 9]
  + data_combine[, 23] * data_combine[, 9] + data_combine[, 12] *
    data_combine[, 9] + data_combine[, 24] * data_combine[, 9]
  + data_combine[, 13] * data_combine[, 9] + data_combine[, 25] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 9]
  + data_combine[, 26] * data_combine[, 9] + data_combine[, 27] *
    data_combine[, 9]
  + data_combine[, 16] * data_combine[, 9] + data_combine[, 28] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 9]
  + data_combine[, 29] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 9] + data_combine[, 30] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 9] + data_combine[, 31] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 9]
  + data_combine[, 32] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 22] + data_combine[, 12] * data_combine[, 22] *
    data_combine[, 9]
  + data_combine[, 13] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 22] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 22] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 10] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 23] + data_combine[, 12] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 23] * data_combine[, 9]
  + data_combine[, 14] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 23] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 21] * data_combine[, 23] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 24] + data_combine[, 12] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 24] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 24] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 24] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 24] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 10] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 25]
  + data_combine[, 12] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 25] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 25] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 25] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26]
  + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 26] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 27] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 27]
  + data_combine[, 12] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 27] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 27] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 27] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 28] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 28]
  + data_combine[, 12] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 28] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 28] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 29] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 29]
  + data_combine[, 12] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 29] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 29] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 29] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30]
  + data_combine[, 12] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 30] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 30] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 31] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 31]
  + data_combine[, 12] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 31] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 31] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 32] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 32]
  + data_combine[, 12] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 32] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 32] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 32] * data_combine[, 9],
  family = Gamma(link = "log"),
  data = data_combine
)
## Length plot
fit_l = step(fit_length)

fit_l_up = glm(data_combine[, 1] ~ data_combine[, 4] + data_combine[,10] 
+ data_combine[, 12] 
+  data_combine[, 17] + data_combine[, 18] 
+ data_combine[, 23] + data_combine[, 27] 
+ data_combine[,18]:data_combine[, 24] 
+ data_combine[,18]:data_combine[, 27] + data_combine[, 13]:data_combine[,28] 
+ data_combine[, 14]:data_combine[, 28]
+ data_combine[, 10]:data_combine[, 29] 
,
family = Gamma(link = "log"),
data = data_combine
)

summary(fit_l_up)

xlabel = 1:length(data_combine[, 1])
pred = predict(fit_l_up, data_combine, type = 'response')
Rmsel = sqrt(mean((pred - data_combine[, 1]) ^ 2))
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
lines(xlabel, sort(data_combine[, 1]), col = "blue", type = "b")
legend(
  "topleft",
  inset = .05,
  c("prediction", "raw data"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue")
)

## save the result
tidy_Gml = tidy(fit_l_up)
write.csv(tidy_Gml, file = "GML.csv")
## predicting volume for one year data
# data_one_year = read.csv('data_oneyear.csv')
# names(data_one_year) = c('destinations','length','width','height','volume','weight','commodity','fragile','iflammable','fits_rate','types')
# # deal with categorical variables
# dest_col = model.matrix(~destinations, data_one_year)[,-1]
# commod_col = model.matrix(~commodity, data_one_year)[,-1]
#
# # combine new columns and delete old categorical columns
# data_oneyear_combine = cbind(data_one_year[,-c(1,7)],dest_col,commod_col)
# write.csv(data_oneyear_combine,file="data1year_combine.csv",quote=F,row.names = F)
data_c1year = read.csv('data1year_combine.csv')
data_medium = data_combine
data_combine = data_c1year
oneYearpredict = predict(fit_l_up, data_combine, type = 'response')
write.csv(oneYearpredict,
          file = "Flength1year.csv",
          quote = F,
          row.names = F)
data_combine = data_medium


### modeling for width
fit_width = glm(
  data_combine[, 2] ~ data_combine[, 1] + data_combine[, 4] + data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13]
  + data_combine[, 14] + data_combine[, 16] + data_combine[, 17] +
    data_combine[, 18]
  + data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23]
  + data_combine[, 24] + data_combine[, 25] + data_combine[, 26] +
    data_combine[, 27] + data_combine[, 28]
  + data_combine[, 29] + data_combine[, 30] + data_combine[, 31] +
    data_combine[, 32]
  + data_combine[, 10] * data_combine[, 22] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22]
  + data_combine[, 13] * data_combine[, 22] + data_combine[, 14] *
    data_combine[, 22]
  + data_combine[, 16] * data_combine[, 22] + data_combine[, 17] *
    data_combine[, 22] + data_combine[, 18] * data_combine[, 22]
  + data_combine[, 19] * data_combine[, 22] + data_combine[, 20] *
    data_combine[, 22] + data_combine[, 21] * data_combine[, 22]
  + data_combine[, 10] * data_combine[, 23] + data_combine[, 11] *
    data_combine[, 23] + data_combine[, 12] * data_combine[, 23]
  + data_combine[, 13] * data_combine[, 23] + data_combine[, 14] *
    data_combine[, 23]
  + data_combine[, 16] * data_combine[, 23] + data_combine[, 17] *
    data_combine[, 23] + data_combine[, 18] * data_combine[, 23]
  + data_combine[, 19] * data_combine[, 23] + data_combine[, 20] *
    data_combine[, 23] + data_combine[, 21] * data_combine[, 23]
  + data_combine[, 10] * data_combine[, 24] + data_combine[, 11] *
    data_combine[, 24] + data_combine[, 12] * data_combine[, 24]
  + data_combine[, 13] * data_combine[, 24] + data_combine[, 14] *
    data_combine[, 24]
  + data_combine[, 16] * data_combine[, 24] + data_combine[, 17] *
    data_combine[, 24] + data_combine[, 18] * data_combine[, 24]
  + data_combine[, 19] * data_combine[, 24] + data_combine[, 20] *
    data_combine[, 24] + data_combine[, 21] * data_combine[, 24]
  + data_combine[, 10] * data_combine[, 25] + data_combine[, 11] *
    data_combine[, 25] + data_combine[, 12] * data_combine[, 25]
  + data_combine[, 13] * data_combine[, 25] + data_combine[, 14] *
    data_combine[, 25]
  + data_combine[, 16] * data_combine[, 25] + data_combine[, 17] *
    data_combine[, 25] + data_combine[, 18] * data_combine[, 25]
  + data_combine[, 19] * data_combine[, 25] + data_combine[, 20] *
    data_combine[, 25] + data_combine[, 21] * data_combine[, 25]
  + data_combine[, 10] * data_combine[, 26] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26]
  + data_combine[, 13] * data_combine[, 26] + data_combine[, 14] *
    data_combine[, 26]
  + data_combine[, 16] * data_combine[, 26] + data_combine[, 17] *
    data_combine[, 26] + data_combine[, 18] * data_combine[, 26]
  + data_combine[, 19] * data_combine[, 26] + data_combine[, 20] *
    data_combine[, 26] + data_combine[, 21] * data_combine[, 26]
  + data_combine[, 10] * data_combine[, 27] + data_combine[, 11] *
    data_combine[, 27] + data_combine[, 12] * data_combine[, 27]
  + data_combine[, 13] * data_combine[, 27] + data_combine[, 14] *
    data_combine[, 27]
  + data_combine[, 16] * data_combine[, 27] + data_combine[, 17] *
    data_combine[, 27] + data_combine[, 18] * data_combine[, 27]
  + data_combine[, 19] * data_combine[, 27] + data_combine[, 20] *
    data_combine[, 27] + data_combine[, 21] * data_combine[, 27]
  + data_combine[, 10] * data_combine[, 28] + data_combine[, 11] *
    data_combine[, 28] + data_combine[, 12] * data_combine[, 28]
  + data_combine[, 13] * data_combine[, 28] + data_combine[, 14] *
    data_combine[, 28]
  + data_combine[, 16] * data_combine[, 28] + data_combine[, 17] *
    data_combine[, 28] + data_combine[, 18] * data_combine[, 28]
  + data_combine[, 19] * data_combine[, 28] + data_combine[, 20] *
    data_combine[, 28] + data_combine[, 21] * data_combine[, 28]
  + data_combine[, 10] * data_combine[, 29] + data_combine[, 11] *
    data_combine[, 29] + data_combine[, 12] * data_combine[, 29]
  + data_combine[, 13] * data_combine[, 29] + data_combine[, 14] *
    data_combine[, 29]
  + data_combine[, 16] * data_combine[, 29] + data_combine[, 17] *
    data_combine[, 29] + data_combine[, 18] * data_combine[, 29]
  + data_combine[, 19] * data_combine[, 29] + data_combine[, 20] *
    data_combine[, 29] + data_combine[, 21] * data_combine[, 29]
  + data_combine[, 10] * data_combine[, 30] + data_combine[, 11] *
    data_combine[, 30] + data_combine[, 12] * data_combine[, 30]
  + data_combine[, 13] * data_combine[, 30] + data_combine[, 14] *
    data_combine[, 30]
  + data_combine[, 16] * data_combine[, 30] + data_combine[, 17] *
    data_combine[, 30] + data_combine[, 18] * data_combine[, 30]
  + data_combine[, 19] * data_combine[, 30] + data_combine[, 20] *
    data_combine[, 30] + data_combine[, 21] * data_combine[, 30]
  + data_combine[, 10] * data_combine[, 31] + data_combine[, 11] *
    data_combine[, 31] + data_combine[, 12] * data_combine[, 31]
  + data_combine[, 13] * data_combine[, 31] + data_combine[, 14] *
    data_combine[, 31]
  + data_combine[, 16] * data_combine[, 31] + data_combine[, 17] *
    data_combine[, 31] + data_combine[, 18] * data_combine[, 31]
  + data_combine[, 19] * data_combine[, 31] + data_combine[, 20] *
    data_combine[, 31] + data_combine[, 21] * data_combine[, 31]
  + data_combine[, 10] * data_combine[, 32] + data_combine[, 11] *
    data_combine[, 32] + data_combine[, 12] * data_combine[, 32]
  + data_combine[, 13] * data_combine[, 32] + data_combine[, 14] *
    data_combine[, 32]
  + data_combine[, 16] * data_combine[, 32] + data_combine[, 17] *
    data_combine[, 32] + data_combine[, 18] * data_combine[, 32]
  + data_combine[, 19] * data_combine[, 32] + data_combine[, 20] *
    data_combine[, 32] + data_combine[, 21] * data_combine[, 32]
  + data_combine[, 10] * data_combine[, 9] + data_combine[, 22] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 9]
  + data_combine[, 23] * data_combine[, 9] + data_combine[, 12] *
    data_combine[, 9] + data_combine[, 24] * data_combine[, 9]
  + data_combine[, 13] * data_combine[, 9] + data_combine[, 25] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 9]
  + data_combine[, 26] * data_combine[, 9] + data_combine[, 27] *
    data_combine[, 9]
  + data_combine[, 16] * data_combine[, 9] + data_combine[, 28] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 9]
  + data_combine[, 29] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 9] + data_combine[, 30] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 9] + data_combine[, 31] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 9]
  + data_combine[, 32] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 22] + data_combine[, 12] * data_combine[, 22] *
    data_combine[, 9]
  + data_combine[, 13] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 22] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 22] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 10] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 23] + data_combine[, 12] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 23] * data_combine[, 9]
  + data_combine[, 14] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 23] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 21] * data_combine[, 23] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 24] + data_combine[, 12] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 24] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 24] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 24] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 24] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 10] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 25]
  + data_combine[, 12] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 25] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 25] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 25] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26]
  + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 26] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 27] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 27]
  + data_combine[, 12] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 27] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 27] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 27] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 28] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 28]
  + data_combine[, 12] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 28] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 28] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 29] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 29]
  + data_combine[, 12] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 29] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 29] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 29] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30]
  + data_combine[, 12] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 30] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 30] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 31] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 31]
  + data_combine[, 12] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 31] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 31] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 32] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 32]
  + data_combine[, 12] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 32] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 32] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 32] * data_combine[, 9],
  family = Gamma(link = "log"),
  data = data_combine
)

## Width plot
fit_wd = step(fit_width)
fit_wd_up = glm(formula = data_combine[, 2] ~ data_combine[, 1] + data_combine[,4] 
                + data_combine[, 9] + data_combine[, 10] 
                + data_combine[, 14] 
                + data_combine[, 17]   
                + data_combine[, 22]
                 + data_combine[,27] 
                + data_combine[, 28] 
                + data_combine[, 14]:data_combine[, 24] + data_combine[,16]:data_combine[, 24] 
                + data_combine[, 18]:data_combine[,24] 
                + data_combine[, 14]:data_combine[, 28]
                + data_combine[, 10]:data_combine[, 29]
                 + data_combine[, 16]:data_combine[, 32] 
    
                + data_combine[, 9]:data_combine[, 14] + data_combine[,     9]:data_combine[, 28] 
                +  data_combine[, 9]:data_combine[, 14]:data_combine[, 24] 
                + data_combine[, 9]:data_combine[, 14]:data_combine[, 28], family = Gamma(link = "log"), data = data_combine)

summary(fit_wd_up)
xlabel = 1:length(data_combine[, 1])
pred = predict(fit_wd_up, data_combine, type = 'response')
Rmsewd = sqrt(mean((pred - data_combine[, 2]) ^ 2))
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
lines(xlabel, sort(data_combine[, 2]), col = "blue", type = "b")
legend(
  "topleft",
  inset = .05,
  c("prediction", "raw data"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue")
)

## save the result
tidy_Gmwd = tidy(fit_wd_up)
write.csv(tidy_Gmwd, file = "GMwd.csv")
## predicting volume for one year data
# data_one_year = read.csv('data_oneyear.csv')
# names(data_one_year) = c('destinations','length','width','height','volume','weight','commodity','fragile','iflammable','fits_rate','types')
# # deal with categorical variables
# dest_col = model.matrix(~destinations, data_one_year)[,-1]
# commod_col = model.matrix(~commodity, data_one_year)[,-1]
#
# # combine new columns and delete old categorical columns
# data_oneyear_combine = cbind(data_one_year[,-c(1,7)],dest_col,commod_col)
# write.csv(data_oneyear_combine,file="data1year_combine.csv",quote=F,row.names = F)
data_c1year = read.csv('data1year_combine.csv')
data_medium = data_combine
data_combine = data_c1year
oneYearpredict = predict(fit_wd_up, data_combine, type = 'response')
write.csv(oneYearpredict,
          file = "Fwidth1year.csv",
          quote = F,
          row.names = F)
data_combine = data_medium


# Weight

data_combine = na.omit(data_combine)
fit_weight = glm(
  data_combine[, 5] ~ data_combine[,4]+ data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13]
  + data_combine[, 14] + data_combine[, 16] + data_combine[, 17] +
    data_combine[, 18]
  + data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23]
  + data_combine[, 24] + data_combine[, 25] + data_combine[, 26] +
    data_combine[, 27] + data_combine[, 28]
  + data_combine[, 29] + data_combine[, 30] + data_combine[, 31] +
    data_combine[, 32]
  + data_combine[, 10] * data_combine[, 22] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22]
  + data_combine[, 13] * data_combine[, 22] + data_combine[, 14] *
    data_combine[, 22]
  + data_combine[, 16] * data_combine[, 22] + data_combine[, 17] *
    data_combine[, 22] + data_combine[, 18] * data_combine[, 22]
  + data_combine[, 19] * data_combine[, 22] + data_combine[, 20] *
    data_combine[, 22] + data_combine[, 21] * data_combine[, 22]
  + data_combine[, 10] * data_combine[, 23] + data_combine[, 11] *
    data_combine[, 23] + data_combine[, 12] * data_combine[, 23]
  + data_combine[, 13] * data_combine[, 23] + data_combine[, 14] *
    data_combine[, 23]
  + data_combine[, 16] * data_combine[, 23] + data_combine[, 17] *
    data_combine[, 23] + data_combine[, 18] * data_combine[, 23]
  + data_combine[, 19] * data_combine[, 23] + data_combine[, 20] *
    data_combine[, 23] + data_combine[, 21] * data_combine[, 23]
  + data_combine[, 10] * data_combine[, 24] + data_combine[, 11] *
    data_combine[, 24] + data_combine[, 12] * data_combine[, 24]
  + data_combine[, 13] * data_combine[, 24] + data_combine[, 14] *
    data_combine[, 24]
  + data_combine[, 16] * data_combine[, 24] + data_combine[, 17] *
    data_combine[, 24] + data_combine[, 18] * data_combine[, 24]
  + data_combine[, 19] * data_combine[, 24] + data_combine[, 20] *
    data_combine[, 24] + data_combine[, 21] * data_combine[, 24]
  + data_combine[, 10] * data_combine[, 25] + data_combine[, 11] *
    data_combine[, 25] + data_combine[, 12] * data_combine[, 25]
  + data_combine[, 13] * data_combine[, 25] + data_combine[, 14] *
    data_combine[, 25]
  + data_combine[, 16] * data_combine[, 25] + data_combine[, 17] *
    data_combine[, 25] + data_combine[, 18] * data_combine[, 25]
  + data_combine[, 19] * data_combine[, 25] + data_combine[, 20] *
    data_combine[, 25] + data_combine[, 21] * data_combine[, 25]
  + data_combine[, 10] * data_combine[, 26] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26]
  + data_combine[, 13] * data_combine[, 26] + data_combine[, 14] *
    data_combine[, 26]
  + data_combine[, 16] * data_combine[, 26] + data_combine[, 17] *
    data_combine[, 26] + data_combine[, 18] * data_combine[, 26]
  + data_combine[, 19] * data_combine[, 26] + data_combine[, 20] *
    data_combine[, 26] + data_combine[, 21] * data_combine[, 26]
  + data_combine[, 10] * data_combine[, 27] + data_combine[, 11] *
    data_combine[, 27] + data_combine[, 12] * data_combine[, 27]
  + data_combine[, 13] * data_combine[, 27] + data_combine[, 14] *
    data_combine[, 27]
  + data_combine[, 16] * data_combine[, 27] + data_combine[, 17] *
    data_combine[, 27] + data_combine[, 18] * data_combine[, 27]
  + data_combine[, 19] * data_combine[, 27] + data_combine[, 20] *
    data_combine[, 27] + data_combine[, 21] * data_combine[, 27]
  + data_combine[, 10] * data_combine[, 28] + data_combine[, 11] *
    data_combine[, 28] + data_combine[, 12] * data_combine[, 28]
  + data_combine[, 13] * data_combine[, 28] + data_combine[, 14] *
    data_combine[, 28]
  + data_combine[, 16] * data_combine[, 28] + data_combine[, 17] *
    data_combine[, 28] + data_combine[, 18] * data_combine[, 28]
  + data_combine[, 19] * data_combine[, 28] + data_combine[, 20] *
    data_combine[, 28] + data_combine[, 21] * data_combine[, 28]
  + data_combine[, 10] * data_combine[, 29] + data_combine[, 11] *
    data_combine[, 29] + data_combine[, 12] * data_combine[, 29]
  + data_combine[, 13] * data_combine[, 29] + data_combine[, 14] *
    data_combine[, 29]
  + data_combine[, 16] * data_combine[, 29] + data_combine[, 17] *
    data_combine[, 29] + data_combine[, 18] * data_combine[, 29]
  + data_combine[, 19] * data_combine[, 29] + data_combine[, 20] *
    data_combine[, 29] + data_combine[, 21] * data_combine[, 29]
  + data_combine[, 10] * data_combine[, 30] + data_combine[, 11] *
    data_combine[, 30] + data_combine[, 12] * data_combine[, 30]
  + data_combine[, 13] * data_combine[, 30] + data_combine[, 14] *
    data_combine[, 30]
  + data_combine[, 16] * data_combine[, 30] + data_combine[, 17] *
    data_combine[, 30] + data_combine[, 18] * data_combine[, 30]
  + data_combine[, 19] * data_combine[, 30] + data_combine[, 20] *
    data_combine[, 30] + data_combine[, 21] * data_combine[, 30]
  + data_combine[, 10] * data_combine[, 31] + data_combine[, 11] *
    data_combine[, 31] + data_combine[, 12] * data_combine[, 31]
  + data_combine[, 13] * data_combine[, 31] + data_combine[, 14] *
    data_combine[, 31]
  + data_combine[, 16] * data_combine[, 31] + data_combine[, 17] *
    data_combine[, 31] + data_combine[, 18] * data_combine[, 31]
  + data_combine[, 19] * data_combine[, 31] + data_combine[, 20] *
    data_combine[, 31] + data_combine[, 21] * data_combine[, 31]
  + data_combine[, 10] * data_combine[, 32] + data_combine[, 11] *
    data_combine[, 32] + data_combine[, 12] * data_combine[, 32]
  + data_combine[, 13] * data_combine[, 32] + data_combine[, 14] *
    data_combine[, 32]
  + data_combine[, 16] * data_combine[, 32] + data_combine[, 17] *
    data_combine[, 32] + data_combine[, 18] * data_combine[, 32]
  + data_combine[, 19] * data_combine[, 32] + data_combine[, 20] *
    data_combine[, 32] + data_combine[, 21] * data_combine[, 32]
  + data_combine[, 10] * data_combine[, 9] + data_combine[, 22] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 9]
  + data_combine[, 23] * data_combine[, 9] + data_combine[, 12] *
    data_combine[, 9] + data_combine[, 24] * data_combine[, 9]
  + data_combine[, 13] * data_combine[, 9] + data_combine[, 25] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 9]
  + data_combine[, 26] * data_combine[, 9] + data_combine[, 27] *
    data_combine[, 9]
  + data_combine[, 16] * data_combine[, 9] + data_combine[, 28] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 9]
  + data_combine[, 29] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 9] + data_combine[, 30] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 9] + data_combine[, 31] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 9]
  + data_combine[, 32] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 22] + data_combine[, 12] * data_combine[, 22] *
    data_combine[, 9]
  + data_combine[, 13] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 22] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 22] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 10] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 23] + data_combine[, 12] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 23] * data_combine[, 9]
  + data_combine[, 14] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 23] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 21] * data_combine[, 23] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 24] + data_combine[, 12] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 24] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 24] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 24] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 24] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 10] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 25]
  + data_combine[, 12] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 25] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 25] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 25] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26]
  + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 26] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 27] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 27]
  + data_combine[, 12] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 27] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 27] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 27] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 28] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 28]
  + data_combine[, 12] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 28] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 28] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 29] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 29]
  + data_combine[, 12] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 29] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 29] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 29] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30]
  + data_combine[, 12] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 30] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 30] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 31] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 31]
  + data_combine[, 12] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 31] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 31] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 32] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 32]
  + data_combine[, 12] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 32] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 32] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 32] * data_combine[, 9],
  family = Gamma(link = "log"),
  data = data_combine
)

fit_w = step(fit_weight)

fit_w_up = glm(formula = data_combine[, 5] ~ data_combine[, 4] + data_combine[,9] + data_combine[, 10] + 
                 data_combine[, 13]  + 
                 data_combine[, 16]  + 
                 data_combine[,     18] + data_combine[, 20] + 
                 data_combine[, 24]+
                 data_combine[, 29]  + 
                 + data_combine[, 13]:data_combine[, 24]+ data_combine[,     13]:data_combine[, 28] + 
                 data_combine[, 14]:data_combine[,     28] 
               + data_combine[, 16]:data_combine[, 28] + data_combine[,     10]:data_combine[, 29] 
               + data_combine[, 16]:data_combine[,     29] + data_combine[, 14]:data_combine[, 32] 
              + data_combine[, 9]:data_combine[,     24] + data_combine[, 9]:data_combine[, 13] + data_combine[,     9]:data_combine[, 14] + data_combine[, 9]:data_combine[,     28] + data_combine[, 9]:data_combine[, 14]:data_combine[,     24], family = Gamma(link = "log"), data = data_combine)

summary(fit_w_up)

xlabel = 1:length(data_combine[, 1])
pred = predict(fit_w_up, data_combine, type = 'response')
Rmsew = sqrt(mean((pred - data_combine[, 5]) ^ 2))
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
lines(xlabel, sort(data_combine[, 5]), col = "blue", type = "b")
legend(
  "topleft",
  inset = .05,
  c("prediction", "raw data"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue")
)

## save the result
tidy_Gmw = tidy(fit_w_up)
write.csv(tidy_Gmw, file = "FGMw.csv")

# Save data
data_c1year = read.csv('data1year_combine.csv')
data_medium = data_combine
data_combine = data_c1year
oneYearpredict_weight = predict(fit_w_up, data_combine, type = 'response')
write.csv(
  oneYearpredict_weight,
  file = "FWeight1year.csv",
  quote = F,
  row.names = F
)
data_combine = data_medium


# ## Decision Tree Regressor
# library(rpart);library(rpart.plot)
# tree_fig = rpart(data_combine[,4]~data_combine[,9]+
#                    data_combine[,10]+data_combine[,11]+data_combine[,12]+data_combine[,13]
#                  +data_combine[,14]+data_combine[,16]+data_combine[,17]+data_combine[,18]
#                  +data_combine[,19]+data_combine[,20]+data_combine[,21]+data_combine[,22]+data_combine[,23]
#                  +data_combine[,24]+data_combine[,25]+data_combine[,26]+data_combine[,27]+data_combine[,28]
#                  +data_combine[,29]+data_combine[,30]+data_combine[,31]+data_combine[,32]
#           ,data_combine)
# rpart.plot(tree_fig)
# # 10 fold cross validation
# Z = 10;
# n = nrow(data_combine)
# zz1 = 1:n
# zz2 = rep(1:Z,ceiling(n/Z))[1:n]
# zz2 = sample(zz2,n)
#
# NMSE = rep(0,Z);NMSE0 = NMSE
# for (i in 1:Z) {
#   m = zz1[zz2 == i]
#   x = data_combine[,9]+
#     data_combine[,10]+data_combine[,11]+data_combine[,12]+data_combine[,13]
#   +data_combine[,14]+data_combine[,15]+data_combine[,16]+data_combine[,17]+data_combine[,18]
#   +data_combine[,19]+data_combine[,20]+data_combine[,21]+data_combine[,22]+data_combine[,23]
#   +data_combine[,24]+data_combine[,25]+data_combine[,26]+data_combine[,27]+data_combine[,28]
#   +data_combine[,29]+data_combine[,30]+data_combine[,31]+data_combine[,32]+data_combine[,33]
#   y = data_combine[,4]
#   a = rpart(y~x,data_combine[-m,])
#
#   y0 = predict(a,data_combine)[-m]
#   y1 = predict(a,data_combine)[m]
#   NMSE0[i] = mean((data_combine[-m,4]-y0)^2)/mean((data_combine[-m,4]-mean(data_combine[-m,4]))^2)
#   NMSE[i] = mean((data_combine[m,4]-y1)^2)/mean((data_combine[m,4]-mean(data_combine[m,4]))^2)
# }
#
# MNMSE0 = mean(NMSE0)
# MNMSE = mean(NMSE)
#
# #bagging regression
# library(ipred)
# b = bagging(data_combine[,4]~data_combine[,9]+
#               data_combine[,10]+data_combine[,11]+data_combine[,12]+data_combine[,13]
#             +data_combine[,14]+data_combine[,16]+data_combine[,17]+data_combine[,18]
#             +data_combine[,19]+data_combine[,20]+data_combine[,21]+data_combine[,22]+data_combine[,23]
#             +data_combine[,24]+data_combine[,25]+data_combine[,26]+data_combine[,27]+data_combine[,28]
#             +data_combine[,29]+data_combine[,30]+data_combine[,31]+data_combine[,32]
#             ,data_combine)
#
# NMSE = rep(0,Z);NMSE0 = NMSE
# for (i in 1:Z) {
#   m = zz1[zz2 == i]
#   x = data_combine[,9]+
#     data_combine[,10]+data_combine[,11]+data_combine[,12]+data_combine[,13]
#   +data_combine[,14]+data_combine[,15]+data_combine[,16]+data_combine[,17]+data_combine[,18]
#   +data_combine[,19]+data_combine[,20]+data_combine[,21]+data_combine[,22]+data_combine[,23]
#   +data_combine[,24]+data_combine[,25]+data_combine[,26]+data_combine[,27]+data_combine[,28]
#   +data_combine[,29]+data_combine[,30]+data_combine[,31]+data_combine[,32]+data_combine[,33]
#   y = data_combine[,4]
#   a = bagging(y~x,data_combine[-m,])
#
#   y0 = predict(a,data_combine)[-m]
#   y1 = predict(a,data_combine)[m]
#   NMSE0[i] = mean((data_combine[-m,4]-y0)^2)/mean((data_combine[-m,4]-mean(data_combine[-m,4]))^2)
#   NMSE[i] = mean((data_combine[m,4]-y1)^2)/mean((data_combine[m,4]-mean(data_combine[m,4]))^2)
# }
# MNMSE0 = mean(NMSE0)
# MNMSE = mean(NMSE)
#
# # Random Forest
# library(randomForest)
# set.seed(42)
# a = randomForest(data_combine[,4]~data_combine[,9]+
#                    data_combine[,10]+data_combine[,11]+data_combine[,12]+data_combine[,13]
#                  +data_combine[,14]+data_combine[,16]+data_combine[,17]+data_combine[,18]
#                  +data_combine[,19]+data_combine[,20]+data_combine[,21]+data_combine[,22]+data_combine[,23]
#                  +data_combine[,24]+data_combine[,25]+data_combine[,26]+data_combine[,27]+data_combine[,28]
#                  +data_combine[,29]+data_combine[,30]+data_combine[,31]+data_combine[,32]
#                  ,data_combine,mtry = 3,importance = T,importanceSD=T)
# #par(mfrow = c(1,2))
# barplot(a$importance[,1])
# title('Variable importance according to accuracy')
# barplot(a$importance[,2])
# title('Variable importance according to MSE')
# barplot(a$importanceSD)
# title('Variable importance according to Standard errors')
# par(mfrow = c(1,1))
#
# NMSE = rep(0,Z);NMSE0 = NMSE
# for (i in 1:Z) {
#   m = zz1[zz2 == i]
#   n0 = n - length(m);n1 = length(m)
#   x = data_combine[,9]+
#     data_combine[,10]+data_combine[,11]+data_combine[,12]+data_combine[,13]
#   +data_combine[,14]+data_combine[,15]+data_combine[,16]+data_combine[,17]+data_combine[,18]
#   +data_combine[,19]+data_combine[,20]+data_combine[,21]+data_combine[,22]+data_combine[,23]
#   +data_combine[,24]+data_combine[,25]+data_combine[,26]+data_combine[,27]+data_combine[,28]
#   +data_combine[,29]+data_combine[,30]+data_combine[,31]+data_combine[,32]+data_combine[,33]
#   y = data_combine[,4]
#   a = randomForest(y~x,data_combine[-m,],mtry = 20,importance= T)
#
#   y0 = predict(a,data_combine)[-m]
#   y1 = predict(a,data_combine)[m]
#   NMSE0[i] = mean((data_combine[-m,4]-y0)^2)/mean((data_combine[-m,4]-mean(data_combine[-m,4]))^2)
#   NMSE[i] = mean((data_combine[m,4]-y1)^2)/mean((data_combine[m,4]-mean(data_combine[m,4]))^2)
# }
# MNMSE0 = mean(NMSE0)
# MNMSE = mean(NMSE)

## model buildng for width

# data_unknowndes = read.csv('data_unknowDes.csv')
# names(data_unknowndes) = c('destinations','length','width','height','volume','weight','commodity','fragile','iflammable','fits_rate','types')
# # deal with categorical variables
# # dest_col = model.matrix(~destinations, data_unknowndes)[,-1]
# commod_col = model.matrix(~commodity, data_unknowndes)[,-1]
#
# # combine new columns and delete old categorical columns
# data_unknown_combine = cbind(data_unknowndes[,-c(1,7)],commod_col)
# write.csv(data_unknown_combine,file="dataunknown_combine.csv",quote=F,row.names = F)

##unknown length
data_ud = read.csv('dataunknown_combine.csv')
data_medium = data_combine
data_combine = data_ud
udpredict_length = predict(fit_l, data_combine, type = 'response')
write.csv(
  udpredict_length,
  file = "FlengthUD.csv",
  quote = F,
  row.names = F
)
data_combine = data_medium

##unknown Volume
data_ud = read.csv('dataunknown_combine.csv')
data_medium = data_combine
data_combine = data_ud
udpredict_volume = predict(fit_v_up, data_combine, type = 'response')
write.csv(
  udpredict_volume,
  file = "FvolumeUD.csv",
  quote = F,
  row.names = F
)
data_combine = data_medium

## unknown Width
data_ud = read.csv('dataunknown_combine.csv')
data_medium = data_combine
data_combine = data_ud
udpredict_width = predict(fit_wd_up, data_combine, type = 'response')
write.csv(
  udpredict_width,
  file = "FwidthUD.csv",
  quote = F,
  row.names = F
)
data_combine = data_medium


## unknown Weight
data_ud = read.csv('dataunknown_combine.csv')
data_medium = data_combine
data_combine = data_ud
udpredict_weight = predict(fit_w_up, data_combine, type = 'response')
write.csv(
  udpredict_weight,
  file = "FweightUD.csv",
  quote = F,
  row.names = F
)
data_combine = data_medium

## histogram

fit_length_linear = lm(
  data_combine[, 1] ~  data_combine[, 9] +
    data_combine[, 10] + data_combine[, 11] + data_combine[, 12] +
    data_combine[, 13]
  + data_combine[, 14] + data_combine[, 16] + data_combine[, 17] +
    data_combine[, 18]
  + data_combine[, 19] + data_combine[, 20] + data_combine[, 21] +
    data_combine[, 22] + data_combine[, 23]
  + data_combine[, 24] + data_combine[, 25] + data_combine[, 26] +
    data_combine[, 27] + data_combine[, 28]
  + data_combine[, 29] + data_combine[, 30] + data_combine[, 31] +
    data_combine[, 32]
  + data_combine[, 10] * data_combine[, 22] + data_combine[, 11] *
    data_combine[, 22] + data_combine[, 12] * data_combine[, 22]
  + data_combine[, 13] * data_combine[, 22] + data_combine[, 14] *
    data_combine[, 22]
  + data_combine[, 16] * data_combine[, 22] + data_combine[, 17] *
    data_combine[, 22] + data_combine[, 18] * data_combine[, 22]
  + data_combine[, 19] * data_combine[, 22] + data_combine[, 20] *
    data_combine[, 22] + data_combine[, 21] * data_combine[, 22]
  + data_combine[, 10] * data_combine[, 23] + data_combine[, 11] *
    data_combine[, 23] + data_combine[, 12] * data_combine[, 23]
  + data_combine[, 13] * data_combine[, 23] + data_combine[, 14] *
    data_combine[, 23]
  + data_combine[, 16] * data_combine[, 23] + data_combine[, 17] *
    data_combine[, 23] + data_combine[, 18] * data_combine[, 23]
  + data_combine[, 19] * data_combine[, 23] + data_combine[, 20] *
    data_combine[, 23] + data_combine[, 21] * data_combine[, 23]
  + data_combine[, 10] * data_combine[, 24] + data_combine[, 11] *
    data_combine[, 24] + data_combine[, 12] * data_combine[, 24]
  + data_combine[, 13] * data_combine[, 24] + data_combine[, 14] *
    data_combine[, 24]
  + data_combine[, 16] * data_combine[, 24] + data_combine[, 17] *
    data_combine[, 24] + data_combine[, 18] * data_combine[, 24]
  + data_combine[, 19] * data_combine[, 24] + data_combine[, 20] *
    data_combine[, 24] + data_combine[, 21] * data_combine[, 24]
  + data_combine[, 10] * data_combine[, 25] + data_combine[, 11] *
    data_combine[, 25] + data_combine[, 12] * data_combine[, 25]
  + data_combine[, 13] * data_combine[, 25] + data_combine[, 14] *
    data_combine[, 25]
  + data_combine[, 16] * data_combine[, 25] + data_combine[, 17] *
    data_combine[, 25] + data_combine[, 18] * data_combine[, 25]
  + data_combine[, 19] * data_combine[, 25] + data_combine[, 20] *
    data_combine[, 25] + data_combine[, 21] * data_combine[, 25]
  + data_combine[, 10] * data_combine[, 26] + data_combine[, 11] *
    data_combine[, 26] + data_combine[, 12] * data_combine[, 26]
  + data_combine[, 13] * data_combine[, 26] + data_combine[, 14] *
    data_combine[, 26]
  + data_combine[, 16] * data_combine[, 26] + data_combine[, 17] *
    data_combine[, 26] + data_combine[, 18] * data_combine[, 26]
  + data_combine[, 19] * data_combine[, 26] + data_combine[, 20] *
    data_combine[, 26] + data_combine[, 21] * data_combine[, 26]
  + data_combine[, 10] * data_combine[, 27] + data_combine[, 11] *
    data_combine[, 27] + data_combine[, 12] * data_combine[, 27]
  + data_combine[, 13] * data_combine[, 27] + data_combine[, 14] *
    data_combine[, 27]
  + data_combine[, 16] * data_combine[, 27] + data_combine[, 17] *
    data_combine[, 27] + data_combine[, 18] * data_combine[, 27]
  + data_combine[, 19] * data_combine[, 27] + data_combine[, 20] *
    data_combine[, 27] + data_combine[, 21] * data_combine[, 27]
  + data_combine[, 10] * data_combine[, 28] + data_combine[, 11] *
    data_combine[, 28] + data_combine[, 12] * data_combine[, 28]
  + data_combine[, 13] * data_combine[, 28] + data_combine[, 14] *
    data_combine[, 28]
  + data_combine[, 16] * data_combine[, 28] + data_combine[, 17] *
    data_combine[, 28] + data_combine[, 18] * data_combine[, 28]
  + data_combine[, 19] * data_combine[, 28] + data_combine[, 20] *
    data_combine[, 28] + data_combine[, 21] * data_combine[, 28]
  + data_combine[, 10] * data_combine[, 29] + data_combine[, 11] *
    data_combine[, 29] + data_combine[, 12] * data_combine[, 29]
  + data_combine[, 13] * data_combine[, 29] + data_combine[, 14] *
    data_combine[, 29]
  + data_combine[, 16] * data_combine[, 29] + data_combine[, 17] *
    data_combine[, 29] + data_combine[, 18] * data_combine[, 29]
  + data_combine[, 19] * data_combine[, 29] + data_combine[, 20] *
    data_combine[, 29] + data_combine[, 21] * data_combine[, 29]
  + data_combine[, 10] * data_combine[, 30] + data_combine[, 11] *
    data_combine[, 30] + data_combine[, 12] * data_combine[, 30]
  + data_combine[, 13] * data_combine[, 30] + data_combine[, 14] *
    data_combine[, 30]
  + data_combine[, 16] * data_combine[, 30] + data_combine[, 17] *
    data_combine[, 30] + data_combine[, 18] * data_combine[, 30]
  + data_combine[, 19] * data_combine[, 30] + data_combine[, 20] *
    data_combine[, 30] + data_combine[, 21] * data_combine[, 30]
  + data_combine[, 10] * data_combine[, 31] + data_combine[, 11] *
    data_combine[, 31] + data_combine[, 12] * data_combine[, 31]
  + data_combine[, 13] * data_combine[, 31] + data_combine[, 14] *
    data_combine[, 31]
  + data_combine[, 16] * data_combine[, 31] + data_combine[, 17] *
    data_combine[, 31] + data_combine[, 18] * data_combine[, 31]
  + data_combine[, 19] * data_combine[, 31] + data_combine[, 20] *
    data_combine[, 31] + data_combine[, 21] * data_combine[, 31]
  + data_combine[, 10] * data_combine[, 32] + data_combine[, 11] *
    data_combine[, 32] + data_combine[, 12] * data_combine[, 32]
  + data_combine[, 13] * data_combine[, 32] + data_combine[, 14] *
    data_combine[, 32]
  + data_combine[, 16] * data_combine[, 32] + data_combine[, 17] *
    data_combine[, 32] + data_combine[, 18] * data_combine[, 32]
  + data_combine[, 19] * data_combine[, 32] + data_combine[, 20] *
    data_combine[, 32] + data_combine[, 21] * data_combine[, 32]
  + data_combine[, 10] * data_combine[, 9] + data_combine[, 22] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 9]
  + data_combine[, 23] * data_combine[, 9] + data_combine[, 12] *
    data_combine[, 9] + data_combine[, 24] * data_combine[, 9]
  + data_combine[, 13] * data_combine[, 9] + data_combine[, 25] *
    data_combine[, 9] + data_combine[, 14] * data_combine[, 9]
  + data_combine[, 26] * data_combine[, 9] + data_combine[, 27] *
    data_combine[, 9]
  + data_combine[, 16] * data_combine[, 9] + data_combine[, 28] *
    data_combine[, 9] + data_combine[, 17] * data_combine[, 9]
  + data_combine[, 29] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 9] + data_combine[, 30] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 9] + data_combine[, 31] *
    data_combine[, 9] + data_combine[, 20] * data_combine[, 9]
  + data_combine[, 32] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 22] + data_combine[, 12] * data_combine[, 22] *
    data_combine[, 9]
  + data_combine[, 13] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 22] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 22] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 22] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 22] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 22] * data_combine[, 9]
  + data_combine[, 10] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 23] + data_combine[, 12] * data_combine[, 23] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 23] * data_combine[, 9]
  + data_combine[, 14] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 16] * data_combine[, 23] * data_combine[, 9] + data_combine[, 17] *
    data_combine[, 23] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 23] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 23] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 23] * data_combine[, 9] + data_combine[, 21] * data_combine[, 23] *
    data_combine[, 9]
  + data_combine[, 10] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 11] * data_combine[, 24] + data_combine[, 12] * data_combine[, 24] *
    data_combine[, 9] + data_combine[, 13] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 14] * data_combine[, 24] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 24] * data_combine[, 9] + data_combine[, 18] *
    data_combine[, 24] * data_combine[, 9]
  + data_combine[, 19] * data_combine[, 24] * data_combine[, 9] +
    data_combine[, 20] * data_combine[, 24] * data_combine[, 9] + data_combine[, 21] *
    data_combine[, 24] * data_combine[, 9] + data_combine[, 10] * data_combine[, 25] *
    data_combine[, 9] + data_combine[, 11] * data_combine[, 25]
  + data_combine[, 12] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 25] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 25] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 25] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 25] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 25] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 26] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 26]
  + data_combine[, 12] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 26] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 26] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 26] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 26] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 26] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 27] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 27]
  + data_combine[, 12] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 27] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 27] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 27] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 27] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 27] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 28] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 28]
  + data_combine[, 12] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 28] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 28] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 28] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 28] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 28] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 29] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 29]
  + data_combine[, 12] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 29] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 29] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 29] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 29] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 29] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 30] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 30]
  + data_combine[, 12] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 30] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 30] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 30] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 30] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 30] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 31] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 31]
  + data_combine[, 12] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 31] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 31] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 31] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 31] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 31] * data_combine[, 9] +
    data_combine[, 10] * data_combine[, 32] * data_combine[, 9] + data_combine[, 11] *
    data_combine[, 32]
  + data_combine[, 12] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 13] * data_combine[, 32] * data_combine[, 9] + data_combine[, 14] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 16] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 17] * data_combine[, 32] * data_combine[, 9]
  + data_combine[, 18] * data_combine[, 32] * data_combine[, 9] +
    data_combine[, 19] * data_combine[, 32] * data_combine[, 9] + data_combine[, 20] *
    data_combine[, 32] * data_combine[, 9]
  + data_combine[, 21] * data_combine[, 32] * data_combine[, 9],
  data = data_combine
)
## Length plot
fit_l_linear = step(fit_length_linear)
xlabel = 1:length(data_combine[, 1])
pred = predict(fit_l_linear, data_combine, type = 'response')
#Rmsew = sqrt(mean((pred - data_combine[, 5]) ^ 2))
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
lines(xlabel, sort(data_combine[, 1]), col = "blue", type = "b")
legend(
  "topleft",
  inset = .05,
  c("prediction", "raw data"),
  lty = c(1, 2),
  pch = c(4, 1),
  col = c("red", "blue")
)

data_c1year = read.csv('data1year_combine.csv')
data_medium = data_combine
data_combine = data_c1year
oneYearpredict = predict(fit_length_linear, data_combine, type = 'response')
