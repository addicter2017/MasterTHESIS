# load the data
data = read.csv('data_all.csv')

# rename column names
names(data) = c('destinations','length','width','height','volume','weight','commodity','fragile','iflammable','fits_rate','types')

# Select destinations as one factor
des_frame = data[,c('destinations','length')]

head(des_frame)
library('ggpubr')
ggline(des_frame,x = 'destinations',y = 'length',add = c('mean_se','jitter'))

# nomial test
des_frame$destinations = as.numeric(des_frame$destinations)
group_data = split(des_frame,des_frame$destinations)
unlist(lapply(group_data,function(x){
  shapiro.test(unlist(x))$p.value
}))

# Select types as one factor
type_frame = data[,c('types','length')]
ggline(type_frame,x = 'types',y = 'length',add = c('mean_se','jitter'))
group_data = split(type_frame,type_frame$types)
unlist(lapply(group_data,function(x){
  shapiro.test(unlist(x))$p.value
}))

summary(aov(length~types,data = type_frame))

# Select commodity types as one factor
com_frame = data[,c('commodity','length')]
ggline(com_frame,x = 'commodity',y = 'length',add = c('mean_se','jitter'))
group_data = split(com_frame[,2],com_frame[,1])
unlist(rapply(group_data,function(x){
  shapiro.test(unlist(x))$p.value
}))
for (i  in 1:length(group_data) ){
  #shapiro.test(unlist(group_data[i]))
  print(unlist(group_data[i]))
}
summary(aov(volume~commodity,data = com_frame))

  
}
