#Mac的数据地址
setwd("~/Documents/学习/共享/OneDrive/数据/武汉工行")

bianjie<-read.csv('wuhan_bianjie.csv',header = T)

#求经纬度各自的最大、最小值
lat_max<-max(bianjie[1])
lat_min<-min(bianjie[1])
lon_max<-max(bianjie[2])
lon_min<-min(bianjie[2])

plot(bianjie)
plot(bianjie[1],bianjie[2])
pairs(bianjie[1],bianjie[2])

range(bianjie[1])
#经度差113.7077 115.0858
range(bianjie[2])
#维度差29.97290 31.36705
heng_dis30<-c(0.00520)
heng_dis30<-c(0.00525)
shu_dis<-c(0.00450)
  
#30.45000 - 30.44550 = 0.00450 = 0.5km（115经度下计算）
#115.10000 - 115.09480 = 0.00520 = 0.5km （30维度下计算）
#115.10000 - 115.09475 = 0.00525 = 0.5km （31维度下计算）

