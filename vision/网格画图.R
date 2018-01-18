install.packages("ggmap")
install.packages("mapproj")
install.packages("ggplot2")
install.packages("maptools")
install.packages("devtools")
install.packages("maps")
library(ggmap)  
library(mapproj)  
library(DBI)  
library(RMySQL)  
library(ggplot2)
library(maptools)
library(devtools)  
library(maps)
library("mapdata") 
install_github('lchiffon/REmap')  
#REmap这个包需要从github上下载，install_github这个函数需要加载devtools后使用
#使用install_github("A","B")，其中A表示要从GitHub上安装的软件包的名称，B表示开发该程序包的作者，由于install_github默认B是“hadley”（即作者是“hadley”）。
#举个例子，mgarch包是vst开发的，则是install_github("mgarch","vst")。运行之后，即可实现安装。
library(REmap)
install_github('badbye/baidumap')
library(baidumap)

demo(REmap)

x <- readShapePoly('bou2_4p.shp')
china.map <- fortify(x)

remapC(data,  
       maptype = 'china',  
       markLineData = NULL,  
       markPointData = NULL,  
       color = c('#1e90ff','#f0ffff'),  
       theme = get_theme("Bright"),  
       title = "",  
       subtitle = "",  
       markLineTheme = markLineControl(),  
       markPointTheme = markPointControl(),  
       geoData = NA,  
       mindata = NA,  
       maxdata = NA)

guangzhou<-read.csv("guangzhong.csv")
gezi<-read.csv("lati_longi.csv")
length(gezi[,1])
lie<-c(3,2,1)
ant<-guangzhou[,lie]
length(ant[,1])
mode(ant)

names(ant)<-c("lat","lon","name")

ant<-ant[1:17000,]

data = data.frame(country = mapNames('广东'),value = 50*sample(21)+200,stringsAsFactors = F)  
data$value<-as.numeric(data$value)  
pointData = data.frame(ant$name,  
                       color = c(rep("orange",length(ant[,1]))))
                                 
remapC(data,maptype = '广东',color = 'skyblue',  
       markPointData = pointData,  
       markPointTheme = markPointControl(symbol = 'pin',  
                                         symbolSize = .11,  
                                         effect = F),  
       geoData = ant)

head(ant)
set --max-ppsize = 500000 and options(expressions=500000) 
#河南气象方案底图------------------------------
value = c(11,10,9,9,9,8,8,7,9,7,6,7,8,7,6,9,6)
  
data = data.frame(country = mapNames('河南'),value = value ,stringsAsFactors = F)  
data$value<-as.numeric(data$value)  
pointData = data.frame(geoData$name,  
                       color = c(rep("orange",20),  
                                 rep("red",10)))  
remapC(data,maptype = '河南',color = 'skyblue',  
       markPointTheme = markPointControl(symbol = 'pin',  
                                         symbolSize = 5,  
                                         effect = F),  
       geoData = geoData)
#源代码------------------------------
data = data.frame(country = mapNames('河南'),value = 50*sample(17)+200,stringsAsFactors = F)  
#上一行代码为各地块的数据，会影响颜色深浅，数字需要与地块数对应(17)
data$value<-as.numeric(data$value)  
pointData = data.frame(geoData$name,  
                       color = c(rep("orange",20),  
                                 rep("red",10)))  
remapC(data,maptype = '河南',color = 'skyblue',  
       markPointTheme = markPointControl(symbol = 'pin',  
                                         symbolSize = 5,  
                                         effect = F),  
       geoData = geoData)


data = data.frame(country = mapNames('广东'),value = 50*sample(11)+200,stringsAsFactors = F)  
data$value<-as.numeric(data$value)  
remapC(data,maptype = '广东',color = 'skyblue') 


#地图基础上添加路径  
#remapC(data,maptype = 'china',color = c('#1e90ff','#f0ffff'),  
#       theme = get_theme("Bright"),  
#       title = "",subtitle = "",  
#       mindata = NA,maxdata = NA,geoData = NA,  
#       #添加路径数据，格式  
#       markLineData = NA,markPointData = NA,  
#       markLineTheme = markLineControl(),markPointTheme = markPointControl())  
markline_control<-markLineControl(symbolSize = c(0,0),  #是否有箭头  
                                  smooth = F,  
                                  smoothness = 0,       #退化为直线  
                                  effect = F,           #炫光效果  
                                  lineWidth = 1,        #标线宽度  
                                  lineType = "solid")   #标线样式 solid dotted dashed  
##markLineControl 的color 默认为随机颜色,设置一个颜色会取为固定颜色  
markpoint_control<-markPointControl(symbol = "Circle",  
                                    symbolSize = 10,    #标点大小  
                                    effect=T,           #炫光效果  
                                    effectType="scale") #炫光方式：’scale’放大,’bounce’跳动  
#symbol:  
#‘circle’,‘emptyCircle’,圆,空心圆  
#‘rectangle’,‘emptyRectangle’,方块,空心方块  
#‘triangle’,‘emptyTriangle’,三角,空心三角  
#‘diamond’,‘emptyDiamond’,钻石,空心钻石  
#‘heart’心形,’droplet’,水滴  
#‘pin’,POI标注,’arrow’箭头, ’star’五角星  
#或者使用’image://http://….’来引用一个图片,如symbol = "image://http://lchiffon.github.io/assets/images/df_logo.jpg",  
#此外对markLineData下设置symbol变量会覆盖该颜色  

remapC(choropleth_map_data,  
       color = 'orange',  
       markLineData = migrate_map_data2,  
       markPointData = migrate_map_data2[,2],  
       markLineTheme=markline_control,  
       markPointTheme=markpoint_control)  



get_city_coord("Beijing")  
get_city_coord("beijing")  
get_city_coord("北京")  
get_city_coord( c("Beijing","Shanghai","Guangzhou")) #报错，只能填写单个，不能是向量  
get_geo_position (c("beijing","Shanghai","广州","新疆"))  

demo(REmap::remapDemo)

#-------------------------------------
plot(x,fg=gray(924:0/924));

getColor = function(mapdata, provname, provcol, othercol){
  f = function(x, y) ifelse(x %in% y, which(y == x), 0);
  colIndex = sapply(mapdata$att.data$NAME, f, provname);
  fg = c(othercol, provcol)[colIndex + 1];
  return(fg);
}

provname=c("北京市","天津市","河北省","山西省","内蒙古自治区",
           "辽宁省","吉林省","黑龙江省","上海市","江苏省",
           "浙江省","安徽省","福建省","江西省","山东省",
           "河南省","湖北省","湖南省","广东省",
           "广西壮族自治区","海南省","重庆市","四川省","贵州省",
           "云南省","西藏自治区","陕西省","甘肃省","青海省",
           "宁夏回族自治区","新疆维吾尔自治区","台湾省",
           "香港特别行政区");
pop=c(1633,1115,6943,3393,2405,4298,2730,3824,1858,7625,
      5060,6118,3581,4368,9367,9360,5699,6355,9449,
      4768,845,2816,8127,3762,4514,284,3748,2617,
      552,610,2095,2296,693);
provcol=rgb(red=1-pop/max(pop)/2,green=1-pop/max(pop)/2,blue=0);
plot(x,fg=getColor(x,provname,provcol,"white"),xlab="",ylab="");


origin<-c("xian","shanghai","chongqing","chengdu")
destination<-c("shanghai","chongqing","chengdu","xian")
#将上面这两列数据存储在一个数据框里面
dat = data.frame(origin,destination)
out = remap(dat,title = "REmap",subtitle = "theme:Dark")
plot(out)


# ------------------------------------
con=dbConnect(MySQL(),user='root',password='123',dbname='cmcc',host='localhost')  
con  = read.csv()
zhongguancun=dbGetQuery(con,'select cell_name,longtitude,latitude from 8_15_table_9 where DEPT_NAME="中关村片区1" or DEPT_NAME="中关村片区2"')#从数据库获取中关村的基站经纬度  
#jingweidu=read.csv('./cmcc_doc/基站清单1.txt',header=T,colClasses=c('NULL','factor',rep(list('NULL'),4),'numeric','numeric',rep(list('NULL'),21)))  
#classes=sapply(jingweidu,class)  
names(jingweidu)=c('name','longitude','latitude')  
#google给我中关村的公路地图数据吧   
bjmap <- qmap('zhongguancun', zoom = 12, maptype = 'roadmap')   
bjmap <- bjmap + geom_point(data = zhongguancun, aes(x = longtitude, y = latitude), size = 5)+  
  geom_path(aes(x = longtitude, y = latitude),  colour = 'red', size = 1.5,data = zhongguancun[1:5,], lineend = 'round')#画出数据中前五个地点之间的线段图  
bjmap 




x <- readShapePoly('bou2_4p.shp')
china.map <- fortify(x)


long <- c(110.02,105.52,118.7,124.52,129.10,121.25)
lat <- c(30.48,34.09,41.43,42.21,46.32,49.34)
plot(china_map)

ggplot(china.map,aes_q(x=long,y=lat))+
  geom_polygon(aes_q(,group=group),fill='white',colour='black')+
  geom_point(aes_q(x=lon,y=lan),colour = 'black',size=2)
