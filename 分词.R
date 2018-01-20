1.中文分词

1.1包加载与引擎设置

install.packages("jiebaR")#安装
library(jiebaR)#加载包
cutter=worker()#设置分词引擎
content="糖葫芦冰淇淋披萨烧烤火锅烤鱼棉花糖巧克力红烧肉炸臭豆腐上校鸡块烤面筋麻辣鸭脖酸辣粉鳕鱼盖浇饭手抓饼瘦肉丸布丁巧克力奶茶寿司肉夹馍炒肝麦乐鸡烤鸡翅泡菜软心布丁乳酪烤肉火锅红烧排骨鸭脖土豆烧牛肉鱼丸炒拉条子大白兔奶糖驴肉火烧鸡米花酸菜鱼皮蛋瘦肉粥冰糖葫芦糖炒栗子豌豆黄烤红薯炸鸡"
segWords<-segment(content,cutter)#对文本进行分词处理
segWords

1.2去除停止词、数字和英文
f<-readLines('stopwords.txt')###读取停止词
stopwords<-c(NULL)
for(i in 1:length(f))
{
  stopwords[i]<-f[i]
}
segWords<-filter_segment(segWords,stopwords)#去除中文停止词

segWords<-gsub("[0-9a-zA-Z]+?","",segWords)###去除数字和英文
library(stringr)#加载stringr包
segWords<-str_trim(segWords)#去除空格
2.构建词频表
library(plyr)
tableWord<-count(seg_words)##形成词频表,tableWord是数据框格式

3.词云展现
library(wordcloud)
windowsFonts(myFont=windowsFont("华文彩云")) ##使用华文彩云字体
wordcloud(tableWord[,1],tableWord[,2],random.order=F,col= rainbow(length(wordFreq)),family="myFont")##参数应该能看懂吧