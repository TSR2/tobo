library(plyr)
library(dplyr)
library(magrittr)

data1=read.table('c:/Users/TSR/Desktop/tobo/掏寶寄件人.txt',stringsAsFactors =F)
data1
names(data1)=c('date','name','local')

data1 %>% str
data1$date=data1$date %>% as.Date()

########################一個月內沒寄件的名字
safename=function(x){
  a=data1 %>% arrange(desc(date)) %>% group_by(name) %>% do(h=Sys.Date()-.$date[1])
  a1=a$h %>% unlist()
  a$name[a1>34]
}

######################halfcount 內do所用函數
subhalf=function(x){
  x1=x %>% filter(date>(Sys.Date()-190))
  dim(x1)[1]
}

#######################計算半年內寄件次數
halfcount=function(x){
  a=data1 %>% arrange(desc(date)) %>% group_by(name) %>% do(h=subhalf(.))
  data.frame(name=a$name,count=a$h %>% unlist %>% as.numeric() )
}

safename(data1)
halfcount(data1)

