library(ggplot2)
con<-file(file.choose(),"r")
count<-0
string<-""
info<-""
header<-""
strdata<-""

while(length(str<-readLines(con,n=1,warn=F))>0){
  count<-count+1
  if(count<3){
    info<-paste0(info,str)
  }
  #Save header files 
  if(count==5){
    header<-strsplit(str,",")[[1]]
  }
  
  if(count>5){
    #if(grep1(" ",str))break
    if (gsub(pattern=",", x=str, replacement="") == "") break
    strdata<-paste0(strdata,str,"\n")
    
  }
  
}
close(con)
head(strdata)
header
tdata<-read.table(textConnection(strdata),sep=",",header=F)
head(tdata)
names(tdata)<-header
head(tdata)

#Split date into start-end date
tdata$StartDate <- as.Date(sapply(strsplit(as.character(tdata[,1]), " - "), `[`, 1))
tdata$EndDate <- as.Date(sapply(strsplit(as.character(tdata[,1]), " - "), `[`, 2))
tdata$Year <- sapply(strsplit(as.character(tdata$StartDate), "-"), `[`, 1)

head(tdata)
tdata[,1]=NULL
head(tdata)
tdata<-tdata[c("StartDate","EndDate","Year","game of thrones","How I Met Your Mother","The Flash")]
head(tdata)
names(tdata)<-c("StartDate","EndDate","Year","GOT","HIMYM","TF")
head(tdata)

#Exploring and visualizing data now
plot(tdata$StartDate,tdata$HIMYM,type='l',col="Black",main="What's Trending!",xlab="Date",ylab = "Trending")
lines(tdata$StartDate,tdata$GOT,col="Blue")
lines(tdata$StartDate,tdata$TF,col="Red")
legend("topleft",legend = c("HIMYM","GOT","The Flash"),fill=c("Black","Blue","Red"),bty = "n")

attach(tdata)

ggplot(tdata,aes(x=Year,y=GOT))+geom_boxplot()+ggtitle("Game of Thrones")

ggplot(tdata,aes(x=Year,y=HIMYM))+geom_boxplot()+ggtitle("How I Met Your Mother")

ggplot(tdata,aes(x=Year,y=TF))+geom_boxplot()+ggtitle("The Flash")

ggplot(tdata,aes(x=Year))+geom_point(aes(y=GOT,colour="GOT"))+
  geom_point(aes(y=HIMYM,colour="HIMYM"))+
  geom_point(aes(y=TF,colour="TF"))+
  stat_smooth(aes(y=GOT,group=1,colour="GOT"),method=lm, formula = y ~ poly(x,3), level=0.95)+
  stat_smooth(aes(y=HIMYM,group=1,colour="HIMYM"),method=lm, formula = y ~ poly(x,3), level=0.95)+
  stat_smooth(aes(y=TF,group=1,colour="TF"),method=lm, formula = y ~ poly(x,3), level=0.95)+
  xlab("Year")+ylab("Trending")+
  ggtitle("Whats trending!")+
  scale_color_manual("Search Terms",breaks=c("GOT","HIMYM","TF"),values = c("blue","black","red"))







