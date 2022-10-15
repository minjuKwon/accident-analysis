
library(ggplot2)
library(ggmap)
register_google(key="AIzaSyCUNz0wLm3amrseiT1RyDtD3sOnxsphGXg")
library(treemap)

accident <- read.csv("?��로교?��공단_?��군구�? ?���? 교통?���? ?���?_20201231.csv")
head(accident)
accident <- data.frame(accident)

#부?��?��?��
for(i in 1:nrow(accident)){
  accident[i,9] <- apply(accident[i,6:8],1,sum)
}
names(accident)[9] <- c("부?��?��?��")
head(accident)


#?��별사고건?��?��그래?��
a.month <- aggregate(accident[,c(4,5,9)],
                     by=list(?��=accident$발생?��),
                     FUN=sum)
names(a.month)[2] <- c("총사고건?��")
a.month
month <- a.month[,1]
accidentToatal <- a.month[,2]
deathtoll <- a.month[,3]
injuries<- a.month[,4]
ggplot(data=a.month,aes(x=month),xlim=c(1,12))+
    geom_line(aes(y=accidentToatal,colour="accidentToatal"),size=1)+
  geom_line(aes(y=deathtoll,colour="deathtoll"),size=1)+
  geom_line(aes(y=injuries,col="injuries"),size=1)+
  ggtitle("?���? 교통?���?")+
  xlab("?��")+
  ylab("교통?���?")+
  scale_x_continuous(breaks = 1:12,labels=month)+
  theme(plot.title = element_text(color="black",size=14,face="bold"))+
  scale_color_manual(name="y",values =c("accidentToatal"="black",
                                        "deathtoll"="red","injuries"="blue"))

#?��?��,경기,??��? 3?��?��고건?��막�?�그래?��
a.deagu <- accident[accident$?��?��=="??��?",]
a.deaguMeans <- aggregate(a.deagu[,4],
                      by=list(?��=a.deagu$발생?��),
                      FUN=mean)
names(a.deaguMeans)[2] <- c("Deagu")

a.seoul <- accident[accident$?��?��=="?��?��",]
a.seoulMeans <- aggregate(a.seoul[,4],
                          by=list(?��=a.seoul$발생?��),
                          FUN=mean)
names(a.seoulMeans)[2] <- c("Seoul")

a.gyeonggi <- accident[accident$?��?��=="경기",]
a.gyeonggiMeans <- aggregate(a.gyeonggi[,4],
                          by=list(?��=a.gyeonggi$발생?��),
                          FUN=mean)
names(a.gyeonggiMeans)[2] <- c("Gyeonggi")

dese <- merge(a.deaguMeans,a.seoulMeans,by=c("?��"))
desegy <- merge(dese,a.gyeonggiMeans,by=c("?��"))

ggplot(data=desegy,aes(x=month),xlim=c(1,12))+
  geom_line(aes(y=Deagu,colour="Deagu"),size=1)+
  geom_line(aes(y=Seoul,colour="Seoul"),size=1)+
  geom_line(aes(y=Gyeonggi,colour="Gyeonggi"),size=1)+
  ggtitle("??��?,?��?��,경기 교통?���?")+
  xlab("?��")+
  ylab("교통?���?")+
  scale_x_continuous(breaks = 1:12,labels=month)+
  theme(plot.title = element_text(color="black",size=14,face="bold"))+
  scale_color_manual(name="y",values =c("Deagu"="red",
                                        "Seoul"="blue","Gyeonggi"="green"))



region <- a.region.3.total[,1]
accident.3 <- a.region.3.total[,2]
ggplot(data=a.region.3.total,aes(x=reorder(region,-accident.3),y=accident.3))+
  geom_bar(stat="identity",width=0.5,fill="darkred")+
  ggtitle("?��?���? 3?�� ?��고건?��")+
  xlab("?��?��")+
  ylab("총사고건?��")+
  theme(plot.title = element_text(color="black",size=14,face="bold"))


#?��?��별사고건?��막�?�그래?��
a.region <- aggregate(accident[,4],
                      by=list(?��?��=accident$?��?��),
                      FUN=sum)
names(a.region)[2] <- c("총사고건?��")
a.region

region <- a.region[,1]
accR <- a.region[,2]
ggplot(data=a.region,aes(x=reorder(region,),y=accR))+
  geom_bar(stat="identity",width=0.5,fill="darkred")+
  ggtitle("?��?���? ?��고건?��")+
  xlab("?��?��")+
  ylab("총사고건?��")+
  theme(plot.title = element_text(color="black",size=14,face="bold"))



#?��?��별사고건?��지?��
a.region
region
gcRegion <- geocode(enc2utf8(region[-18]))
a.region.loc <- data.frame(a.region,
                  lon=gcRegion$lon,
                  lat=gcRegion$lat)
#?���? ?���? ?��?��
a.region.loc[10,3] <- 127.1600
a.region.loc[10,4] <- 36.3000
a.region.loc
a.region.cen <- c(mean(a.region.loc$lon),mean(a.region.loc$lat))
a.region.map <- get_googlemap(center=a.region.cen,
                      maptype="roadmap",
                      size=c(640,640),
                      zoom=7)
a.region.gmap <- ggmap(a.region.map)

a.region.gmap+geom_point(data=a.region.loc,
                         aes(x=lon,y=lat,size=총사고건?��),
                         alpha=0.3,col="red")+
  scale_size_continuous(range=c(1,10))


#?��고건?�� 많�?� ?��군구 ?��?�� 10 막�?�그래?��?��
a.gu <- aggregate(accident[,4],
                  by=list(?��?��=accident$?��?��,?��군구=accident$?��군구),
                  FUN=sum)
names(a.gu)[3] <- c("총사고건?��")
a.gu

a.gu <- a.gu[order(a.gu$총사고건?��,decreasing = T),]
gu10 <- a.gu[1:10,]
gu10

ggplot(gu10,aes(x=reorder(?��군구,-총사고건?��),y=총사고건?��))+
  geom_bar(stat="identity",width=0.7,fill="darkred")+
  ggtitle("총사고건?��가 많�?� ?��?�� ?��군구")+
  xlab("?��군구")+
  theme(plot.title=element_text(color="black",size=14,face="bold"))


#경북 ?���? 지?��?��
  a.gu.ac <-a.gu[a.gu$?��?��=="경북",]
  a.gu.ac
  gc3 <- geocode(enc2utf8(a.gu.ac$?��군구))
  df3 <- data.frame(a.gu.ac,
                    lon=gc3$lon,
                    lat=gc3$lat)
  df3
  cen3 <- c(mean(df3$lon),mean(df3$lat))
  cen3
  map3 <- get_googlemap(center=cen3,
                        maptype="roadmap",
                        size=c(640,640),
                        zoom=7)
  gmap3 <- ggmap(map3)
  gmap3+geom_point(data=df3,
                   aes(x=lon,y=lat,size=총사고건?��),
                   alpha=0.3,col="red")

#?��?���? ?���?, ?���? ?��리맵
a.region.death <- aggregate(accident[,c(4,5)],
                            by=list(?��?��=accident$?��?��),
                            FUN=sum)
names(a.region.death)[2] <- c("총사고건?��")
a.region.death

treemap(a.region.death,
        index=c("?��?��"),
        vSize="?��고건?��",
        vColor="?��망자?��",
        type="value",
        title="?��?���? ?��고건?��,?��망자?��")


#?���?-(?���?+부?��)
head(accident)
accident[,10]<- apply(accident[,c(4,9)],1,sum)
names(accident)[10] <- c("?��망�?�?��")
accident[,11]<- apply(accident[,c(4,10)],1,diff)
names(accident)[11] <- c("?��고피?��")

a.diff <- aggregate(accident[,11],
                  by=list(?��?��=accident$?��?��),
                  FUN=sum)
names(a.diff)[2] <- c("총피?��")
a.diff

a.diff <- a.diff[order(a.diff$총피?��,decreasing = T),]
a.diff10 <- a.diff[1:10,]
a.diff10

ggplot(a.diff10,aes(x=reorder(?��?��,-총피?��),y=총피?��))+
  geom_bar(stat="identity",width=0.7,fill="darkred")+
  ggtitle("?��고피?��가 많�?� ?��?�� ?��?��")+
  xlab("?��?��")+
  theme(plot.title=element_text(color="black",size=14,face="bold"))


#?��관분석
cor(accident[,4:8])
plot(accident[,4:8],pch=16,col="blue")



#1,12?�� ?��고건?�� 변?��가 ?�� ?��?�� 5?��?��
a.change <- aggregate(accident[,4],
                      by=list(?��?��=accident$?��?��,?��=accident$발생?��),
                      FUN=sum)
names(a.change)[3] <- c("총사고건?��")
a.change

change1 <- a.change[a.change$?��==1,]
change12 <- a.change[a.change$?��==12,]
names(change1)[3] <- c("cnt1")
names(change12)[3] <- c("cnt3")

a.diff <- merge(change1[,c(1,3)],change12[,c(1,3)])
a.diff
a.diff$diff <- abs(a.diff$cnt1-a.diff$cnt3)
a.diff <- a.diff[order(by=a.diff$diff,decreasing = T),]
diff5 <- a.diff[1:5,1]
diff5

change <- subset(a.change,a.change$?��?��%in%diff5)
change

ggplot(change,aes(x=?��,y=총사고건?��,colour=?��?��,group=?��?��))+
  geom_line() +
  geom_point(size=4,shape=18,alpha=0.5)+
  ggtitle("1,12?�� 총사건건?�� 변?�� Top5 ?��?��")+
  scale_x_continuous(breaks = 1:12,labels=month)+
  theme(plot.title = element_text(color="black",size=14,face="bold"))