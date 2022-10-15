
library(ggplot2)
library(ggmap)
register_google(key="AIzaSyCUNz0wLm3amrseiT1RyDtD3sOnxsphGXg")
library(treemap)

accident <- read.csv("?„ë¡œêµ?†µê³µë‹¨_?‹œêµ°êµ¬ë³? ?›”ë³? êµí†µ?‚¬ê³? ?†µê³?_20201231.csv")
head(accident)
accident <- data.frame(accident)

#ë¶€?ƒ?ž?•©
for(i in 1:nrow(accident)){
  accident[i,9] <- apply(accident[i,6:8],1,sum)
}
names(accident)[9] <- c("ë¶€?ƒ?ž?ˆ˜")
head(accident)


#?›”ë³„ì‚¬ê³ ê±´?ˆ˜?„ ê·¸ëž˜?”„
a.month <- aggregate(accident[,c(4,5,9)],
                     by=list(?›”=accident$ë°œìƒ?›”),
                     FUN=sum)
names(a.month)[2] <- c("ì´ì‚¬ê³ ê±´?ˆ˜")
a.month
month <- a.month[,1]
accidentToatal <- a.month[,2]
deathtoll <- a.month[,3]
injuries<- a.month[,4]
ggplot(data=a.month,aes(x=month),xlim=c(1,12))+
    geom_line(aes(y=accidentToatal,colour="accidentToatal"),size=1)+
  geom_line(aes(y=deathtoll,colour="deathtoll"),size=1)+
  geom_line(aes(y=injuries,col="injuries"),size=1)+
  ggtitle("?›”ë³? êµí†µ?‚¬ê³?")+
  xlab("?›”")+
  ylab("êµí†µ?‚¬ê³?")+
  scale_x_continuous(breaks = 1:12,labels=month)+
  theme(plot.title = element_text(color="black",size=14,face="bold"))+
  scale_color_manual(name="y",values =c("accidentToatal"="black",
                                        "deathtoll"="red","injuries"="blue"))

#?„œ?š¸,ê²½ê¸°,??€êµ? 3?›”?‚¬ê³ ê±´?ˆ˜ë§‰ë?€ê·¸ëž˜?”„
a.deagu <- accident[accident$?‹œ?„=="??€êµ?",]
a.deaguMeans <- aggregate(a.deagu[,4],
                      by=list(?›”=a.deagu$ë°œìƒ?›”),
                      FUN=mean)
names(a.deaguMeans)[2] <- c("Deagu")

a.seoul <- accident[accident$?‹œ?„=="?„œ?š¸",]
a.seoulMeans <- aggregate(a.seoul[,4],
                          by=list(?›”=a.seoul$ë°œìƒ?›”),
                          FUN=mean)
names(a.seoulMeans)[2] <- c("Seoul")

a.gyeonggi <- accident[accident$?‹œ?„=="ê²½ê¸°",]
a.gyeonggiMeans <- aggregate(a.gyeonggi[,4],
                          by=list(?›”=a.gyeonggi$ë°œìƒ?›”),
                          FUN=mean)
names(a.gyeonggiMeans)[2] <- c("Gyeonggi")

dese <- merge(a.deaguMeans,a.seoulMeans,by=c("?›”"))
desegy <- merge(dese,a.gyeonggiMeans,by=c("?›”"))

ggplot(data=desegy,aes(x=month),xlim=c(1,12))+
  geom_line(aes(y=Deagu,colour="Deagu"),size=1)+
  geom_line(aes(y=Seoul,colour="Seoul"),size=1)+
  geom_line(aes(y=Gyeonggi,colour="Gyeonggi"),size=1)+
  ggtitle("??€êµ?,?„œ?š¸,ê²½ê¸° êµí†µ?‚¬ê³?")+
  xlab("?›”")+
  ylab("êµí†µ?‚¬ê³?")+
  scale_x_continuous(breaks = 1:12,labels=month)+
  theme(plot.title = element_text(color="black",size=14,face="bold"))+
  scale_color_manual(name="y",values =c("Deagu"="red",
                                        "Seoul"="blue","Gyeonggi"="green"))



region <- a.region.3.total[,1]
accident.3 <- a.region.3.total[,2]
ggplot(data=a.region.3.total,aes(x=reorder(region,-accident.3),y=accident.3))+
  geom_bar(stat="identity",width=0.5,fill="darkred")+
  ggtitle("?‹œ?„ë³? 3?›” ?‚¬ê³ ê±´?ˆ˜")+
  xlab("?‹œ?„")+
  ylab("ì´ì‚¬ê³ ê±´?ˆ˜")+
  theme(plot.title = element_text(color="black",size=14,face="bold"))


#?‹œ?„ë³„ì‚¬ê³ ê±´?ˆ˜ë§‰ë?€ê·¸ëž˜?”„
a.region <- aggregate(accident[,4],
                      by=list(?‹œ?„=accident$?‹œ?„),
                      FUN=sum)
names(a.region)[2] <- c("ì´ì‚¬ê³ ê±´?ˆ˜")
a.region

region <- a.region[,1]
accR <- a.region[,2]
ggplot(data=a.region,aes(x=reorder(region,),y=accR))+
  geom_bar(stat="identity",width=0.5,fill="darkred")+
  ggtitle("?‹œ?„ë³? ?‚¬ê³ ê±´?ˆ˜")+
  xlab("?‹œ?„")+
  ylab("ì´ì‚¬ê³ ê±´?ˆ˜")+
  theme(plot.title = element_text(color="black",size=14,face="bold"))



#?‹œ?„ë³„ì‚¬ê³ ê±´?ˆ˜ì§€?„
a.region
region
gcRegion <- geocode(enc2utf8(region[-18]))
a.region.loc <- data.frame(a.region,
                  lon=gcRegion$lon,
                  lat=gcRegion$lat)
#?„¸ì¢? ?œ„ì¹? ?ˆ˜? •
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
                         aes(x=lon,y=lat,size=ì´ì‚¬ê³ ê±´?ˆ˜),
                         alpha=0.3,col="red")+
  scale_size_continuous(range=c(1,10))


#?‚¬ê³ ê±´?ˆ˜ ë§Žì?€ ?‹œêµ°êµ¬ ?ƒ?œ„ 10 ë§‰ë?€ê·¸ëž˜?”„?”„
a.gu <- aggregate(accident[,4],
                  by=list(?‹œ?„=accident$?‹œ?„,?‹œêµ°êµ¬=accident$?‹œêµ°êµ¬),
                  FUN=sum)
names(a.gu)[3] <- c("ì´ì‚¬ê³ ê±´?ˆ˜")
a.gu

a.gu <- a.gu[order(a.gu$ì´ì‚¬ê³ ê±´?ˆ˜,decreasing = T),]
gu10 <- a.gu[1:10,]
gu10

ggplot(gu10,aes(x=reorder(?‹œêµ°êµ¬,-ì´ì‚¬ê³ ê±´?ˆ˜),y=ì´ì‚¬ê³ ê±´?ˆ˜))+
  geom_bar(stat="identity",width=0.7,fill="darkred")+
  ggtitle("ì´ì‚¬ê³ ê±´?ˆ˜ê°€ ë§Žì?€ ?ƒ?œ„ ?‹œêµ°êµ¬")+
  xlab("?‹œêµ°êµ¬")+
  theme(plot.title=element_text(color="black",size=14,face="bold"))


#ê²½ë¶ ?‚¬ê³? ì§€?„?„
  a.gu.ac <-a.gu[a.gu$?‹œ?„=="ê²½ë¶",]
  a.gu.ac
  gc3 <- geocode(enc2utf8(a.gu.ac$?‹œêµ°êµ¬))
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
                   aes(x=lon,y=lat,size=ì´ì‚¬ê³ ê±´?ˆ˜),
                   alpha=0.3,col="red")

#?‹œ?„ë³? ?‚¬ê±?, ?‚¬ë§? ?Š¸ë¦¬ë§µ
a.region.death <- aggregate(accident[,c(4,5)],
                            by=list(?‹œ?„=accident$?‹œ?„),
                            FUN=sum)
names(a.region.death)[2] <- c("ì´ì‚¬ê³ ê±´?ˆ˜")
a.region.death

treemap(a.region.death,
        index=c("?‹œ?„"),
        vSize="?‚¬ê³ ê±´?ˆ˜",
        vColor="?‚¬ë§ìž?ˆ˜",
        type="value",
        title="?‹œ?„ë³? ?‚¬ê³ ê±´?ˆ˜,?‚¬ë§ìž?ˆ˜")


#?‚¬ê³?-(?‚¬ë§?+ë¶€?ƒ)
head(accident)
accident[,10]<- apply(accident[,c(4,9)],1,sum)
names(accident)[10] <- c("?‚¬ë§ë?€?ƒ")
accident[,11]<- apply(accident[,c(4,10)],1,diff)
names(accident)[11] <- c("?‚¬ê³ í”¼?•´")

a.diff <- aggregate(accident[,11],
                  by=list(?‹œ?„=accident$?‹œ?„),
                  FUN=sum)
names(a.diff)[2] <- c("ì´í”¼?•´")
a.diff

a.diff <- a.diff[order(a.diff$ì´í”¼?•´,decreasing = T),]
a.diff10 <- a.diff[1:10,]
a.diff10

ggplot(a.diff10,aes(x=reorder(?‹œ?„,-ì´í”¼?•´),y=ì´í”¼?•´))+
  geom_bar(stat="identity",width=0.7,fill="darkred")+
  ggtitle("?‚¬ê³ í”¼?•´ê°€ ë§Žì?€ ?ƒ?œ„ ?‹œ?„")+
  xlab("?‹œ?„")+
  theme(plot.title=element_text(color="black",size=14,face="bold"))


#?ƒê´€ë¶„ì„
cor(accident[,4:8])
plot(accident[,4:8],pch=16,col="blue")



#1,12?›” ?‚¬ê³ ê±´?ˆ˜ ë³€?™”ê°€ ?° ?ƒ?œ„ 5?‹œ?„
a.change <- aggregate(accident[,4],
                      by=list(?‹œ?„=accident$?‹œ?„,?›”=accident$ë°œìƒ?›”),
                      FUN=sum)
names(a.change)[3] <- c("ì´ì‚¬ê³ ê±´?ˆ˜")
a.change

change1 <- a.change[a.change$?›”==1,]
change12 <- a.change[a.change$?›”==12,]
names(change1)[3] <- c("cnt1")
names(change12)[3] <- c("cnt3")

a.diff <- merge(change1[,c(1,3)],change12[,c(1,3)])
a.diff
a.diff$diff <- abs(a.diff$cnt1-a.diff$cnt3)
a.diff <- a.diff[order(by=a.diff$diff,decreasing = T),]
diff5 <- a.diff[1:5,1]
diff5

change <- subset(a.change,a.change$?‹œ?„%in%diff5)
change

ggplot(change,aes(x=?›”,y=ì´ì‚¬ê³ ê±´?ˆ˜,colour=?‹œ?„,group=?‹œ?„))+
  geom_line() +
  geom_point(size=4,shape=18,alpha=0.5)+
  ggtitle("1,12?›” ì´ì‚¬ê±´ê±´?ˆ˜ ë³€?™” Top5 ?‹œ?„")+
  scale_x_continuous(breaks = 1:12,labels=month)+
  theme(plot.title = element_text(color="black",size=14,face="bold"))