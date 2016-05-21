#DS 2016

#Working Directory
setwd("") #MUST SET WORKING DIRECTORY

#Data
links <- read.csv("NewData/Links.csv")
links$PublishDate <- as.POSIXct(links$PublishDate)
analysis <- read.csv("NewData/Analysis.csv")

#IMAGES
library(ggplot2)
library(scales)
library(dplyr)
library(grid)

dat <- links[,c("PublishDate","Topic")]
dat$Day <- substr(dat$PublishDate,1,10)
d <- tbl_df(dat[,3:2])

## Graph with number of news per topic
nrNewsTopic <- d %>% group_by(Topic) %>% filter(Day >= "2015-04-01") %>% filter(Day <= "2015-07-15") %>% summarize(nrNews=n())
g1 <- ggplot(nrNewsTopic,aes(x=Topic,y=nrNews)) + geom_bar(stat="identity")

## Graph with number of news per topic per day (2 alternatives: lines or smoothed lines)
nrNewsTopicDay <- d %>% group_by(Topic,Day) %>% filter(Day > "2015-04-01") %>% filter(Day <= "2015-07-15") %>% summarize(nrNews=n()) %>% arrange(Day)
nrNewsTopicDay$Day <- as.Date(nrNewsTopicDay$Day)
#This is to avoid that month names appear in Portuguese
currLoc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
g2.1 <- ggplot(nrNewsTopicDay,aes(x=Day,y=nrNews,group=Topic,color=Topic,linetype=Topic)) + geom_smooth() +  scale_x_date(labels=date_format("%b-%Y")) + ylab("nrNews/Day")
g2.2 <- ggplot(nrNewsTopicDay,aes(x=Day,y=nrNews,group=Topic,color=Topic,linetype=Topic)) + geom_line() +  scale_x_date(labels=date_format("%b-%Y"))

pdf("smoothVersion2.pdf",width=12,height=5)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(g1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(g2.1,vp=viewport(layout.pos.row=1,layout.pos.col=2))
dev.off()


##########
newstweets <- data.frame(IDLink=analysis$IDLink,NTweets=analysis$TS144)
newstweets["Topic"] <- links[match(newstweets$IDLink,links$IDLink),]$Topic

newstweets.economy <- as.data.frame(table(newstweets[newstweets$Topic=="economy",]$NTweets))
colnames(newstweets.economy) <- c("NTweets","Frequency")
newstweets.economy["Topic"] <- "economy"

newstweets.microsoft <- as.data.frame(table(newstweets[newstweets$Topic=="microsoft",]$NTweets))
colnames(newstweets.microsoft) <- c("NTweets","Frequency")
newstweets.microsoft["Topic"] <- "microsoft"

newstweets.obama <- as.data.frame(table(newstweets[newstweets$Topic=="obama",]$NTweets))
colnames(newstweets.obama) <- c("NTweets","Frequency")
newstweets.obama["Topic"] <- "obama"

newstweets.palestine <- as.data.frame(table(newstweets[newstweets$Topic=="palestine",]$NTweets))
colnames(newstweets.palestine) <- c("NTweets","Frequency")
newstweets.palestine["Topic"] <- "palestine"

newstweets.final <- rbind(newstweets.economy,newstweets.microsoft,newstweets.obama,newstweets.palestine)
newstweets.final <- newstweets.final[!(newstweets.final$NTweets %in% c(-1,0)),]
powerlaw <- ggplot(newstweets.final,aes(x=NTweets,y=Frequency,group=Topic,color=Topic)) + geom_line(size=1) + scale_x_discrete(breaks=c(10,100,250,500)) + scale_y_discrete(breaks=seq(0,2000,by=200))
pdf("powerlaw.pdf",width=6,height=4)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
print(powerlaw,vp=viewport(layout.pos.row=1,layout.pos.col=1))
dev.off()


