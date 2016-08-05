#get clean data
players <- players[,c(1:10)]
players<-players[c(1:3924),]
players$firstseason <- as.numeric(players$firstseason)
players$lastseason <- as.numeric(players$lastseason)
players$weight <- as.numeric(players$weight)
players$h_feet <- as.numeric(players$h_feet)
players$h_inches <- as.numeric(players$h_inches)
players <- na.omit(players)
#p_hei <- paste(players$h_feet,players$h_inches,sep = ".")
#height<-c(as.numeric(p_hei))
#players<-cbind(players[,c(1:6)],height,players[,c(9,10)])
players<-players[which(players$lastseason<2006 & players$firstseason>1979),]
players <- cbind(players[,c(1)], paste(players$firstname, players$lastname),players[,c(4:9)])
#clean data career
aaa<-player_career[,c(6:21)] / player_career$gp
names(aaa)<-c("avg.minutes","avg.pts", "avg.oreb", "avg.dreb","avg.reb","avg.asts","avg.stl","avg.blk","avg.turnover","avg.pf","avg.fga","avg.fgm","avg.fta","avg.ftm","avg.tpa","avg.tpm")

#merge 
players_1980_to_2005 <- merge(players,player_career,by="ilkid")
players_1980_to_2005 <- players_1980_to_2005[,c(1:8,12:28)]

#do some statistics for overall data, mean
statistics<-as.data.frame(t(as.data.frame(apply(players_1980_to_2005avg[,c(10:25)], 2, mean))))
#statistics<-cbind(t(as.data.frame(c(ilkid="ZZZZZZ", name="Median", year=1980))),a)
#row.names(statistics)<-c(99999:(99999+25))
row.names(statistics)<-c("statistics")

commu<-players_1980_to_2005[,c(6:25)]
row.names(commu)<-players_1980_to_2005$ilkid
commu <- na.omit(commu)

#ncommu <- apply(commu, 2, scale)
#ncommu<-as.data.frame(ncommu)

#row.names(ncommu) <- row.names(commu)
d <- dist(commu)

#k-median
fit.pam <- pam(d, k=6, stand=TRUE)
clusplot(fit.pam, main="Cluster Plot", color = TRUE, lines = 0, labels=5,col.p=c("black"))
players_6 <- players_1980_to_2005[fit.pam$clustering==6,]
players_5 <- players_1980_to_2005[fit.pam$clustering==5,]
players_4 <- players_1980_to_2005[fit.pam$clustering==4,]
players_3 <- players_1980_to_2005[fit.pam$clustering==3,]
players_2 <- players_1980_to_2005[fit.pam$clustering==2,]
players_1 <- players_1980_to_2005[fit.pam$clustering==1,]

#k-means
kclus <- kmeans(commu, centers=6)
#fpc required
plotcluster(commu, kclus$cluster)
players_6 <- players_1980_to_2005[kclus$cluster==6,]
players_5 <- players_1980_to_2005[kclus$cluster==5,]
players_4 <- players_1980_to_2005[kclus$cluster==4,]
players_3 <- players_1980_to_2005[kclus$cluster==3,]
players_2 <- players_1980_to_2005[kclus$cluster==2,]
players_1 <- players_1980_to_2005[kclus$cluster==1,]
#plot(fit)

#clus <- hclust(d, method="ward.D")
#colors<-c("red","blue","green","orange","pink","black")
#clusterCut <- cutree(clus, 5)
#plot(clus)
#plot(as.phylo(clus), type = "fan", tip.color = colors[clusterCut],label.offset = 1.5, cex = 0.5,font = 1)



#do some satistics about player_4
options(digits = 2)
players_4_avg <-cbind(players_4[,c(2)], players_4[,c(10:25)]/players_4$gp)
fix(players_4_avg)
players_4_avg<-rbind(players_4_avg, cbind(name="Average Performance", statistics))

#height and weight
ggplot(players_4,
       aes(x=(h_feet*12+h_inches),y=weight))+
  geom_point(aes(color=weight/(h_feet*12+h_inches), size=weight/(h_feet*12+h_inches)/(h_feet*12+h_inches)*703/10) )+
  geom_smooth(linetype=2)+ geom_text(aes(label=name), size = 4,vjust = -1, check_overlap = T)+
  scale_color_continuous(name="weight/height",breaks=c(2.5,2.8,3.0),labels=c("thin", "middle", "strong"))+
  scale_size_continuous(name = "weight/height",breaks=c(2.2,2.5,2.8),labels=c("thin", "middle", "strong"))+
  scale_x_continuous(breaks = c(72,75,78,81,84,85), labels = c("6-0","6-3","6-6","6-9","7-0","7-1"))+
  theme(panel.grid =element_blank())+
  labs(x = "Height(ft-in)", y = "Weight(lbs)", title = "Weight/Height of Top Player")

#player_over_year_in_regular_season
player_regular_season$ilkid <- sub(" ","",player_regular_season$ilkid)
prs_4 <- merge(players_4[,c(1:2)], player_regular_season, by="ilkid")
prs_4<-prs_4[,c(1,2,3,8:24)]
prs_4 <-aggregate(prs_4[,c(4:20)], by=list(ilkid=prs_4$ilkid, name=prs_4$name, year=prs_4$year), FUN = sum)
prs_4_avg <-cbind(prs_4[,c(1,2,3)], prs_4[,c(5:20)]/prs_4$gp)
#find some sample
sample_name_list<-players_4$name[sample(c(1:length(players_4$name)), 7)]
prs_4_avg_sample<-prs_4_avg[(prs_4_avg$name %in% sample_name_list),]
#plot it
PlayerName <-prs_4_avg_sample$name
ggplot(prs_4_avg_sample, aes(x=year, y=minutes,colour=PlayerName, group=PlayerName))+geom_line(size=1)+geom_hline(size=1.5, yintercept=statistics$minutes,colour="black")
ggplotly(ggplot2::last_plot())

#draw something intersting
options(digits = 2)
players_4_avg_some<-players_4_avg[,c(1,2,3,6,7,12,13)]
radarDF <- players_4_avg_some %>% select(name, 2:7) %>% as.data.frame()
radarDF <- gather(radarDF, key=Label, value=Score, convert = TRUE, -name) %>%
  spread(key=name, value=Score, convert=TRUE)
chartJSRadar(scores = radarDF, maxScale = 30, showToolTipLabel = TRUE)

#stastistic between group
group_statistics_avg <- as.data.frame(t(apply(cbind(gp_100=players_1[,c(9)]/100, players_1[,c(10:25)]/players_1$gp), 2, mean)))
group_statistics_avg <- rbind(group_statistics_avg, apply(cbind(gp_hundred=players_2[,c(9)]/100, players_2[,c(10:25)]/players_2$gp), 2, mean))
group_statistics_avg <- rbind(group_statistics_avg, apply(cbind(gp_hundred=players_3[,c(9)]/100, players_3[,c(10:25)]/players_3$gp), 2, mean))
group_statistics_avg <- rbind(group_statistics_avg, apply(cbind(gp_hundred=players_4[,c(9)]/100, players_4[,c(10:25)]/players_4$gp), 2, mean))
group_statistics_avg <- rbind(group_statistics_avg, apply(cbind(gp_hundred=players_5[,c(9)]/100, players_5[,c(10:25)]/players_5$gp), 2, mean))
group_statistics_avg <- rbind(group_statistics_avg, apply(cbind(gp_hundred=players_6[,c(9)]/100, players_6[,c(10:25)]/players_6$gp), 2, mean))
group_statistics_avg<-cbind(Group=c("Group 1", "Group 2","Group 3", "Group 4", "Group 5", "Group 6"), group_statistics_avg)
test<-melt(group_statistics_avg, id=c("Group"))
test<-test[order(test$variable),]
#ggplot(test, aes(x=variable, y=value, fill=Group)) + geom_area()
ggplot(test, 
       aes(x=as.numeric(factor(variable, ordered=TRUE, levels=names(group_statistics_avg[c(2:18),]))), 
           y=value, group=Group, fill=Group))+
  geom_bar(stat="identity", alpha=0.9, position="dodge")+
  labs(x="Player Data", title="Compare Amoung Different Group")+
  scale_x_continuous(breaks = c(2:18), labels = names(group_statistics_avg[,c(2:18)]))
  

#options(digits = 2)
#group_statistics<-as.data.frame(t(apply(players_1[,c(9:25)], 2, mean)))
#group_statistics<-rbind(group_statistics,apply(players_2[,c(9:25)], 2, mean))
#group_statistics<-rbind(group_statistics,apply(players_3[,c(9:25)], 2, mean))
#group_statistics<-rbind(group_statistics,apply(players_4[,c(9:25)], 2, mean))
#group_statistics<-rbind(group_statistics,apply(players_5[,c(9:25)], 2, mean))
#group_statistics<-rbind(group_statistics,apply(players_6[,c(9:25)], 2, mean))
#group_statistics<-group_statistics[,c()]
#group_statistics<-cbind(Group=c("Group 1", "Group 2","Group 3", "Group 4", "Group 5", "Group 6"), group_statistics)
#melt_gs<-melt(group_statistics, id=c("Group"))
#ggplot(melt_gs, 
#       aes(x=as.numeric(factor(variable, ordered=TRUE)), 
#           y=value,
#           colour=Group, 
#           group=Group))+
#  geom_point(size=3)
