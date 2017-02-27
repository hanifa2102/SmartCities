#First branch activity
#Process data for MK-18
#subzone-lotId
t1<-read.csv("data/landlot_subzones_clean.csv",stringsAsFactors = FALSE) %>%
  filter(grepl("MK18.*",lotId))
#lotId-attribute
t2<-read.csv("data/landlots_classification.csv",stringsAsFactors = FALSE) %>%
  filter(grepl("MK18.*",lotId))
#lotId-{long,lat}
t3<-read.csv("data/map_coordinates.csv",stringsAsFactors = FALSE) %>%
  filter(grepl("MK18.*",lotId))
#inner join
t4 <- inner_join(t1,t2,by="lotId") %>%
  inner_join(t3,by="lotId")

#centroids and separated by zones
t5<-t4 %>% 
  group_by(szId,lotId) %>%
  summarize(cnt=n(),long=mean(long),lat=mean(lat)) %>%
  split(.,f=.$szId)

#Load data and join it
r<-loadData("data/landlot_subzones_clean.csv",
            "data/landlots_classification.csv",
            "data/map_coordinates.csv")
## Run Code
#
results <-bind_rows(runDbScan(150),runDbScan(271),runDbScan(270),runDbScan(268)) %>%
  beautifyClusters(.)
# All
allSzids<-select(t4,szId) %>% distinct %>% .$szId
results<-runDbScan(allSzids[1])
for(i in allSzids[2:length(allSzids)]){results<-bind_rows(results,runDbScan(i))}
results<-beautifyClusters(results)

write.csv(results,"data/dbscan_clusters.csv",row.names = FALSE,quote=FALSE)
# 


#Run Code 21/09/2015
r<-loadData("data/landlot_subzones_clean.csv",
            "data/Final_Category_v3.csv",
            "data/map_coordinates.csv")
allSzids<-select(r[[1]],szId) %>% distinct %>% .$szId
results<-runDbScan(allSzids[1],r[[1]],r[[2]])
for(i in allSzids[2:length(allSzids)]){results<-bind_rows(results,runDbScan(i,r[[1]],r[[2]]))}
results<-beautifyClusters(results)
ar_attribute<-select(results,attribute) %>% distinct %>% .$attribute
for(i in 1:length(ar_attribute)){
  filter(results,attribute==ar_attribute[i]) %>% 
    write.csv(.,paste("data/21092015/",ar_attribute[i],".csv",sep=""),row.names = FALSE,quote=FALSE)
}

#Stats checking
#To check
select(results,clusterid,newclusterid) %>% 
  distinct %>% 
  arrange(as.integer(newclusterid)) %>%
  as.data.frame
#noise percentage
group_by(results,attribute) %>% summarize(total_cnt=n()) %>%
  inner_join(.,filter(results,newclusterid==0) %>% group_by(attribute) %>% summarize(noise_cnt=n()),
             by="attribute") %>%
  mutate(noise_percent=noise_cnt/total_cnt) %>%
  arrange(desc(noise_percent))
