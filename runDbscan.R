library(dplyr)
library(dbscan)
library(ggplot2)

# Landlots which have 
# 1) less than numOfNeighbours 
# 2) "Roads" are excluded.

#Returns lotids and their new clusterid
runDbScan<-function(szId,t4,t5){
  
    db_param<-read.csv("data/dbscan_param.csv",stringsAsFactors = FALSE)
#numOfNeighbours<-3
  
  t7<-t5[[as.character(szId)]]
  centroid.attribute<-select(t4,szId,lotId,attribute) %>%
    distinct(.) %>%
    inner_join(.,t7,by="lotId") %>%
    select(.,-szId.y) %>%
    rename(szId=szId.x)
  
  
  lot_categories <-unique(centroid.attribute$attribute)
  lot_categories<-lot_categories[lot_categories!="Roads"]
  
  #Empty data.frame to hold results
  centroid.attribute.results<-centroid.attribute[0,]
  
  for(lot_category in lot_categories){

	minPts<-filter(db_param,attribute==lot_category) %>% select(minPts) %>% as.integer
    epsilon<-filter(db_param,attribute==lot_category) %>% select(epsilon) %>% as.numeric

    centroid.attribute.filter<-filter(centroid.attribute,attribute==lot_category)
    centroid.attribute.pt<-select(centroid.attribute.filter,long,lat)
    if(count(centroid.attribute.pt)>minPts){
      # kNNdistplot(centroid.attribute.pt,k=minPts)
      # abline(h=epsilon)
      ds<-dbscan(centroid.attribute.pt,eps=epsilon,minPts=minPts)
      centroid.attribute.filter$clusterid<-paste(szId,lot_category,ds$cluster,sep="_")
#       print(qplot(data=centroid.attribute.pt,
#             x=long,y=lat,
#             color=as.factor(ds$cluster),
#             main=paste(lot_category,szId))
# 		   )
    }
    else{
      centroid.attribute.filter$clusterid<-paste(szId,lot_category,0L,sep="_")
    }
    #Append results
    centroid.attribute.results<-bind_rows(centroid.attribute.results,centroid.attribute.filter)
  }

return (centroid.attribute.results);
}

beautifyClusters<-function(centroid.attribute.results){
  r1<-select(centroid.attribute.results,clusterid) %>% distinct
  r1$newclusterid<-gsub(patter=".*0$",replacement=0,r1$clusterid)
  t<-bind_rows(filter(r1,newclusterid=="0"),
            filter(r1,newclusterid!="0") %>% 
              mutate(newclusterid=as.character(row_number(newclusterid)))
  )
  inner_join(t,centroid.attribute.results,by="clusterid")
}

loadData<-function(subzones_filename,classification_filename,coordinates_filename){
  #subzone-lotId
  t1<-read.csv(subzones_filename,stringsAsFactors = FALSE) 
  #lotId-attribute
  t2<-read.csv(classification_filename,stringsAsFactors = FALSE) 
  #lotId-{long,lat}
  t3<-read.csv(coordinates_filename,stringsAsFactors = FALSE) 
  #inner join
  t4 <- inner_join(t1,t2,by="lotId") %>%
    inner_join(t3,by="lotId")
  
  #centroids and separated by zones
  t5<-t4 %>% 
    group_by(szId,lotId) %>%
    summarize(cnt=n(),long=mean(long),lat=mean(lat)) %>%
    split(.,f=.$szId)
  return(list(t4,t5))
  
}
