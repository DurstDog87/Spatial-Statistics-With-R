
####MI Sampler####
###Takes disease count data and randomly assigns each observation to a random point in the county of origin###
##Arguments:
#spdf = SpatialPolygonsDataFrame
#att = defines column of data frame holding disease counts

mi_sample<-function(spdf, att){
  ##Save CRS##
  crs_save<-CRS(proj4string(spdf))
  ##Subset only counties with count > 0; will return error if you try to sample from counties where count = 0##
  spdf_zrm<-spdf[spdf@data[,att]>0,]
  ##spsample assigns random point for all observations in county##
  ##loops through all counties with count > 0##
  list<-list()
  for(i in 1:length(spdf_zrm)){
    list[[i]]<-spsample(x=spdf_zrm[i,], n=spdf_zrm@data[i,att], type="random", iter=100)
  }
  ##Create a single matrix of coordinates from elements of previous list (samples per county)##
  coord.list<-list()
  for(i in 1:length(list)){
    coord.list[[i]]<-list[[i]]@coords
  }
  ##Collapses list into single matrix##
  coord.mat<-do.call(rbind, coord.list)
  colnames(coord.mat)<-c("x","y")
  ##Create new SpatialPointsDataFrame##
  sp<-SpatialPoints(coord.mat, crs_save)
}





