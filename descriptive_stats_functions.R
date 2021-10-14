####Functions####

############CT############
####Create Function called center_feat####
##arguments: 
###spdf - SpatialPointsDataFrame
cent_feat<-function(spdf){
  ###extract coordinates and add generic column header#
  xydat<-spdf@coords
  colnames(xydat)<-c("x","y")
  ####Calculate Euclidean Distance among all pairs#
  pt_dist<-dist(xydat[,c("x","y")], method="euclidean")
  ####Find total distance to sites (Table 4.2)#
  pt_dist_df<-as.data.frame(as.matrix(pt_dist))
  rm<-rowSums(pt_dist_df)
  ###Identify feature with smallest total distance#
  print(min(rm))
  final<-spdf[which.min(rm),]
}
#####

####Create Function called mean_center####
##arguments: 
###spdf - SpatialPointsDataFrame
mean_center<-function(spdf){
  ###extract coordinates and add generic column header#
  xydat<-spdf@coords
  colnames(xydat)<-c("x","y")
  ###Save CRS to project new SPDF#
  crs_save<-CRS(proj4string(spdf))
  ###Calculate mean of X and Y coordinates#
  mean_x<-sum(xydat[,1])/length(xydat[,1])
  mean_y<-sum(xydat[,2])/length(xydat[,2])
  ###Create new SP object - coords slot shows mean center#
  cent<-SpatialPoints(coords=cbind(mean_x,mean_y), proj4string=crs_save)
}
#####

####Create Function called med_feat####
##arguments: 
###spdf - SpatialPointsDataFrame
med_center<-function(spdf){
  ###extract coordinates and add generic column header#
  xydat<-spdf@coords
  colnames(xydat)<-c("x","y")
  ###Save CRS to project new SPDF#
  crs_save<-CRS(proj4string(spdf))
  ###Weiszfeld's algorithm - from "Gmedian" package#
  gmed<-Weiszfeld(X=xydat, nitermax=999)
  ###Create new SP object - coords slot shows med center#
  gmed_final<-SpatialPoints(gmed, proj4string=crs_save)
}
####

####Create Function called st_dist####
##arguments: 
###spdf - SpatialPointsDataFrame
###mc - mean center (SpatialPoints layer)
st_dist<-function(spdf, mc){
  ###extract coordinates and add generic column header#
  xydat<-spdf@coords
  mcdat<-mc@coords
  colnames(xydat)<-c("x","y")
  ###Save CRS to project new SPDF#
  crs_save<-CRS(proj4string(spdf))
  ###Calculate standard distance#
  sd<-sqrt(sum((xydat[,1]-mcdat[,1])^2 +(xydat[,2]-mcdat[,2])^2 )/nrow(xydat))
  print(sd)
  ###Draw Polygon#
  bearing<-1:360 * pi/180
  cx<-mcdat[,1]+sd*cos(bearing)
  cy<-mcdat[,2]+sd*sin(bearing)
  circle<-cbind(cx,cy)
  dat.mat<-as.data.frame(cbind(sd,mc@coords))
  colnames(dat.mat)<-c("sd","mcx", "mcy")
  poly<-Polygon(coords=circle)
  poly.list<-Polygons(srl=list(poly), ID=1)
  spl<-SpatialPolygons(list(poly.list), proj4string=crs_save)
  spdf<-SpatialPolygonsDataFrame(Sr=spl, data=dat.mat)
}
#####

####Create Function called st_ellipse####
##arguments: 
###spdf - SpatialPointsDataFrame
###mc - mean center (SpatialPoints Layer)
std_ellipse<-function(spdf, mc){
  ###extract coordinates and add generic column header#
  xydat<-spdf@coords
  mcdat<-mc@coords
  colnames(xydat)<-c("x","y")
  n<-nrow(xydat)
  ###Save CRS to project new SPDF#
  crs_save<-CRS(proj4string(spdf))
  ###Calculate deviations###
  dev_x<-xydat[,1]-mcdat[,1]
  dev_y<-xydat[,2]-mcdat[,2]
  dev_xy<-dev_x*dev_y
  ###Calculate angle of rotation###
  A<-(sum(dev_x^2)-sum(dev_y^2))
  B<-sqrt((A^2)+4*(sum(dev_xy)^2))
  C=2*sum(dev_xy)
  tan_theta=(A+B)/C
  theta_rad<-atan(tan_theta)
  theta_deg<-theta_rad*(180/pi)
  ###Standard deviations
  minor_step1<-(dev_x*cos(theta_rad)-dev_y*sin(theta_rad))
  minor_step2<-minor_step1^2
  minor_step3<-sqrt(sum(minor_step2)/n)     
  sd_minor<-minor_step3 * sqrt(2)
  major_step1<-(dev_x*sin(theta_rad)+dev_y*cos(theta_rad))
  major_step2<-major_step1^2
  major_step3<-sqrt(sum(major_step2)/n)
  sd_major<-major_step3*sqrt(2)
  ###Find ellipse
  draft1<-getellipse(rx=sd_minor, ry=sd_major, mid=mcdat, angle=180-theta_deg)
  eli_poly<-Polygon(draft1)
  eli_polys<-Polygons(list(eli_poly), ID="ellipse")
  eli_sppolys<-SpatialPolygons(Srl=list(eli_polys), proj4string=crs_save)
  angle<-ifelse(theta_deg<0, theta_deg + 180, theta_deg)
  eli_df<-as.data.frame(cbind(mcdat, sd_minor, sd_major, angle))
  rownames(eli_df)<-c("ellipse")
  colnames(eli_df)<-c("mean_x", "mean_y", "stdev_minor", "stdev_major", "rotation")
  eli_spdfpolys<-SpatialPolygonsDataFrame(eli_sppolys, data=eli_df)
}
#####
