variation<-function(folderIn,R=TRUE,folderOut){
  require(png)
  require(raster)
  require(SpaDES.tools)
  require(igraph)
  
filenames <- list.files(folderIn, pattern="*.png", full.names=TRUE)
imgl <- lapply(filenames, try(readPNG))
PlotName<- sapply(strsplit(sapply(strsplit(filenames,"_-_"),"[",2),"\\."),"[",1)

prop.vec<-rep(0,length(imgl))
prop.vec.within<-rep(0,length(imgl))
for(i in 1:length(imgl)){
  img<-imgl[[i]]
  img.r <- as.raster(img,interpolate=F)
  img2<-as.matrix(img.r)
  if(length(sort(unique(as.vector(img.r))))==2){
  img2[img2==sort(unique(as.vector(img.r)))[2]]<-1 #FFF 
  img2[img2==sort(unique(as.vector(img.r)))[1]]<-0 #000 
  } else {
  if(length(sort(unique(as.vector(img.r))))==1){
    img2[img2==sort(unique(as.vector(img.r)))[1]]<-1 #FFF
  }
  }
  img2<-matrix(as.numeric(img2),nrow=dim(img2)[1],ncol=dim(img2)[2],byrow=FALSE)

  Rmat <- raster(img2)
  sections=splitRaster(Rmat, nx=2, ny=1)
  plot(sections[[2]])

clump.list<-list()
clump.list<-lapply(sections,FUN=function(x) as.matrix(clump(x,directions=8)))

  tot1 <- max(clump.list[[1]],0, na.rm=TRUE)
  res1 <- vector("list",max(tot1,1))
  if(tot1==0){
    res1[1]<-1
    } else { 
      for (j in 1:max(clump.list[[1]], na.rm=TRUE)){
  res1[j] <- list(which(clump.list[[1]] == j, arr.ind = TRUE))
      }
    }
  
NPixels1<-unlist(lapply(res1,FUN=function(x){length(x)}))

tot2 <- max(clump.list[[2]],0, na.rm=TRUE)
res2 <- vector("list",max(1,tot2))
if(tot2==0){
  res2[1]<-1
  } else {
    for (k in 1:max(clump.list[[2]], na.rm=TRUE)){
      res2[k] <- list(which(clump.list[[2]] == k, arr.ind = TRUE))
    }
  }

NPixels2<-unlist(lapply(res2,FUN=function(x){length(x)}))

prop.vec[i]<-max(sum(NPixels1),sum(NPixels2))/min(sum(NPixels1),sum(NPixels2))

}

for(i in 1:length(imgl)){
  img<-imgl[[i]]
  img.r <- as.raster(img,interpolate=F)
  img2<-as.matrix(img.r)
  if(length(sort(unique(as.vector(img.r))))==2){
    img2[img2==sort(unique(as.vector(img.r)))[2]]<-1 #FFF 
    img2[img2==sort(unique(as.vector(img.r)))[1]]<-0 #000 
  } else {
    if(length(sort(unique(as.vector(img.r))))==1){
      img2[img2==sort(unique(as.vector(img.r)))[1]]<-1 #FFF
    }
  }
  img2<-matrix(as.numeric(img2),nrow=dim(img2)[1],ncol=dim(img2)[2],byrow=FALSE)
  
  Rmat <- raster(img2)
  sections=splitRaster(Rmat, ny=2, nx=1)
  plot(sections[[2]])
  
  clump.list<-list()
  clump.list<-lapply(sections,FUN=function(x) as.matrix(clump(x,directions=8)))
  
  tot1 <- max(clump.list[[1]],0, na.rm=TRUE)
  res1 <- vector("list",max(tot1,1))
  if(tot1==0){
    res1[1]<-1
  } else { 
    for (j in 1:max(clump.list[[1]], na.rm=TRUE)){
      res1[j] <- list(which(clump.list[[1]] == j, arr.ind = TRUE))
    }
  }
  
  NPixels.w.1<-unlist(lapply(res1,FUN=function(x){length(x)}))
  
  tot2 <- max(clump.list[[2]],0, na.rm=TRUE)
  res2 <- vector("list",max(1,tot2))
  if(tot2==0){
    res2[1]<-1
  } else {
    for (k in 1:max(clump.list[[2]], na.rm=TRUE)){
      res2[k] <- list(which(clump.list[[2]] == k, arr.ind = TRUE))
    }
  }
  
  NPixels.w.2<-unlist(lapply(res2,FUN=function(x){length(x)}))
  
  prop.vec.within[i]<-max(sum(NPixels.w.1),sum(NPixels.w.2))/min(sum(NPixels.w.1),sum(NPixels.w.2))
  
}

res <-data.frame(PlotName = PlotName, 
                 Variation.between=prop.vec,
                 Variation.within=prop.vec.within)

if(R){
  res
  } else {
    setwd(folderOut)
    write.csv2(res,"variation.csv")
  }
}

