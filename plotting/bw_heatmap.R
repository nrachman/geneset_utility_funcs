bwHMAP <- function(mat){
  #takes a matrix and plots a black and white heatmap
  #It is recommended that it is reordered beforehand
  
  colfunc <- colorRampPalette(c("white","black"))
  
  layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
  image(mat, col = gray(seq(0,1, by = .1)))
  #image(tmp, col = gray(1))
  
  legend_image <- as.raster(matrix(colfunc(20), ncol=1))
  #image(tmp, col = gray(seq(0,1, by = .1)))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'correlation')
  text(x=1.5, y = seq(0,1,l=5), labels = seq(round(min(mat),2), round(max(mat), 2),l=5))
  rasterImage(legend_image, 0, 0, 1,1)
}



#modified this code from first answer
#https://stackoverflow.com/questions/13355176/gradient-legend-in-base

#potential other function that adapts second answer
#legend currently wrong
# layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))
# colfunc <- colorRampPalette(c("white","black"))
# 
# par(mar=c(5.1,4.1,4.1,2.1))
# image(tmp, col = gray(seq(0,1, by = .1)))
# 
# #plot(1:10,ann=FALSE,type="n")
# #grid()
# #points(1:10,col=colfunc(10),pch=19,cex=1.5)
# 
# 
# xl <- 1
# yb <- 1
# xr <- 1.5
# yt <- 2
# 
# par(mar=c(5.1,0.5,4.1,0.5))
# plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
# #image(tmp, col = gray(seq(0,1, by = .1)))
# rect(
#      xl,
#      head(seq(yb,yt,(yt-yb)/10),-1),
#      xr,
#      tail(seq(yb,yt,(yt-yb)/10),-1),
#      col=colfunc(10)
#     )
# 
# mtext(round(min(tmp),2):round(max(tmp), 2),side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)bwHMAP <- function(mat){
#takes a matrix and plots a black and white heatmap
#It is recommended that it is reordered beforehand

colfunc <- colorRampPalette(c("white","black"))

layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
image(mat, col = gray(seq(0,1, by = .1)))
#image(tmp, col = gray(1))

legend_image <- as.raster(matrix(colfunc(20), ncol=1))
#image(tmp, col = gray(seq(0,1, by = .1)))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'correlation')
text(x=1.5, y = seq(0,1,l=5), labels = seq(round(min(mat),2), round(max(mat), 2),l=5))
rasterImage(legend_image, 0, 0, 1,1)
}



#modified this code from first answer
#https://stackoverflow.com/questions/13355176/gradient-legend-in-base

#potential other function that adapts second answer
#legend currently wrong
# layout(matrix(1:2,nrow=1),widths=c(0.8,0.2))
# colfunc <- colorRampPalette(c("white","black"))
# 
# par(mar=c(5.1,4.1,4.1,2.1))
# image(tmp, col = gray(seq(0,1, by = .1)))
# 
# #plot(1:10,ann=FALSE,type="n")
# #grid()
# #points(1:10,col=colfunc(10),pch=19,cex=1.5)
# 
# 
# xl <- 1
# yb <- 1
# xr <- 1.5
# yt <- 2
# 
# par(mar=c(5.1,0.5,4.1,0.5))
# plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
# #image(tmp, col = gray(seq(0,1, by = .1)))
# rect(
#      xl,
#      head(seq(yb,yt,(yt-yb)/10),-1),
#      xr,
#      tail(seq(yb,yt,(yt-yb)/10),-1),
#      col=colfunc(10)
#     )
# 
# mtext(round(min(tmp),2):round(max(tmp), 2),side=2,at=tail(seq(yb,yt,(yt-yb)/10),-1)-0.05,las=2,cex=0.7)