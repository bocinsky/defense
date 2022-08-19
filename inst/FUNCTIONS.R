library(xts)
library(MASS)

# A method to plot rasters
raster.png <- function(x,palette,file.name,title,legend.lab,extra.vector.plots.function) {
  image.width <- 6
  image.height <- 6
  x <- setMinMax(x)
  
  ratio.raster <- ncol(x)/nrow(x)

  if(ratio.raster>=1){
  	image.width <- 6
  	image.height <- max(image.width*(1/ratio.raster),4)
  }else{
  	image.height <- 6
  	image.width <- image.height*ratio.raster
  }

  colors <- colorRampPalette(palette)(as.integer(as.integer(maxValue(x)-minValue(x))+1))
  quartz(file=paste(output,'FIGS/',file.name,'.png',sep=''), width=image.width, height=image.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=1, dpi=2400)
    if(ratio.raster>=1){
  	plot.width <- 5
  	plot.height <- plot.width*(1/ratio.raster)
  	inch <- (extent(x)@xmax-extent(x)@xmin)/(image.width-1)
  	par(mai=c((image.height-plot.height)/2,0.5,(image.height-plot.height)/2,0.5),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }else{
  	plot.height <- 5
  	plot.width <- plot.height*ratio.raster
  	inch <- (extent(x)@ymax-extent(x)@ymin)/(image.height-1)
  	par(mai=c(0.5,(image.width-plot.width)/2,0.5,(image.width-plot.width)/2),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }
  par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', family='Helvetica Bold',lend=2,ljoin=1)
  plot(x,xlim=c(extent(x)@xmin,extent(x)@xmax),ylim=c(extent(x)@ymin,extent(x)@ymax),xlab="", ylab="", axes=FALSE, main='',asp=1, col=colors, useRaster=FALSE, legend=FALSE)
  
  plot(americas, col='gray90',add=T, ljoin=0)
  plot(x,add=T,col=colors, useRaster=FALSE, legend=FALSE)
  plot(americas, add=T, ljoin=0)
  plot(waterbodies,add=T, ljoin=0)
  
  
 # plot(x,add=T,col=colors, useRaster=FALSE, legend=FALSE)
  xseq <- c(seq(extent(x)@xmin, extent(x)@xmax, by=20000),extent(x)@xmax)
  yseq <- c(seq(extent(x)@ymin, extent(x)@ymax, by=20000),extent(x)@ymax)
  
  if(abs(diff(tail(xseq, n=2)))<15000){
    xseq <- xseq[-(length(xseq)-1)]
  }
  
  if(abs(diff(tail(yseq, n=2)))<15000){
    yseq <- yseq[-(length(yseq)-1)]
  }
  
  par(tck=(-1*(0.01)), family="Helvetica", lend=2)
  axis(3, at=xseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@ymax), labels=FALSE)
  text(xseq, extent(x)@ymax+(0.05*inch), labels = c(xseq[-length(xseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  axis(4, at=yseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@xmax), labels=FALSE)
  text(extent(x)@xmax+(0.05*inch), yseq, labels = c(yseq[-length(yseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  
  text(extent(x)@xmax, extent(x)@ymax+(0.05*inch), labels = extent(x)@xmax, srt = 90, pos = 4, xpd = TRUE, cex=6*(7/8))
  text(extent(x)@xmax+(0.05*inch), extent(x)@ymax,labels = extent(x)@ymax, srt = 0, pos = 4, xpd = TRUE, cex=6*(7/8))
  
  plot(sim.poly, add=TRUE)
#   plot(coastline, add=T, ljoin=0)
#   plot(lakes, col='white',add=T, ljoin=0)
#   plot(rivers, add=T, ljoin=0)
  
  plot(borders2, add=T,lty=1,lwd=4,col='white',lend=2)
  plot(borders3, add=T,lty=1,lwd=4,col='white',lend=2)
  plot(borders, add=T,lty=1,lwd=2.5,col='white',lend=2)
  plot(borders, add=T,lty=2,lwd=1.5,lend=2)
#   plot(americas, add=T)
  
  plot(sim.poly, add=TRUE)
  
  if(!is.na(extra.vector.plots.function)){
  	FUN <- match.fun(extra.vector.plots.function)
  	FUN()
  }
  
  par(bty='o', tck=-(1/4))
  plot(x, add=TRUE, legend.only=TRUE, col=colors, smallplot= c(0.5,(((image.width-plot.width)/2)+plot.width)/image.width,(((image.height-plot.height)/2)-0.15)/image.height,(((image.height-plot.height)/2)-0.05)/image.height), horiz=TRUE, axis.args=list(cex.axis=6*(7/8), lwd=0, lwd.ticks=0.15*6,pos=-0.25, padj=1.1,bty='o'), legend.args=list(text=legend.lab,side=1,font=2,line=12,cex=6))
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.098*inch)), labels=title, pos = 4, xpd = TRUE, cex=8,font=2)
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.21*inch)), labels='R. Kyle Bocinsky', pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.28*inch)), labels=format(Sys.Date(), "%d %B %Y"), pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.35*inch)), labels=paste('Coordinate Reference: ',projection(x),sep=''), pos = 4, xpd = TRUE, cex=4,font=2)
  
  dev.off()
}

# A method to plot rasters
raster.small.png <- function(x,ext,palette,file.name,title,legend.lab,extra.vector.plots.function) {
  e <- extent(ext)
  x <- crop(x,e)
  image.width <- 6
  image.height <- 6
  x <- setMinMax(x)
  
  ratio.raster <- ncol(x)/nrow(x)

  if(ratio.raster>=1){
  	image.width <- 6
  	image.height <- max(image.width*(1/ratio.raster),4)
  }else{
  	image.height <- 6
  	image.width <- image.height*ratio.raster
  }

  colors <- colorRampPalette(palette)(as.integer(as.integer(maxValue(x)-minValue(x))+1))
  quartz(file=paste(output,'FIGS/',file.name,'.png',sep=''), width=image.width, height=image.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=1, dpi=2400)
    if(ratio.raster>=1){
  	plot.width <- 5
  	plot.height <- plot.width*(1/ratio.raster)
  	inch <- (extent(x)@xmax-extent(x)@xmin)/(image.width-1)
  	par(mai=c((image.height-plot.height)/2,0.5,(image.height-plot.height)/2,0.5),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }else{
  	plot.height <- 5
  	plot.width <- plot.height*ratio.raster
  	inch <- (extent(x)@ymax-extent(x)@ymin)/(image.height-1)
  	par(mai=c(0.5,(image.width-plot.width)/2,0.5,(image.width-plot.width)/2),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }
  par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', family='Helvetica Bold',lend=2,ljoin=1)
  plot(x,xlim=c(extent(x)@xmin,extent(x)@xmax),ylim=c(extent(x)@ymin,extent(x)@ymax),xlab="", ylab="", axes=FALSE, main='',asp=1, col=colors, useRaster=FALSE, legend=FALSE)


  
  
 # plot(x,add=T,col=colors, useRaster=FALSE, legend=FALSE)
  xseq <- c(seq(extent(x)@xmin, extent(x)@xmax, by=600),extent(x)@xmax)
  yseq <- c(seq(extent(x)@ymin, extent(x)@ymax, by=600),extent(x)@ymax)
  
  if(abs(diff(tail(xseq, n=2)))<300){
    xseq <- xseq[-(length(xseq)-1)]
  }
  
  if(abs(diff(tail(yseq, n=2)))<300){
    yseq <- yseq[-(length(yseq)-1)]
  }
  
  for(i in xseq){
  	abline(v=i, col='white', lty=1, cex=0.8)
  }
  
  for(i in yseq){
  	abline(h=i, col='white', lty=1, cex=0.8)
  }
  
  plot(waterbodies,add=T, col="white", ljoin=0)
  
  par(tck=(-1*(0.01)), family="Helvetica", lend=2)
  axis(3, at=xseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@ymax), labels=FALSE)
  text(xseq, extent(x)@ymax+(0.05*inch), labels = c(xseq[-length(xseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  axis(4, at=yseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@xmax), labels=FALSE)
  text(extent(x)@xmax+(0.05*inch), yseq, labels = c(yseq[-length(yseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  
  text(extent(x)@xmax, extent(x)@ymax+(0.05*inch), labels = extent(x)@xmax, srt = 90, pos = 4, xpd = TRUE, cex=6*(7/8))
  text(extent(x)@xmax+(0.05*inch), extent(x)@ymax,labels = extent(x)@ymax, srt = 0, pos = 4, xpd = TRUE, cex=6*(7/8))
  
	box(ljoin=0)
  
  if(!is.na(extra.vector.plots.function)){
  	FUN <- match.fun(extra.vector.plots.function)
  	FUN()
  }
  
  par(bty='o', tck=-(1/4))
  plot(x, add=TRUE, legend.only=TRUE, col=colors, smallplot= c(0.5,(((image.width-plot.width)/2)+plot.width)/image.width,(((image.height-plot.height)/2)-0.15)/image.height,(((image.height-plot.height)/2)-0.05)/image.height), horiz=TRUE, axis.args=list(cex.axis=6*(7/8), lwd=0, lwd.ticks=0.15*6,pos=-0.25, padj=1.1,bty='o'), legend.args=list(text=legend.lab,side=1,font=2,line=12,cex=6))
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.098*inch)), labels=title, pos = 4, xpd = TRUE, cex=8,font=2)
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.21*inch)), labels='R. Kyle Bocinsky', pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.28*inch)), labels=format(Sys.Date(), "%d %B %Y"), pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.35*inch)), labels=paste('Coordinate Reference: ',projection(x),sep=''), pos = 4, xpd = TRUE, cex=4,font=2)
  
  dev.off()
}


# A method to plot index (0,1) rasters
index.raster.png <- function(x,palette,file.name,title,legend.lab,extra.vector.plots.function) {
  image.width <- 6
  image.height <- 6
  x <- setMinMax(x)
  
  ratio.raster <- ncol(x)/nrow(x)

  if(ratio.raster>=1){
  	image.width <- 6
  	image.height <- max(image.width*(1/ratio.raster),4)
  }else{
  	image.height <- 6
  	image.width <- image.height*ratio.raster
  }

  colors <- colorRampPalette(palette)(1000)
  quartz(file=paste(output,'FIGS/',file.name,'.png',sep=''), width=image.width, height=image.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=1, dpi=2400)
    if(ratio.raster>=1){
  	plot.width <- 5
  	plot.height <- plot.width*(1/ratio.raster)
  	inch <- (extent(x)@xmax-extent(x)@xmin)/(image.width-1)
  	par(mai=c((image.height-plot.height)/2,0.5,(image.height-plot.height)/2,0.5),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }else{
  	plot.height <- 5
  	plot.width <- plot.height*ratio.raster
  	inch <- (extent(x)@ymax-extent(x)@ymin)/(image.height-1)
  	par(mai=c(0.5,(image.width-plot.width)/2,0.5,(image.width-plot.width)/2),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }
  par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', family='Helvetica Bold',lend=2,ljoin=1)
  plot(x,xlim=c(extent(x)@xmin,extent(x)@xmax),ylim=c(extent(x)@ymin,extent(x)@ymax),xlab="", ylab="", axes=FALSE, main='',asp=1, zlim=c(0,1),col=colors, useRaster=FALSE, legend=FALSE)
  
  plot(americas,col='gray90',add=T, ljoin=0)
  plot(x,add=T,col=colors, useRaster=FALSE, legend=FALSE,zlim=c(0,1))
  plot(americas, add=T, ljoin=0)
  plot(waterbodies,add=T, ljoin=0)
  
  xseq <- c(seq(extent(x)@xmin, extent(x)@xmax, by=20000),extent(x)@xmax)
  yseq <- c(seq(extent(x)@ymin, extent(x)@ymax, by=20000),extent(x)@ymax)
  
  if(abs(diff(tail(xseq, n=2)))<15000){
    xseq <- xseq[-(length(xseq)-1)]
  }
  
  if(abs(diff(tail(yseq, n=2)))<15000){
    yseq <- yseq[-(length(yseq)-1)]
  }
  
  par(tck=(-1*(0.01)), family="Helvetica", lend=2)
  axis(3, at=xseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@ymax), labels=FALSE)
  text(xseq, extent(x)@ymax+(0.05*inch), labels = c(xseq[-length(xseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  axis(4, at=yseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@xmax), labels=FALSE)
  text(extent(x)@xmax+(0.05*inch), yseq, labels = c(yseq[-length(yseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  
  text(extent(x)@xmax, extent(x)@ymax+(0.05*inch), labels = extent(x)@xmax, srt = 90, pos = 4, xpd = TRUE, cex=6*(7/8))
  text(extent(x)@xmax+(0.05*inch), extent(x)@ymax,labels = extent(x)@ymax, srt = 0, pos = 4, xpd = TRUE, cex=6*(7/8))
  
  plot(sim.poly, add=TRUE)
  #   plot(coastline, add=T, ljoin=0)
  #   plot(lakes, col='white',add=T, ljoin=0)
  #   plot(rivers, add=T, ljoin=0)
  
  plot(borders2, add=T,lty=1,lwd=4,col='white',lend=2)
  plot(borders3, add=T,lty=1,lwd=4,col='white',lend=2)
  plot(borders, add=T,lty=1,lwd=2.5,col='white',lend=2)
  plot(borders, add=T,lty=2,lwd=1.5,lend=2)
  #   plot(americas, add=T)
  
  plot(sim.poly, add=TRUE)
  
  if(!is.na(extra.vector.plots.function)){
  	FUN <- match.fun(extra.vector.plots.function)
  	FUN()
  }
  
  par(bty='o', tck=-(1/4))
  plot(x, add=TRUE, legend.only=TRUE, col=colors, zlim=c(0,1),smallplot= c(0.5,(((image.width-plot.width)/2)+plot.width)/image.width,(((image.height-plot.height)/2)-0.15)/image.height,(((image.height-plot.height)/2)-0.05)/image.height), horiz=TRUE, axis.args=list(cex.axis=6*(7/8), lwd=0, lwd.ticks=0.15*6,pos=-0.25, padj=1.1,bty='o'), legend.args=list(text=legend.lab,side=1,font=2,line=12,cex=6))
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.098*inch)), labels=title, pos = 4, xpd = TRUE, cex=8,font=2)
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.21*inch)), labels='R. Kyle Bocinsky', pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.28*inch)), labels=format(Sys.Date(), "%d %B %Y"), pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.35*inch)), labels=paste('Coordinate Reference: ',projection(x),sep=''), pos = 4, xpd = TRUE, cex=4,font=2)
  
  dev.off()
}

# A method to plot index (0,1) rasters
index.raster.small.png <- function(x,ext,palette,file.name,title,legend.lab,extra.vector.plots.function) {
  e <- extent(ext)
  x <- crop(x,e)
  image.width <- 6
  image.height <- 6
  x <- setMinMax(x)
  
  ratio.raster <- ncol(x)/nrow(x)

  if(ratio.raster>=1){
  	image.width <- 6
  	image.height <- max(image.width*(1/ratio.raster),4)
  }else{
  	image.height <- 6
  	image.width <- image.height*ratio.raster
  }

  colors <- colorRampPalette(palette)(1000)
  quartz(file=paste(output,'FIGS/',file.name,'.png',sep=''), width=image.width, height=image.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=1, dpi=2400)
    if(ratio.raster>=1){
  	plot.width <- 5
  	plot.height <- plot.width*(1/ratio.raster)
  	inch <- (extent(x)@xmax-extent(x)@xmin)/(image.width-1)
  	par(mai=c((image.height-plot.height)/2,0.5,(image.height-plot.height)/2,0.5),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }else{
  	plot.height <- 5
  	plot.width <- plot.height*ratio.raster
  	inch <- (extent(x)@ymax-extent(x)@ymin)/(image.height-1)
  	par(mai=c(0.5,(image.width-plot.width)/2,0.5,(image.width-plot.width)/2),omi=c(0,0,0,0),pin=c(plot.width,plot.height))
  }
  par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', family='Helvetica Bold',lend=2,ljoin=1)
  plot(x,xlim=c(extent(x)@xmin,extent(x)@xmax),ylim=c(extent(x)@ymin,extent(x)@ymax),xlab="", ylab="", axes=FALSE, main='',asp=1, zlim=c(0,1),col=colors, useRaster=FALSE, legend=FALSE)
  
  xseq <- c(seq(extent(x)@xmin, extent(x)@xmax, by=600),extent(x)@xmax)
  yseq <- c(seq(extent(x)@ymin, extent(x)@ymax, by=600),extent(x)@ymax)
  
  if(abs(diff(tail(xseq, n=2)))<300){
    xseq <- xseq[-(length(xseq)-1)]
  }
  
  if(abs(diff(tail(yseq, n=2)))<300){
    yseq <- yseq[-(length(yseq)-1)]
  }
  
    for(i in xseq){
  	abline(v=i, col='white', lty=1, cex=0.8)
  }
  
  for(i in yseq){
  	abline(h=i, col='white', lty=1, cex=0.8)
  }
  
  plot(waterbodies,add=T, col="white", ljoin=0)
  
  par(tck=(-1*(0.01)), family="Helvetica", lend=2)
  axis(3, at=xseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@ymax), labels=FALSE)
  text(xseq, extent(x)@ymax+(0.05*inch), labels = c(xseq[-length(xseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  axis(4, at=yseq, cex.axis=6*(7/8), lwd=-1, lwd.ticks=1, padj=-.4, pos=(extent(x)@xmax), labels=FALSE)
  text(extent(x)@xmax+(0.05*inch), yseq, labels = c(yseq[-length(yseq)],''), srt = 45, pos = 4, xpd = TRUE, cex=6*(7/8))
  
  text(extent(x)@xmax, extent(x)@ymax+(0.05*inch), labels = extent(x)@xmax, srt = 90, pos = 4, xpd = TRUE, cex=6*(7/8))
  text(extent(x)@xmax+(0.05*inch), extent(x)@ymax,labels = extent(x)@ymax, srt = 0, pos = 4, xpd = TRUE, cex=6*(7/8))
  
#  plot(sim.poly, add=TRUE)
  
  box(ljoin=0)
  
  if(!is.na(extra.vector.plots.function)){
  	FUN <- match.fun(extra.vector.plots.function)
  	FUN()
  }
  
  par(bty='o', tck=-(1/4))
  plot(x, add=TRUE, legend.only=TRUE, col=colors, zlim=c(0,1),smallplot= c(0.5,(((image.width-plot.width)/2)+plot.width)/image.width,(((image.height-plot.height)/2)-0.15)/image.height,(((image.height-plot.height)/2)-0.05)/image.height), horiz=TRUE, axis.args=list(cex.axis=6*(7/8), lwd=0, lwd.ticks=0.15*6,pos=-0.25, padj=1.1,bty='o'), legend.args=list(text=legend.lab,side=1,font=2,line=12,cex=6))
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.098*inch)), labels=title, pos = 4, xpd = TRUE, cex=8,font=2)
  
  text(extent(x)@xmin,(extent(x)@ymin-(0.21*inch)), labels='R. Kyle Bocinsky', pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.28*inch)), labels=format(Sys.Date(), "%d %B %Y"), pos = 4, xpd = TRUE, cex=4,font=2)
  text(extent(x)@xmin,(extent(x)@ymin-(0.35*inch)), labels=paste('Coordinate Reference: ',projection(x),sep=''), pos = 4, xpd = TRUE, cex=4,font=2)
  
  dev.off()
}




hist.pdf <- function(x,file.name,x.lab,extra.plots.function) {
	pdf(file=paste(output,'FIGS/',file.name,'.pdf',sep=''), width=6, height = 6*(1/1.61), bg="white", paper="special", family="Helvetica", pointsize=8)
	y.ceil <- ceiling(max(x$density*100))/100
	while(round((round(y.ceil*100,digits=0))%%5,digits=2)!=0){
		y.ceil <- y.ceil+0.01
	}
	par(mar=c(4.5,4.5,1.2,1.2), oma=c(0,0,0,0), lend=2,ljoin=1)
	plot(1, type='n', xlab="", ylab="", xlim=c(first(x$breaks),last(x$breaks)),ylim=c(0,y.ceil),xaxs="i", yaxs="i", axes=FALSE, main='')
	plot(x,freq=F,add=T)
	if(!is.na(extra.plots.function)){
  		FUN <- match.fun(extra.plots.function)
  		FUN()
  	}
	axis(1)
	axis(2, las=1)
	box()
	mtext(x.lab, side=1, line=3, cex=1.25)
	mtext("Empirical Probability", side=2, line=3.2, cex=1.25)
	suppressWarnings(try(minor.tick(nx=2, ny=2, tick.ratio=0.5), silent=T))
	dev.off()
}

hist.compare.pdf <- function(hist.1,hist.2,lab.1,lab.2,x.lab,file.name,extra.plots.function) {
	pdf(file=paste(output,'FIGS/',file.name,'.pdf',sep=''), width=6, height = 6*(1/1.61), bg="white", paper="special", family="Helvetica", pointsize=8)
	y.ceil.1 <- ceiling(max(hist.1$density*100))/100
	while(round((round(y.ceil.1*100,digits=0))%%5,digits=2)!=0){
		y.ceil.1 <- y.ceil.1+0.01
	}
		y.ceil.2 <- ceiling(max(hist.2$density*100))/100
	while(round((round(y.ceil.2*100,digits=0))%%5,digits=2)!=0){
		y.ceil.2 <- y.ceil.2+0.01
	}
	
	y.ceil <- max(y.ceil.1,y.ceil.2)
	
	par(mar=c(4.5,4.5,1.2,1.2), oma=c(0,0,0,0), lend=2,ljoin=1)
	plot(1, type='n', xlab="", ylab="", xlim=c(first(hist.1$breaks),last(hist.1$breaks)),ylim=c(0,y.ceil),xaxs="i", yaxs="i", axes=FALSE, main='')
	
	plot(hist.1,freq=F,add=T, col='#00000095')
	plot(hist.2,freq=F,add=T, col='#FFFFFF95')
	
	if(!is.na(extra.plots.function)){
  		FUN <- match.fun(extra.plots.function)
  		FUN()
  	}
  	
	axis(1)
	axis(2, las=1)
	box()
	legend("topleft", y=NULL, legend=c(lab.1,lab.2), bty="n", fill = c('#00000095','#FFFFFF95'),cex=1,ncol=1)
	mtext(x.lab, side=1, line=3, cex=1.25)
	mtext("Empirical Probability", side=2, line=3.2, cex=1.25)
	suppressWarnings(try(minor.tick(nx=2, ny=2, tick.ratio=0.5), silent=T))
	dev.off()
}

ecdf.compare.pdf <- function(sample,population,sample.lab,population.lab,x.lab,file.name,extra.plots.function) {
	pdf(file=paste(output,'FIGS/',file.name,'.pdf',sep=''), width=6, height = 6*(1/1.61), bg="white", paper="special", family="Helvetica", pointsize=8)
	par(mar=c(4.5,4.5,1.2,1.2), oma=c(0,0,0,0), lend=2,ljoin=1)
	
	f.sample <- ecdf(sample)
	f.population <- ecdf(population)
	
	ks.stat <- ks.test(sample, f.population)$statistic
	
	x <- seq(0,1,length.out=100000)
	x0 <- mean(x[which(abs(f.sample(x)-f.population(x))==max(abs(f.sample(x)-f.population(x))))])
	y0 <- f.sample(x0)
	y1 <- f.population(x0)
	
	yD <- mean(max(y0,y1))
	xD <- mean(x[which(abs(f.population(x)-yD)==min(abs(f.population(x)-yD)))])
	
	x95 <- mean(x[which(abs(f.population(x)-0.95)==min(abs(f.population(x)-0.95)))])
	
	if(length(sample)>100000){
		sample <- sample(sample,100000)
	}
	
	if(length(population)>100000){
		population <- sample(population,100000)
	}
	
	plot(1, type='n', xlab="", ylab="", xlim=c(0,1),ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, main='')
	
	segments(x0,y0,x0,y1,lty=3)
	segments(0,0.95,x95,0.95, lty=2)
	segments(x95,0.95,x95,0, lty=2)
	
	plot(ecdf(sample), do.points=F, verticals=T,add=T, col='red')
	plot(ecdf(population), do.points=F, verticals=T,add=T)
	
	text(x=xD,y=yD+0.05,substitute(D[max]==ks.stat, list(ks.stat = round(ks.stat,digits=3))),pos=2)
	
	text(x=x95,y=0.1,substitute(I[95]==x95, list(x95 = round(x95,digits=3))),pos=4)
	
	if(!is.na(extra.plots.function)){
  		FUN <- match.fun(extra.plots.function)
  		FUN()
  	}
  	z <- sort(c(0,0.2,0.4,0.6,0.8,1.0,x95))
	axis(1, at=z, labels=round(z,digits=2))
	axis(2, at=c(0,0.2,0.4,0.6,0.8,1.0,0.95), las=1)
	box()
	legend(x=0,y=0.9, legend=c(sample.lab,population.lab), bty="n", fill = c('red','black'),cex=1,ncol=1)
	mtext(x.lab, side=1, line=3, cex=1.25)
	mtext("Cumulative Probability", side=2, line=3.2, cex=1.25)
	suppressWarnings(try(minor.tick(nx=2, ny=2, tick.ratio=0.5), silent=T))
	dev.off()
}


index.compare.pdf <- function(x,y,x.lab,y.lab,file.name,extra.plots.function) {
	pdf(file=paste(output,'FIGS/',file.name,'.pdf',sep=''), width=6, height = 6, bg="white", paper="special", family="Helvetica", pointsize=8)
	par(mar=c(4.5,4.5,1.2,1.2), oma=c(0,0,0,0), lend=2,ljoin=1)
	
	plot(1, type='n', xlab="", ylab="", xlim=c(0,1),ylim=c(0,1),xaxs="i", yaxs="i", axes=FALSE, main='')
	
	points(x,y, pch=19, cex=1, col='black')
	abline(rlm(y~x), lty=2)
	
	curve(1*x,0,1, add=TRUE, lty=1)
	axis(1)
	axis(2, las=1)
	box()
	mtext(x.lab, side=1, line=3, cex=1.25)
	mtext(y.lab, side=2, line=3.2, cex=1.25)

	suppressWarnings(try(minor.tick(nx=2, ny=2, tick.ratio=0.5), silent=T))
	dev.off()
	summary(rlm(y~x),correlation=TRUE)$correlation
}


