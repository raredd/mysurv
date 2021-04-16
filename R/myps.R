myps <- function(file,pointsize=20,family='Helvetica',lwd=2.5,...){
  postscript(paste(file,'.ps',sep=''),pointsize=pointsize,family=family,...)
  par(lwd=lwd,font=2,font.axis=2,font.lab=2,font.main=2,font.sub=2,las=1,ljoin=1,lend=2,...)
}
mypsu <- function(file,pointsize=20,family='Helvetica',lwd=2.5,...){
  postscript(paste(file,'.ps',sep=''),pointsize=pointsize,family=family,...)
  par(lwd=lwd,font=2,font.axis=2,font.lab=2,font.main=2,font.sub=2,las=1,...)
}
mywmf <- function(file,path=NULL,pointsize=18,lwd=4,maincol='white',width=8,height=6,...) {
  win.metafile(paste(path,file,'.emf',sep=''),pointsize=pointsize,width=width,height=height)
  par(font=2,lwd=lwd,fg=maincol,col=maincol,col.axis=maincol,col.main=maincol,col.lab=maincol,col.sub=maincol,font.axis=2,font.lab=2,font.main=2,font.sub=2,las=1,ljoin=1,lend=2,...)
}
mypdf <- function (file, pointsize = 20, family = "Helvetica", lwd = 2.5, 
    width=10.5, height=8.0, maincol='black', ...) 
{
    pdf(paste(file, ".pdf", sep = ""), pointsize = pointsize, 
        family = family, width=width, height=height, ...)
    par(lwd = lwd, font = 2, font.axis = 2, font.lab = 2, font.main = 2, 
        font.sub = 2, fg=maincol, col=maincol, col.axis=maincol,
        col.main=maincol,col.lab=maincol,col.sub=maincol,
        las = 1,ljoin=1,lend=2,...)
}

mybitmap <- function (file, type=c('png','tiff','jpeg'),path = NULL, 
  pointsize = 14, lwd = 2,maincol = "black", width = 640, height = 480,
  font=2, ...) {
  type=match.arg(type)
  do.call(type,list(filename=paste(path, file,'.',type, sep = ""), 
                    pointsize = pointsize,width = width, height = height))
#    tiff(paste(path, file, ".tiff", sep = ""), pointsize = pointsize, 
#        width = width, height = height)
# use background='transparent' with png
  par(font=font, lwd = lwd, fg = maincol, col = maincol, col.axis = maincol, 
    col.main = maincol, col.lab = maincol, col.sub = maincol, 
    font.axis=font, font.lab=font, font.main=font, font.sub=font, 
    las = 1, ljoin = 1, lend=font, ...)
}
