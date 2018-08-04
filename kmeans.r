if (!"magick" %in% .packages(all=1))
{
  install.packages("magick")
}
library(magick)

img = "pic.jpg"
w = 320
h = 200
xs = 8
ys = 8
cset = 256

pic = image_read(img)
pic = image_resize(pic,paste0(w,"x",h,"!"))
pic = image_convert(pic,colorspace="gray")

data = pic[[1]]
mdata = NULL
for (dx in c(0))
  for (x in seq(1,w,xs))
    for (y in seq(1,h,ys))
      mdata = c(mdata, as.integer(data[,((x:(x+xs-1)-1+dx)%%w)+1,y:(y+ys-1)]))

m = matrix(mdata,nrow=xs*ys)

cl = kmeans(t(m),cset,algorithm="Hartigan-Wong",iter.max = 100)
#cl = kmeans(t(m),cset,iter.max = 100)

i = 1
for (x in seq(1,w,xs))
{
  for (y in seq(1,h,ys))
  {
    clust = cl$cluster[i]
    data[,x:(x+xs-1),y:(y+ys-1)] = as.raw(cl$centers[clust,])
    i = i + 1
  }
}

pic2 = image_read(data)

image_write(pic,"pic1.png","PNG")
image_write(pic2,"pic2.png","PNG")
