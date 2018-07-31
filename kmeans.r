library(magick)

img = "pic.jpg"  #"hqdefault.jpg"
xs = 8
ys = 8
cset = 256

pic = image_read(img)
pic = image_scale(pic,c(320,200))
pic = image_convert(pic,colorspace="gray")

data = pic[[1]]
mdata = NULL
for (x in seq(1,320,xs))
{
  for (y in seq(1,200,ys))
  {
    mdata = c(mdata, as.integer(data[,x:(x+xs-1),y:(y+ys-1)]))
  }
}
m = matrix(mdata,nrow=xs*ys)

cl = kmeans(t(m),cset,algorithm="Hartigan-Wong",iter.max = 1000)

i = 1
for (x in seq(1,320,xs))
{
  for (y in seq(1,200,ys))
  {
    clust = cl$cluster[i]
    data[,x:(x+xs-1),y:(y+ys-1)] = as.raw(cl$centers[clust,])
    i = i + 1
  }
}

pic2 = image_read(data)

image_write(pic2,"pic2.png","PNG")
