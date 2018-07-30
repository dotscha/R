pic = image_read("pic.jpg")
pic = image_scale(pic,c(320,200))
pic = image_convert(pic,colorspace="gray")

xs = 8
ys = 8

data = pic[[1]]
mdata = c()
for (x in seq(1,320,xs))
{
  for (y in seq(1,200,ys))
  {
    mdata = c(mdata, as.integer(data[,x:(x+xs-1),y:(y+ys-1)]))
  }
}
m = matrix(mdata,ncol=xs*ys)

cl = kmeans(m,256,iter.max = 1000)

i = 1
for (x in seq(1,320,xs))
{
  for (y in seq(1,200,ys))
  {
    cluster = cl$cluster[i]
    data[,x:(x+xs-1),y:(y+ys-1)] = as.raw(cl$centers[cluster,])
    i = i + 1
  }
}

pic2 = image_read(data)
