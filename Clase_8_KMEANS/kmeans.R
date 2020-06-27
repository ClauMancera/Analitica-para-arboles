file <- link
im <- load.image(file)
bdf <- as.data.frame(im, wide="c")

set.seed(123)
k2 <- kmeans(bdf, centers = 5, nstart = 25)
C1 <- rgb(k2$centers[1,3],k2$centers[1,4],k2$centers[1,5])
C2 <- rgb(k2$centers[2,3],k2$centers[2,4],k2$centers[2,5])
C3 <- rgb(k2$centers[3,3],k2$centers[3,4],k2$centers[3,5])
C4 <- rgb(k2$centers[4,3],k2$centers[4,4],k2$centers[4,5])
C5 <- rgb(k2$centers[5,3],k2$centers[5,4],k2$centers[5,5])