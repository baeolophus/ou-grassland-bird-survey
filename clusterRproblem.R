library(randomForest)
library(party)
library(raster)
library(microbenchmark)

iris.rf <- randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris, replace = FALSE)
iris.cf <- cforest(Species ~ ., data = iris, control = cforest_unbiased(mtry = 3))

m <- cforest(pa~., control=cforest_unbiased(mtry=3), data=v)
f <- list(levels(v$red))
names(f) <- 'red'
pc <- predict(logo, m, OOB=TRUE, factors=f)

Sepal.Length <- raster()
Sepal.Length[] <- seq(from = min(iris$Sepal.Length), to = max(iris$Sepal.Length), length.out = ncell(Sepal.Length))
Sepal.Width <- raster()
Sepal.Width[] <- seq(from = min(iris$Sepal.Width), to = max(iris$Sepal.Width), length.out = ncell(Sepal.Width))
Petal.Length <- raster()
Petal.Length[] <- seq(from = min(iris$Petal.Length), to = max(iris$Petal.Length), length.out = ncell(Petal.Length))
Petal.Width <- raster()
Petal.Width[] <- seq(from = min(iris$Petal.Width), to = max(iris$Petal.Width), length.out = ncell(Petal.Width))

beginCluster()
r <- stack(Sepal.Length,
           Sepal.Width,
           Petal.Length,
           Petal.Width)
predicted.r <- raster::predict(object = r,
                               model = iris.rf,
                               progress = "text")
