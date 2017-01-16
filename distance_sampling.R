library(Distance)

data(book.tee.data)
tee.data<-book.tee.data$book.tee.dataframe[book.tee.data$book.tee.dataframe$observer==1,]
ds.model <- ds(tee.data,4)
summary(ds.model)
plot(ds.model)

ds.minke<-ds(minke, truncation="10%")
plot(ds.minke)
summary(ds.minke)

ds.gof(ds.minke)
