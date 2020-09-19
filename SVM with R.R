install.packages("e1071")

library("e1071")

data(iris)
head(iris)

summary(iris)
str(iris)

x11(width = 8, height = 8) #to create a pop up window
plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
legend("topright", legend=c("setosa", "versicolor", "virginica"), col=c("black", "red", "green"), pch=1, cex=0.8)

x11(width = 8, height = 8) #to create a pop up window
plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)
legend("topright", legend=c("setosa", "versicolor", "virginica"), col=c("black", "red", "green"), pch=1, cex=0.8)

set.seed(20)

s1<-sample(150,100)
s1
input_v <-c("Petal.Length","Petal.Width","Species")
iris_train<-iris[s1,input_v]
head(iris_train)
iris_test<- iris[-s1,input_v]
head(iris_test)

tuned<-tune(svm,Species~.,data = iris_train,kernel = "linear", ranges = list(cost = c(.001,0.01,0.1,1,5,10,100)))
summary(tuned)
model<-svm(Species~.,data = iris_train,kernel="linear",cost=0.1, scale=TRUE)
print(model)

plot(model,iris_train[,input_v])

head(iris_test[,input_v[-3]]) #remove species from the test data

#Predicts what species it is based on input variables "Petal.Length","Petal.Width"
p<-predict(model,iris_test[,input_v[-3]],type="class")
p

iris_test[,3]

table(p,iris_test[,3])
mean(p==iris_test[,3])