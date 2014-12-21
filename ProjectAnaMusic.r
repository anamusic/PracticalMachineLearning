library(caret); library(gbm); library(survival); library(splines); library(plyr)

set.seed(1)

# Set directory
dir = "C:/Users/domen/Documents/Coursera_MachineLearning/"
setwd(dir)
training = read.csv("pml-training.csv")

# zbrisemo vse stoplce, ki imajo kaksno vrednost = NA
training <- training[,colSums(is.na(training)) == 0] 
columns = colnames(training)

# razdelimo na train in test matriki
inTrain = createDataPartition(y=training$classe,p=0.75,list=FALSE)
train = training[inTrain,]
test = training[-inTrain,]

# parameter setting 
fitControl <- trainControl(method  = "repeatedcv",
                           number  = 5,
                           repeats = 2)


start.time <- Sys.time()
modFit <- train(classe ~ yaw_belt+total_accel_belt+total_accel_arm+accel_belt_z+roll_arm+yaw_arm+gyros_arm_x+gyros_arm_y+accel_arm_z+pitch_forearm+roll_dumbbell,
                method     = "gbm",
                data       = train,
                trControl  = fitControl,
                #preProcess = "pca",
                verbose    = FALSE)
end.time <- Sys.time()
(end.time - start.time)

varImp(modFit, scale = FALSE) # variable importance http://topepo.github.io/caret/varimp.html
outturn = predict(modFit,test)

# stevilno pravilno napovedanih razredov
length(which(outturn == test$classe))/nrow(test)










dummies = dummyVars(classe ~ ., data=training)
head(predict(dummies,newdata=training))



# Correlations
a = 45
b = a+4
featurePlot(x=training[,c(columns[a:b],"classe")], 
            y=training$classe, plot="pairs")
columns[a:b]

# Density functuins
for (i in 1:length(columns) ) {
  qplot(x=training[,columns[i]],colour=classe,data=training,xlab=columns[i])
  ggsave(file=paste(i,columns[i],"_point.png",sep=""))
}

#install.packages("rpart")
#install.packages("e1071")
#install.packages("rJava")
#install.packages("RWeka")

#library(rJava)
library(RWeka)

