library(caret); library(gbm); library(survival); library(splines); library(plyr)

set.seed(1)

# Set directory
dir = "local_directory"
setwd(dir)
training = read.csv("pml-training.csv")
testMain = read.csv("pml-testing.csv")

training <- training[,colSums(is.na(training)) == 0] 
columns = colnames(training)

inTrain = createDataPartition(y=training$classe,p=0.75,list=FALSE)
train = training[inTrain,]
test = training[-inTrain,]

# parameter setting 
fitControl <- trainControl(method  = "repeatedcv",
                           number  = 7,
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

varImp(modFit, scale = FALSE)
outturn = predict(modFit,test)

# ratio
length(which(outturn == test$classe))/nrow(test)

# prediction
predictions = predict(modFit,testMain)
(predictions)
