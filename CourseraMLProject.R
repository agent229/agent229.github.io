library(caret); library(rattle); library(e1071)

# File writing (answers in character vector, call: pml_write_files(answers))
pml_write_files = function(x){
	n = length(x)
	for(i in 1:n){
		filename = paste0("problem_id_",i,".txt")
		write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
	}
}

# Import data
trainData <- read.csv("pml-training.csv")
testData <- read.csv("pml-testing.csv")


## Data Cleaning ##

# Remove columns with lots of NAs
rmNA <- colSums(is.na(trainData)) < nrow(trainData)/2
training <- trainData[,rmNA]
testing <- testData[,rmNA]
# Remove columns 1 through 7 (irrelevant) - now 86 columns
training <- training[,8:93]
testing <- testing[,8:93]
# Remove Factor variables
rmFac <- sapply(training[,-86], is.numeric)
training <- training[,rmFac]
testing <- testing[,rmFac]

# Create a partition (training/validation)
inTrain <- createDataPartition(y=training$classe,p=0.90,list=FALSE)
train1 <- training[inTrain,]
train2 <- training[-inTrain,]


## Fit Model ##

# fit SVM using e1071 package to predict classe
model <- svm(classe ~., data=train1)


## Training and Validation Prediction/Errors ##

# Predict on train to find in sample error
pred <- predict(model, newdata=train1)
# Find accuracy
correct <- pred==train1$classe
sum(correct)/length(train1$classe)

# test on reserved training set (validation)
pred2 <- predict(model, newdata=train2)
correct <- pred2==train2$classe
sum(correct)/length(train2$classe)


## Testing Prediction ##

# apply to testing set
predTest <- predict(model, newdata=testing[,-53])
answers <- predTest
pml_write_files(answers)