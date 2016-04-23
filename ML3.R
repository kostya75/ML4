library(caret)


#get the data into R
fileUrltrain<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url=fileUrltrain,destfile="train_raw.csv")

fileUrltest<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url=fileUrltest,destfile="test_raw.csv")


mainData<-read.csv("train_raw.csv")
finalTesting<-read.csv("test_raw.csv")


#Explore

table(mainData$classe)

set.seed(12345)

#Check for NAs, compare test and train. Same features have NAs. Too many to correct for outliers. Exclude from the list of variables in the model
EmptyVar<-as.data.frame(cbind(apply(mainData,2,function(x) {sum(is.na(x))}),apply(finalTesting,2,function(x) {sum(is.na(x))}),names(mainData)))
names(EmptyVar)<-c("NATrain","NATest","FeatureName")

ValidVars<-as.character(EmptyVar$FeatureName[EmptyVar$NATest==0])

#further reduce the list of vars to exclude non-informative columns like timestamps and user names

ValidVars<-ValidVars[8:59]

mainData<-data.frame(mainData[,ValidVars],classe=mainData$classe)
finalTesting<-finalTesting[,ValidVars]



#partition the original trianing into training validation and testing
inBuild<-createDataPartition(y=mainData$classe, p=0.7, list=FALSE)

#split data in to validation and build
validation<-mainData[-inBuild,]
buildData<-mainData[inBuild,]

#split build into train and test

inTrain<-createDataPartition(y=buildData$classe,p=0.7, list=FALSE)

training<-buildData[inTrain,]
testing<-buildData[-inTrain,]

#preprocess the data by using PCA

preProc<-preProcess(training[,-53],method="pca")

trainPC<-predict(preProc,training)
testPC<-predict(preProc,testing)
validationPC<-predict(preProc,validation)
finalTestingPC<-predict(preProc,finalTesting)


#Models to consider
methods<-c("rf","rpart","nb","lda","gbm","svmRadial")


########
#temp partition for code validaton
#codeval<-createDataPartition(y=trainPC$classe, p=0.1, list=FALSE)
#trainPC<-trainPC[codeval,]
########

AccuracyListTest<-NULL
AccuracyListValidation<-NULL
AccuracyList<-NULL
modList<-list()
PredDF<-testPC$classe
PredVDF<-validation$classe

for (i in 1:length(methods)) { 
    print(methods[i])
    
    if (i==5) {
        modTemp<-train(classe~.,method=methods[i],data=trainPC,verbose = FALSE)
    }
    else {
        modTemp<-train(classe~.,method=methods[i],data=trainPC)  
    }
    #AccuracyTemp<-confusionMatrix(trainPC$classe,predict(modTemp,trainPC))$overall[1]
    #assign(prCounter,predict(modTemp,testPC))
    PredDF<-data.frame(PredDF,predict(modTemp,testPC))
    PredVDF<-data.frame(PredVDF,predict(modTemp,validationPC))
    
    modList[[i]]<-modTemp
    
    Accuracy<-confusionMatrix(trainPC$classe,predict(modTemp,trainPC))$overall[1]
    AccuracyTest<-confusionMatrix(testPC$classe,predict(modTemp,testPC))$overall[1]
    AccuracyValidation<-confusionMatrix(validationPC$classe,predict(modTemp,validationPC))$overall[1]
    
    AccuracyList<-c(AccuracyList,Accuracy)
    AccuracyListTest<-c(AccuracyListTest,AccuracyTest)
    AccuracyListValidation<-c(AccuracyListValidation,AccuracyValidation)
} 

names(PredDF)<-c("classe",methods)
names(PredVDF)<-c("classe",methods)

AccuracyReport<-data.frame(Method=methods,AccuracyTrain=AccuracyList,AccuracyTest=AccuracyListTest,AccuracyValidation=AccuracyListValidation)

#select top 3 models based on Accuracy
top3Index<-order(-AccuracyListTest)[1:3]

#PredDF and PredVDF contain estimates by method but need to mo move column index by one to alighn with models on the list
combModFit<-train(classe~.,method="glmnet",data=PredDF[,c(1,top3Index+1)])

stackedAccuracyTest<-confusionMatrix(testPC$classe,predict(combModFit,PredDF))$overall[1]
stackedAccuracyValidation<-confusionMatrix(validationPC$classe,predict(combModFit,PredVDF))$overall[1]

print(stackedAccuracyTest);print(stackedAccuracyValidation)

#Predict Project Test

finalTestingPCDF<-data.frame(predict(modList[top3Index[1]],finalTestingPC),
                             predict(modList[top3Index[2]],finalTestingPC),
                             predict(modList[top3Index[3]],finalTestingPC))


names(finalTestingPCDF)<-methods[top3Index]

quizOutput<-data.frame(obs=(1:20),classe=predict(combModFit,finalTestingPCDF))
write.csv(quizOutput,"quizOutput.csv")