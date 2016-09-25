require(plyr)
require(dplyr)
require(MASS)
require(ISLR)
require(leaps)
require(glmnet)
require(caret)
require(e1071)
require(depmixS4)

setup()

setup <- function() {
  #Script to load Suturing Kinematics dataset, metadata and its transcriptions
  setwd("/R Projects/temp/SU_kinematics/Suturing/kinematics/AllGestures")
  remove(SU_kinematics_dataset)

  file_list <- list.files()
  file_list

  for (file in file_list) {
    # if the merged dataset doesn't exist, create it
    if(!exists("SU_kinematics_dataset")){
      SU_kinematics_dataset <- read.table(file, header=FALSE)
      SU_kinematics_dataset$filename<-file
    }

    # if the merged dataset does exist, append to it
    if(exists("SU_kinematics_dataset")){
      temp_dataset <-read.table(file, header=FALSE)
      temp_dataset$filename<-file
      SU_kinematics_dataset<-rbind(SU_kinematics_dataset, temp_dataset)
      rm(temp_dataset)
    }
  }

  #Loaded 138506 observations and 76 columns
  #Create column heading for dataframe based on readme.txt provided
  colnames(SU_kinematics_dataset) <- c("Master_left_tooltip_x","Master left_tooltip_y", "Master_left_tooltip_z","Master_left_tooltip_R1","Master_left_tooltip_R2","Master_left_tooltip_R3","Master_left_tooltip_R4", "Master_left_tooltip_R5","Master_left_tooltip_R6", "Master_left_tooltip_R7", "Master_left_tooltip_R8",
      "Master_left_tooltip_R9","Master_left_tooltip_trans_vel_x'","Master_left_tooltip_trans_vel_y'","Master_left_tooltip_trans_vel_z'","Master_left_tooltip_rot_vel_1","Master_left_tooltip_rot_vel_2",
      "Master_left_tooltip_rot_vel_3","Master_left_gripper_angle","Master_right1","Master_right2","Master_right3","Master_right4","Master_right5","Master_right6","Master_right7","Master_right8","Master_right9",
      "Master_right10","Master_right11","Master_right12","Master_right13","Master_right14","Master_right15","Master_right16","Master_right17","Master_right18","Master_right19","Slave_left_tooptip_x",
      "Slave_left_tooptip_y","Slave_left_tooptip_z","Slave_left_tooltip_R1","Slave_left_tooltip_R2","Slave_left_tooltip_R3","Slave_left_tooltip_R4","Slave_left_tooltip_R5","Slave_left_tooltip_R6",
      "Slave_left_tooltip_R7","Slave_left_tooltip_R8","Slave_left_tooltip_R9","Slave_left_tooltip_trans_vel_x'","Slave_left_tooltip_trans_vel_y'","Slave_left_tooltip_trans_vel_z'",
      "Slave_left_tooltip_rot_vel_1","Slave_left_tooltip_rot_vel_2","Slave_left_tooltip_rot_vel_3","Slave_left_gripper_angle","Slave_right1","Slave_right2","Slave_right3","Slave_right4",
      "Slave_right5","Slave_right6","Slave_right7","Slave_right8","Slave_right9","Slave_right10","Slave_right11","Slave_right12","Slave_right13","Slave_right14","Slave_right15","Slave_right16",
      "Slave_right17","Slave_right18","Slave_right19","filename")

  #############

  remove(SU_kinematics_metafile)
  SU_kinematics_metafile<-read.table("/R Projects/temp/SU_kinematics/Suturing/meta_file_Suturing.txt",header=F)
  colnames(SU_kinematics_metafile) <-c("filename","skill_level_self_proclaimed","skill_level_GRS","Global_Rating_Score1","Global_Rating_Score2","Global_Rating_Score3","Global_Rating_Score4","Global_Rating_Score5","Global_Rating_Score6")

  #############

  remove(SU_kinematics_transcriptions)
  setwd("/R Projects/temp/SU_kinematics/Suturing/transcriptions")

  file_list <- list.files()

  for (file in file_list){
  
    # if the merged dataset doesn't exist, create it
    if (!exists("SU_kinematics_transcriptions")){
      SU_kinematics_transcriptions <- read.table(file, header=FALSE)
      SU_kinematics_transcriptions$filename<-file
    }
  
    # if the merged dataset does exist, append to it
    if(exists("SU_kinematics_transcriptions")){
      temp_dataset <-read.table(file, header=FALSE)
      temp_dataset_transcriptions$filename<-file
      SU_kinematics_transcriptions<-rbind(SU_kinematics_transcriptions, temp_dataset)
      rm(temp_dataset)
    }
  }

  colnames(SU_kinematics_transcriptions ) <- c("start_frame","end_frame","gesture_id","file")
  #Loaded 
  #Create tasks for Gesture
  Gesture_Suturing_TaskID <- c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11)
  Gesture_Knot_Trying_TaskID<-c(1 , 11 , 12 , 13 , 14 , 15)
  Gesture_NeedlePassing_TaskID<- c( 1, 2, 3, 4, 5, 6, 8, 11)

  GestureID<-c("G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","G13","G14","G15")
  GestureDesc <-
  c("Reaching for needle with right hand",
    "Positioning needle",
    "Pushing needle through tissue",
    "Transferring needle from left to right",
    "Moving to center with needle in grip",
    "Pulling suture with left hand",
    "Pulling suture with right hand",
    "Orienting needle",
    "Using right hand to help tighten suture",
    "Loosening more suture",
    "Dropping suture at end and moving to end points",
    "Reaching for needle with left hand",
    "Making C loop around right hand",
    "Reaching for suture with right hand",
    "Pulling suture with both hands")

  head(SU_kinematics_dataset)
  
  GestureDF<-data.frame(GestureID,GestureDesc)
  Gesture_SuturingTasks<-data.frame(Gesture_Suturing_TaskID)
  Gesture_Knot_TryingTasks<-data.frame(Gesture_Knot_Trying_TaskID)
  Gesture_NeedlePassingTasks<-data.frame(Gesture_NeedlePassing_TaskID)

  classify()
}

classify <- function() {
  #Generate Classifications on GestureID
  dvvs <- SU_kinematics_dataset
  dvvs[,"filename"] <- NULL
  labels <- SU_kinematics_transcriptions
  offset = ncol(labels)
  for(i in 1:3) {
    labels[offset+i] <- rowShift(labels$gesture_id,i)
  }
  colnames(labels)[[offset+1]] <- "next.id"
  colnames(labels)[[offset+2]] <- "next.next.id"
  colnames(labels)[[offset+3]] <- "next.next.next.id"
  starts <- labels[which(labels$gesture_id == "G2"),]
  starts$classe <- ifelse(starts$next.id == "G3" & starts$next.next.id == "G6" 
                           & starts$next.next.next.id == "G4", 1, 0)
  starts$classe <- as.factor(starts$classe)
  starts <- na.omit(starts)
  
  appendHelper <- function(x) {
    ind <- append(ind,list(x[["start_frame"]]:x[["end_frame"]]))
  }
  
  ind <- list()
  ind <- apply(starts, 1, function(x) do.call(appendHelper, list(x)))
  lLength = length(ind)
  growDF <- dvvs
  growDF <- subset(dvvs,FALSE)
  for(i in 1:lLength) {
    indexes <- ind[[i]][[1]]
    subDF <- dvvs[indexes,]
    retVec <- apply(subDF,2,mean)
    growDF <- rbind(growDF,retVec)
  }
  colnames(growDF) <- colnames(dvvs)
  dvvs <- growDF
  dvvs$classe <- starts$classe
  
  #for(i in 1:length(ind)) { dvvs[c(ind[[i]][[1]]),"classe"] <- as.integer(starts[[i,"classe"]]) }
  
  dvvs <- dvvs[!is.na(dvvs$classe),]
  dvvs$classe <- as.numeric(dvvs$classe)
  dvvs$classe <- dvvs$classe - 1
  dvvs$classe <- as.factor(dvvs$classe)
  
  #Setup
  inTrain <- sample(1:nrow(dvvs),as.integer(0.5 * nrow(dvvs)),FALSE)
  trainDF <- dvvs[inTrain,]
  testDF <- dvvs[-inTrain,]
  
  # PCA on input features => Feature Engineering
  nzv <- nearZeroVar(trainDF,saveMetrics = T)
  zeros <- nzv$zeroVar
  trainDF <- trainDF[,which(zeros == F)]
  testDF <- testDF[,which(zeros == F)]
  # remove zero variance features
  ytrain <- trainDF$classe
  ytest <- testDF$classe
  trainDF$classe <- NULL
  testDF$classe <- NULL
  l <- sapply(testDF,function(x) is.factor(x))
  ind <- which(sapply(trainDF,function(x) is.factor(x)))
  levels(testDF[,ind]) <- levels(trainDF[,ind])
  pcaTrain <- model.matrix(~.,data=trainDF)
  pcaTest <- model.matrix(~.,data=testDF)
  pcaTrain <- pcaTrain[,2:ncol(pcaTrain)]
  pcaTest <- pcaTest[,2:ncol(pcaTest)]
  pca <- prcomp(pcaTrain,scale=T,center=T)
  
  # Feature Selection by PCA Loadings
  summary(pca)
  pcaTrain <- pca$x[,1:3] # Extract Principal Components
  pcaTest <- predict(pca,newdata = pcaTest)[,1:3] #PCA transformation onto test set
  pcaTrain <- data.frame(pcaTrain)
  pcaTest <- data.frame(pcaTest)
  ytrain <- as.factor(ytrain)
  ytest <- as.factor(ytest)
  pcaTrain$classe <- ytrain
  pcaTest$classe <- ytest
  
  # predict "lift up" classe using SVM (1)
  costs <- array(dim = 20)
  for(i in 1:20) {
    svm.dvvs <- svm(classe~.,data=pcaTrain,kernel="radial",cost=i,scale=F)
    svm.predict <- predict(svm.dvvs,newdata=pcaTest)
    costs[i] = round(mean(svm.predict == pcaTest$classe),5)
  }
  best.cost = which(costs == max(costs))[[1]]
  svm.best <- svm(classe~.,data=pcaTrain,kernel="radial",cost=best.cost,scale=F,probability=T)
}  

rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

predict.classe <- function(x) {
  predDF <- trainDF
  predDF <- subset(trainDF,FALSE)
  names(x) <- colnames(trainDF)
  predDF <- rbind(predDF,x); predDF <- rbind(predDF,x)
  colnames(predDF) <- colnames(trainDF)
  predPCA <- model.matrix(~.,data=predDF)[,-1]
  predTest <- predict(pca,newdata=predPCA)[,1:3]
  svm.pred <- predict(svm.best,newdata=predTest,probability=T)
  probs <- attr(svm.pred,"probabilities")[1,]
  return (probs[1]);
}