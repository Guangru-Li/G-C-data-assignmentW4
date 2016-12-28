library("dplyr")
##X indicates the feature, Y indicates the activity(standing, sitting etc.),
##subject indicates the label of the volunteers

X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt")
Subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt")
Subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
Features<-read.table("./UCI HAR Dataset/features.txt")

##Task 1 merge the train and test sets to create one data set
Test<-cbind(X_test,Y_test,Subject_test)
Train<-cbind(X_train,Y_train, Subject_train)
Combine<-rbind(Test,Train)
names(Combine)<-c(as.character(Features$V2),"activity","subject")
##Task 2 Extract only the measurements on the mean and standard deviation for each other 
Extract<-Combine[,grep("mean\\(\\)|std\\(\\)",names(Combine))]
Extract<-cbind(Extract,Combine[,c("activity","subject")])
##Task 3 Uses descriptive activity names to name the acivities in the data set
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")$V2
activity_labels<-factor(activity_labels, levels=activity_labels)
Extract$activity<-as.factor(Extract$activity)
levels(Extract$activity)<-levels(activity_labels)

##Task 4 Appropriately labels the data set with decriptive names
names(Extract)<-gsub("^t","time",names(Extract))
names(Extract)<-gsub("^f","frequency",names(Extract))
names(Extract)<-gsub("^Acc","Accelerometer",names(Extract))
names(Extract)<-gsub("^Gyro","Gyroscope",names(Extract))
names(Extract)<-gsub("^Mag","Magnitude",names(Extract))
names(Extract)<-gsub("^BodyBody","Body",names(Extract))
Extract$subject<-paste("Participate", as.character(Extract$subject))

##Task 5 From the data set in setp 4, create a second, independent tidy data set with the average of each variable for each activity and each subject 
Data2<-aggregate(. ~subject + activity, Extract, mean)
Data2<-arrange(Data2, as.integer(substring(subject,nchar("participate "))))
write.table(Data2,file="tidydata.txt")

##Produce Codebook
#library("knitr")
#knit2html("codebook.Rmd")

