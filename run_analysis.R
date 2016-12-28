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
Test<-cbind(X_test,Y_test,Subject_test)   #combine the test data with its subject/activity information
Train<-cbind(X_train,Y_train, Subject_train) #combine the train data with its subject/activity information
Combine<-rbind(Test,Train)   #combine all together
names(Combine)<-c(as.character(Features$V2),"activity","subject") #give names to the test parameters

##Task 2 Extract only the measurements on the mean and standard deviation for each other 
Extract<-Combine[,grep("mean\\(\\)|std\\(\\)",names(Combine))] #extract required columns
Extract<-cbind(Extract,Combine[,c("activity","subject")])  #combine them with activity and subject

##Task 3 Uses descriptive activity names to name the acivities in the data set
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")$V2
activity_labels<-factor(activity_labels, levels=activity_labels)
Extract$activity<-as.factor(Extract$activity)
levels(Extract$activity)<-levels(activity_labels) #Use factors to diretly give the activity descriptive names

##Task 4 Appropriately labels the data set with decriptive names
#rename the columns by patterning matching
names(Extract)<-gsub("^t","time",names(Extract))
names(Extract)<-gsub("^f","frequency",names(Extract))
names(Extract)<-gsub("Acc","Accelerometer",names(Extract))
names(Extract)<-gsub("Gyro","Gyroscope",names(Extract))
names(Extract)<-gsub("Mag","Magnitude",names(Extract))
names(Extract)<-gsub("BodyBody","Body",names(Extract))
names(Extract)<- gsub('[()]', '', names(Extract))
Extract$subject<-paste("Participate", as.character(Extract$subject))

##Task 5 From the data set in setp 4, create a second, independent tidy data set with the average of each variable for each activity and each subject 
#use the aggregate funciton to summarize the data into a more compact table
TidyData<-aggregate(. ~subject + activity, Extract, mean)
TidyData<-arrange(TidyData, as.integer(substring(subject,nchar("participate "))))
write.table(TidyData,file="tidydata.txt")


