#By Trever Alexander 12/18/2016



rm(list = ls())


## Data Analysis 

####setwd("C:/Users/Trever Alexander/Desktop/Fall 2016/Project")
## df <- read.csv("genderclassmodel.csv", header= T)

test <- read.csv("test.csv", header = T)

## test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,]) #Create new column for merging



#Another Method 
test.survived <- test #duplicate into new dataframe
test.survived$Survived <- "None" #write new survived column for data merging

train <- read.csv("train.csv", header = T)

#Merging test with train data frame 
data.combined <-rbind(train, test.survived)

str(data.combined)




data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


#observe the data table format
table(data.combined$Pclass)
table(data.combined$Survived)
table(data.combined$Sex)


#plot data to see a visulization of the data

library(ggplot2)
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)
Pclass <- qplot(Pclass, data = train, geom = "bar", fill = Survived, width = 0.5) #0 = died 1 = survived
Pclass + ylab("Total Count")

Sex <- qplot(Sex, data = train, geom = "bar", fill = Survived, width = 0.5)
Sex +  ylab("Total Count")

#Exam unique names between test and training set
length(unique(as.character(train$Name))) # Training

length(unique(as.character(data.combined$Name))) # Test

#examine the duplicated names structure to decide whether to keep the names or not

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#take a look at the records duplicated
data.combined[which(data.combined$Name %in% dup.names),]


#What is up with Miss and Mr?
library(stringr)

Misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
Misses[1:5,]

Mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
Mrs[1:5,]


#Check out Males to see if there is a pattern
male <- data.combined[which(train$Sex == "male"),]
male[1:5,]


#Create a utility function to help with title extraction

extractTitle <- function(Name){
  
  Name <- as.character(Name)
  
  if(length(grep("Miss.", Name))>0){
    return("Miss.")
  } else if(length(grep("Master.", Name))>0){
    return("Master.")
  } else if(length(grep("Mrs.", Name))>0){
    return("Mrs.")
  } else if(length(grep("Mr.", Name))>0){
    return("Mr.")
    
  } else{
    return("Other")
  }
}

Titles <- NULL

for(i in 1:nrow(data.combined)){
  Titles <- c(Titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$Titles <- as.factor(Titles)


#Plot titles against survived

Title <- qplot(Titles, data = data.combined[1:891,], geom = "bar", fill = Survived, width = 0.5)
Title + facet_wrap(~Pclass) + ggtitle("Pclass") + xlab("Title") + ylab("Total Count") + labs(fill = "Survived")

#Summary distribution of Sex

table(data.combined$Sex)
summary(data.combined$Sex)


Sex <- qplot(Sex, data = train, geom = "bar", fill = Survived, width = 0.5)
Sex +  facet_wrap(~Pclass) + ggtitle("Pclass") + xlab("Sex") + ylab("Total Count") + labs(fill = "Survived")

#Table distrubtion of Age
table(data.combined$Age)
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

Age <- ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) 
Age + facet_wrap(~Sex + Pclass) + geom_histogram(binwidth = 10) + xlab("Age") + ylab("Total Count")


#Validate that master is a good proxy for male children

Boys <- data.combined[which(data.combined$Titles == "Master."),]
summary(Boys$Age)

#Miss is more complicated variable so we should examine that variable deeper as well

Miss <- data.combined[which(data.combined$Titles == "Miss."),]
summary(Miss$Age)


#MissPlot over Age
MissPlot <- ggplot(Miss[Miss$Survived != "None",], aes(x = Age, fill = Survived))
MissPlot + facet_wrap(~Pclass) + geom_histogram(binwidth = 5) + ggtitle("Age for Miss.' by Pclass") +
  xlab("Age") + ylab("Total Count")

#Female children appear to have a different survival rate , could be a candidate for feature engineering

Miss.alone <- Miss[which(Miss$SibSp == 0 & Miss$Parch == 0),]
summary(Miss.alone$Age)
length(which(Miss.alone$Age <= 14.5))


#summarize sibsp variable 
summary(data.combined$SibSp)


#Can we treat this as a factor
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

#Plot Sibsp against Pclass
SibspPlot <- ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived, width = 1)) 
SibspPlot + geom_bar() + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Title") + 
  xlab("Sibsp") + ylab("Total Count") + ylim(0,300) + labs(fill = "Survived")

#Plot Parch against Pclass
ParchPlot <- ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived, width = 1)) 
ParchPlot + geom_bar() + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Title") + 
  xlab("Parch") + ylab("Total Count") + ylim(0,300) + labs(fill = "Survived")


#Feature engineering. Create Family Size

Temp.sibsp <- c(train$SibSp, test$SibSp)
Temp.parch <- c(train$Parch, test$Parch)
data.combined$Familysize <- as.factor(Temp.sibsp + Temp.parch +1)

#Plot family size
FamilySize <- ggplot(data.combined[1:891,], aes(x = Familysize, fill = Survived, width = 1)) 
FamilySize + geom_bar() + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Title") + 
  xlab("Family Size") + ylab("Total Count") + ylim(0,300) + labs(fill = "Survived")


#Analyze Ticket 

str(data.combined$Ticket)

data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:30]

#No structure, lets see if we can find a unique pattern or structure
Ticket.1st.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.1st.char)

#turn ticket.1st.char into a factor so we can analyze the data

data.combined$Ticket.1st.char <- as.factor(Ticket.1st.char)

Ticketplot <- ggplot(data.combined[1:891,], aes(x = Ticket.1st.char, fill = Survived)) +
  geom_bar() + ggtitle("Survivability by Ticket.1st.char") + xlab("Ticket.1st.char") +
  ylab("Total Count") + ylim(0,350) + labs(fill = "Survived")
Ticketplot


Ticketpclass <- ggplot(data.combined[1:891,], aes(x = Ticket.1st.char, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass) + ggtitle("Pclass") + xlab("Ticket.1st.char") +
  ylab("Total Count") + ylim(0,150) + labs(fill = "Survived")
Ticketpclass

TicketpclassTitle <- ggplot(data.combined[1:891,], aes(x = Ticket.1st.char, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Title") + xlab("Ticket.1st.char") +
  ylab("Total Count") + ylim(0,200) + labs(fill = "Survived")
TicketpclassTitle


#summarize fare
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#cannot make a factor so use histogram to visualzie data

Fare <- ggplot(data.combined, aes(x = Fare)) + geom_histogram(binwidth = 5) 
Fare + ggtitle("Combined Fare Distribution") + xlab("Fare") + ylab("Total Count") +ylim(0,200)

#Predictive power?

FarePlot <- ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Titles") +
  xlab("Fare") + ylab("Total Count") + ylim(0,50) + labs(fill = "Survived")
FarePlot

#analysis of cabin variable
str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replace empty cabins with variavle
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#Take a look at first Char
Cabin.1st.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.1st.char)
levels(Cabin.1st.char)

#Add to dataset
data.combined$Cabin.1st.char <- Cabin.1st.char

#Visualize Cabin.1st.char

Cabin <- ggplot(data.combined[1:891,], aes(x = Cabin.1st.char, fill = Survived)) +
  geom_bar() + ggtitle("Survivability by Cabin.1st.char") + xlab("Cabin.1st.char") +
  ylab("Total Count") + ylim(0,750) + labs(fill = "Survivability")
Cabin

#Possibily predictice

CabinPlot <- ggplot(data.combined[1:891,], aes(x = Cabin.1st.char, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass) + ggtitle("Survivability by Cabin.1st.char") + xlab("Pclass") +
  ylab("Total Count") + ylim(0,500) + labs(fill = "Survivability")
CabinPlot

CabinPlot1 <- ggplot(data.combined[1:891,], aes(x = Cabin.1st.char, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Titles") + xlab("Pclass") +
  ylab("Total Count") + ylim(0,500) + labs(fill = "Survivability")
CabinPlot1

#How about multiple cabins
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N" ))

MultipleCabin <- ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Titles") + xlab("Cabin.multiple") +
  ylab("Total Count") + ylim(0,350) + labs(fill = "Survived")
MultipleCabin

#Does survivability depend on where you boarded?

str(data.combined$Embarked)
levels(data.combined$Embarked)

Embarked <- ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass + Titles) + ggtitle("Pclass, Titles") + xlab("Embarked") + 
  ylab("Total Count") + ylim(0,350) + labs(fill = "Survived")
Embarked



library(randomForest)

## Exploratory Analysis
#Training RandomForest model with default parameter using Pclass and Titles

rf.train.1 <- data.combined[1:891, c("Pclass", "Titles")]
rf.label <- as.factor(train$Survived)

set.seed(9999)

rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = T, ntree = 1000)
rf.1
varImpPlot(rf.1)



#Train a new Random Forest against Pclass, Titles, SibSp



rf.train.2 <- data.combined[1:891, c("Pclass", "Titles", "SibSp")]

set.seed(9999)


rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = T, ntree  = 1000)
rf.2
varImpPlot(rf.2)


rf.train.3 <- data.combined[1:891, c()]



#Train a new Random Forest against Pclass, Titles, Parch

rf.train.3 <- data.combined[1:891, c("Pclass", "Titles", "Parch")]

set.seed(9999)

rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = T, ntree = 1000)
rf.3
varImpPlot(rf.3)


#Train a new Random Forest against Pclass, Titles, Parch, SibSp

set.seed(9999)

rf.train.4 <- data.combined[1:891, c("Pclass", "Titles", "Parch", "SibSp")]

rf.4 <- randomForest(x = rf.train.4, y =rf.label, importance = T, ntree = 1000)
rf.4
varImpPlot(rf.4)



#Train a Random Forest with Pclass, Titles, Familysize

rf.train.5 <- data.combined[1:891, c("Pclass", "Titles", "Familysize")]

set.seed(9999)

rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = T, ntree = 1000)
rf.5
varImpPlot(rf.5)


#Train Random Forest with Pclass, Titles, Familysize, SibSp

rf.train.6 <- data.combined[1:891, c("Pclass", "Titles", "Familysize", "SibSp")]

set.seed(9999)

rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = T, ntree = 1000)
rf.6
varImpPlot(rf.6)


#Train Random Forest with Pclass, Titles, Familysize, Parch

rf.train.7 <- data.combined[1:891, c("Pclass", "Titles", "Familysize", "Parch")]

set.seed(9999)

rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = T, ntree = 1000)
rf.7
varImpPlot(rf.7)



###

#Subset our test records and features

test.submit.df <- data.combined[892:1309, c("Pclass", "Titles", "Familysize")]


#Make Predictions
rf.5.preds <- predict(rf.5, test.submit.df)

table(rf.5.preds)


#Write csv file to test predictions
submit.df <- data.frame(PassengerId = rep(892:1309), Survived= rf.5.preds)


write.csv(submit.df, file = "RandomForestPred_12_21_2016.csv", row.names = F)



#Cross Validation

library(caret)
##help(package = "caret")
library(doSNOW)




#Leverage caret to create 100 total folds 
set.seed(99999)

cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)


#Check stratification
table(rf.label)

342/549

table(rf.label[cv.10.folds[[33]]])

308/494


#Set up caret train control object for above

ctrl.1 <- trainControl(method ="repeatedcv", number = 10, repeats = 10, index = cv.10.folds)



#use doSNOW package for multi-core training

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


#Set seed for reproductability and train 

set.seed(999999)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", 
                   tuneLength = 3, ntree = 1000, trControl = ctrl.1)

#Shutdown cluster 
stopCluster(cl)


#Review Results
rf.5.cv.1



#   Potentially overfitting still let's run 5 folds intead to reduce
#   chance of overfitting



#Set up caret train control object for above
set.seed(52094)

cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method ="repeatedcv", number = 5, 
                       repeats = 10, index = cv.5.folds)



#use doSNOW package for multi-core training

cl <- makeCluster(5, type = "SOCK")
registerDoSNOW(cl)


#Set seed for reproductability and train 

set.seed(052094)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", 
                   tuneLength = 3, ntree = 1000, trControl = ctrl.2)

#Shutdown cluster 
stopCluster(cl)


#Review Results
rf.5.cv.2


# 5 fold is not too better so lets retry it using 3 folds instead

#Set up caret train control object for above
set.seed(7777)

cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method ="repeatedcv", number = 3, 
                       repeats = 10, index = cv.3.folds)



#use doSNOW package for multi-core training

cl <- makeCluster(5, type = "SOCK")
registerDoSNOW(cl)


#Set seed for reproductability and train 

set.seed(94520)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", 
                   tuneLength = 3, ntree = 1000, trControl = ctrl.3)

#Shutdown cluster 
stopCluster(cl)


#Review Results
rf.5.cv.3
rf.5.cv.2
rf.5.cv.1



#More data exploratory test using a single decision tree so we can understand 
#What is going on better potentially feature engineering so we can have better
#Results

library(rpart)
library(rpart.plot)


#use 3 fold cv repeated 10 times 
#Create utility function

rpart.cv <- function(seed, training, labels, ctrl){
  cl <- makeCluster(5, type = "SOCK")
  registerDoSNOW(cl)
  
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30,
                    trControl = ctrl)
  
  # Shutdown Cluster
  stopCluster(cl)
  
  return(rpart.cv)
}



features <- c("Pclass", "Titles", "Familysize") 
rpart.train.1 <- data.combined[1:891, features]


#Run cv and review results

rpart.1.cv.1 <- rpart.cv(9999, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#Build plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = T)


#The plot brings out some interesting lines to investigate further
#     1. Titles of  "Mr." and "Other" are predicted to parish at an
#        overall accuracy rate of 83.2%
#     2. Titles of "Master.", "Miss.", & "Mrs." in 1st & 2nd class 
#        are predicted to survive at an overall accuracy of 94.9%
#     3. Titles of "Master.", "Miss.", & "Mrs." in 3rd class  with
#        family sizes of 5,6,8,& 11 are predicted to parish with 100% accuracy
#     4. Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#        family sizes not equal to 5,6,8,&11 are predicted to survive
#        with 59.6% accuracy



# Both rpart and rf confirm that title is important, lets analyze more

table(data.combined$Titles)


#Parse out last name and title

data.combined[1:25, "Name"]

name.split <- str_split(data.combined$Name, ",")
name.split[1]
last.names <- sapply(name.split, "[", 1)
last.names[1:10]

#Add lastname to data frame just incase it is useful later on
data.combined$Last.names <- last.names

#Now for Titles
name.split <- str_split(sapply(name.split, "[", 2), " ")
Title <- sapply(name.split, "[", 2)
unique(Title)


#What is up with a title of "the"?

data.combined[which(Title == "the"),]


#Renaming titles to be more accurate

Title[Title %in% c("Dona.", "the")] <- "Lady."
Title[Title %in% c("Ms.", "Mlle.")] <- "Miss."
Title[Title == "Mme."] <- "Mrs."
Title[Title %in% c("Jonkheer.", "Don.")] <- "Sir."
Title[Title %in% c("Col.", "Capt.","Major.")] <- "Officer"  
table(Title)

#Make title a factor 
data.combined$New.titles <- as.factor(Title)

#Visualize the new title

Newtitle <- ggplot(data.combined[1:891,], aes(x = New.titles, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass) + ggtitle("Survival rate for New.titles by Pclass")
Newtitle


#Collapse titles based off visual analysis 

indexes <- which(data.combined$New.titles == "Lady.")
data.combined$New.titles[indexes] <- "Mrs."

indexes <- which(data.combined$New.titles == "Dr." |
                 data.combined$New.titles == "Rev."|
                 data.combined$New.titles == "Sir."|
                 data.combined$New.titles == "Officer")
data.combined$New.titles[indexes] <- "Mr."


#Visualize after the change
Newtitle1 <- ggplot(data.combined[1:891,], aes(x = New.titles, fill = Survived)) +
  geom_bar() + facet_wrap(~Pclass) + ggtitle("Survival rate for New.titles by Pclass")
Newtitle1

#Grab Features again 
features <- c("Pclass", "New.titles", "Familysize")
rpart.train.2 <- data.combined[1:891, features]


#Run CV and check out result
rpart.2.cv.1 <- rpart.cv(9999, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1


#Plot new Decision Tree
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = T)


#Dive in on 1st class "Mr."
indexes.first.mr <- which(data.combined$New.titles == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)


#One female missed categorized
first.mr.df[first.mr.df$Sex == "female",]

#Update new title
indexes <- which(data.combined$New.titles == "Mr." &
                   data.combined$Sex == "female")
data.combined$New.titles[indexes] <- "Mrs."

#Any other missed cateogrized genders?
length(which(data.combined$Sex == "female" &
               (data.combined$New.titles == "Master."|
                  data.combined$New.titles == "Mr.")))

#Refresh data frame 
indexes.first.mr <- which(data.combined$New.titles == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]


#Lets view surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])


#Let view the high fare tickets (Use ticket as identifier instead of fare price)
indexes <- which(data.combined$Ticket == "PC 17755" |
                 data.combined$Ticket == "PC 17611" |
                 data.combined$Ticket == "113760")
View(data.combined[indexes,])


#Visualize survival for 1st class male by fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival rate by Fare")


#Engineer the features based on passengers traveling with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  
  for (k in 1:length(party.indexes)){
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
    
  }
  
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare


#Refresh 1st class "Mr." Data Frame
first.mr.df <-data.combined[indexes.first.mr,]
summary(first.mr.df)

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) + ggtitle("Survival Rate 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) + ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")


#Hypothesis - ticket.party.size is highly correlated with avg.fare

summary(data.combined$avg.fare)



#One missing value
data.combined[is.na(data.combined$avg.fare),]


#Get records for similar passaengers and summarize avg.fare

indexes <- with(data.combined, which(Pclass == "3" & Titles == "Mr." & Familysize == "1" &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)


#Choose median because it is more robust to outliers 

data.combined[is.na(avg.fare), "avg.fare"] <- 7.840


#Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)


#Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

#These two features are highly uncorrelated which means they can be two new features
#we can build prediction on 
##################################

#How about testing just 1st class?

indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes],
    postproc.data.combined$avg.fare[indexes])

#Hypothesis refuted again 



# Let's see if our feature engineering has made any difference 
features <- c("Pclass", "New.titles", "Familysize", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]


#Run CV and review results
rpart.3.cv.1 <- rpart.cv(9999, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

#plot the data
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = T)



##############################



#rpart score = 0.80383

#subset our test records and features
test.submit.df <- data.combined[892:1309, features]
  
#Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)


  #Write csv file 
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RpartPred_12_23_2016.csv", row.names = F)


#
## Random Forest scores = 0.80861
#

features <- c("Pclass", "New.titles", "ticket.party.size","avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(9999)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]


#Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

#Write out csv file 
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)


write.csv(submit.df, file = "RandomForestPred_12_23_2016.csv", row.names = F)


#Additional analysis using mutual information
##install.packages("infotheo")
library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$Titles[1:891])
mutinformation(rf.label, data.combined$Familysize[1:891])
mutinformation(rf.label, data.combined$Ticket.1st.char[1:891])
mutinformation(rf.label, data.combined$Cabin.multiple[1:891])
mutinformation(rf.label, data.combined$New.titles[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))

#Leveraging tsne 2D-visualization to represent our data
##install.packages("Rtsne")
library(Rtsne)


most.correct <- data.combined[data.combined$New.titles != "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.1 <- Rtsne(most.correct[, features], check_duplicates = F)
ggplot(NULL, aes(x = tsne.1$Y[indexes ,1], y = tsne.1$Y[indexes, 2],
                 color = most.correct$Survived[indexes])) +
  geom_point() + labs(color = "Survived") + 
  ggtitle("tsne 2D Visualization of Features for Females and Boys")



#### You can check your features and cluster them together based off this ####
#### this visualizaiton... As you can see our prediction haev been pretty  ####
#### accurate since suvrive and parish are all predominately group together ####



### To get a baseline on how well we are doing let's use conditional Mutual information
### on the tsne X and Y features for females and boys in 1st and 2nd class. The 
### intuition here is that the combination of these features should be higher than any
### individual feature we looked at above 

condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


#We can also leverage conditional mutual information using the top two 
# features used in our tree plot - new.title and Pclass 

condinformation(rf.label, data.combined[1:891, c("New.titles", "Pclass")])

##Now lets make a visualization for males since our model has the biggest potential
### updside for improving (e.g., the tree predicts incorrectly for 86 adult males)
#### lets visualize with tsne

misters <- data.combined[data.combined$New.titles == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = F)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2],
                  color = misters$Survived[indexes])) +
  geom_point() + labs(color = "Survived") +
  ggtitle("tsne 2D visualization of Features for New.titles of 'Mr.'")


#Now conditional mutual information for tsne features for adult males 
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#Use Tsne feature for all of the training data and using them in our model

tsne.3 <- Rtsne(data.combined[, features], check_duplicates = F)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y =tsne.3$Y[1:891 ,2],
                 color = data.combined$Survived[1:891])) +
  geom_point() + labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")


#Conditional mutual information for everyone
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))


#Add Tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]



  