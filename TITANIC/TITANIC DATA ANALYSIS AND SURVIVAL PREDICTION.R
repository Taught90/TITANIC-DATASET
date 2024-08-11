#Setting a working directory.
setwd("C:/Users/EliteBook/OneDrive/Desktop/DATA SCIENCE PERSONAL PROJECTS/TITANIC")

library("dplyr")
library("datasets")
library("ggplot2")
library("graphics")
library("stats")
library("ggeffects")
library("stringi")
library("stringr")
library("randomForest")
library("rpart")
library("tidyselect")

##############################
#calling the dataset
titanic_train <- read.csv('train.csv', stringsAsFactors = FALSE, header = T)
titanic_test <- read.csv('test.csv', stringsAsFactors = FALSE, header = T)

#viewing the dataset
View(titanic_train)
View(titanic_test)

#combining the two datasets.
titanic_test$Survived <- NA

titanic_train$Istraindata <- TRUE
titanic_test$Istraindata <- FALSE

ncol(titanic_test)
ncol(titanic_train)

#combining them with r-bind.
titanic_data <- rbind(titanic_train, titanic_test)
View(titanic_data)

#setting factors to categorical data
titanic_data$Pclass<-as.factor(titanic_data$Pclass)
titanic_data$Embarked<-as.factor(titanic_data$Embarked)
titanic_data$Sex<-as.factor(titanic_data$Sex)
titanic_data$Survived<- as.factor(titanic_data$Survived)

#cleaning the data
mean.Age<- mean(titanic_data$Age, na.rm = T)

mean.Age

titanic_data$Age<- titanic_data %>%
  select(Age) %>%
  apply(c(2), . %>% {ifelse(is.na(.), mean.Age, .)})

titanic_data[titanic_data$Embarked == '', "Embarked"] <- 'S'

mean.Fare<- mean(titanic_data$Fare, na.rm = T)

mean.Fare

titanic_data$Fare <- titanic_data %>%
  select(Fare) %>%
  apply(c(2), . %>% {ifelse(is.na(.), mean.Fare, .)})

#creating age clusters
titanic_data<- titanic_data %>%
  mutate(age_groups = ifelse(0<=Age & Age<=13, 1, ifelse(14<=Age & Age<=35, 2, ifelse(36<=Age & Age<=60, 3, 4))))

titanic_data<- titanic_data %>%
  mutate(age_groups_young = ifelse(0<=Age & Age<=13, "child",ifelse(13<=Age & Age<=35, "youth", "adult")))

titanic_data<- titanic_data %>%
  mutate(age_groups_children = ifelse(0<=Age & Age<=5, "infant",ifelse(5<=Age & Age<=7, "child",ifelse(7<=Age & Age<=13, "teen", "non-child"))))

titanic_data <- titanic_data %>%
  mutate(FamilySize= Parch + SibSp + 1)

titanic_data<- titanic_data %>%
  mutate(IsAlone = ifelse(FamilySize==1,1,0))

titanic_data <- titanic_data %>%
  mutate(Eldest= stri_extract(titanic_data$Name, regex="[:alpha:]'?+[:alpha:]+,")) %>%
  mutate(Eldest= stri_extract(titanic_data$Name, regex="[:alpha:]'?+[:alpha:]+")) 
  

titanic_data <- titanic_data %>%
  mutate(Title= stri_extract(titanic_data$Name, regex=", [:alpha:]+.")) %>%
  mutate(Title = trimws(stri_extract(Title, regex = " [:alpha:]+")))

titanic_data <- titanic_data %>%
  group_by(Ticket) %>%
  mutate(PeopleSharingTicket = n(),
         IndividualFare = Fare / PeopleSharingTicket) %>%
  ungroup()

str(titanic_data)


#splitting up the dataset.
titanic_train_data<- titanic_data[titanic_data$Istraindata == TRUE,]
titanic_test_data<- titanic_data[titanic_data$Istraindata == FALSE,]

#viewing the dataset
View(titanic_train_data)
View(titanic_test_data)

#analyzing the data by grouping
titanic_train_data%>%
  group_by(Pclass)%>%
  summarise(mean(Survived))

titanic_train_data%>%
  group_by(Sex)%>%
  summarise(mean(Survived))

titanic_train_data%>%
  group_by(Embarked)%>%
  summarise(mean(Survived))

titanic_train_data%>%
  group_by(SibSp)%>%
  summarise(mean(Survived))

titanic_train_data%>%
  group_by(Parch)%>%
  summarise(SibSp) %>%
  View()

titanic_train_data %>%
  group_by(age_groups)%>%
  summarise(mean(Survived))

titanic_train_data %>%
  group_by(age_groups_children)%>%
  summarise( mean(Survived))

#setting factors to categorical data
titanic_train_data$Survived<- as.factor(titanic_train_data$Survived)

#visualizing the data for insights
titanic_train_data %>%
  select(age_groups,Age) %>%
  ggplot(data= ) +
  geom_col(aes(x=age_groups, y=Age))

titanic_train_data %>%
  select(Survived, Sex) %>%
  ggplot(data= ) +
  geom_col(aes(x=Survived, y=Sex))

titanic_train_data %>%
  select(Survived, Pclass, Ticket ,age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_point(mapping =  aes(x=Ticket, y= Age), stat = "identity")

titanic_train_data %>%
  select(Survived, Pclass, Fare, Ticket ,age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_point(mapping =  aes(x=Fare, y= Age), stat = "identity")

titanic_train_data %>%
  select(Survived, Pclass, Fare, Ticket ,age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_point(mapping =  aes(x=Fare, y= Pclass), stat = "identity")

titanic_train_data %>%
  select(Survived, Embarked, Pclass, Fare, Ticket ,age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_point(mapping =  aes(x=Pclass, y= Fare), stat = "identity") +
  facet_grid( Embarked ~ Sex)

titanic_train_data %>%
  select(Survived, Pclass, age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=Pclass, fill= factor(Survived)), color = "black", position = position_dodge())

titanic_train_data %>%
  select(Survived, Embarked, Pclass, age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=Embarked, fill= factor(Survived)), color = "black", position = position_dodge()) 

titanic_train_data %>%
  select(Survived, Embarked, Pclass, age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=Sex, fill= factor(Survived)), color = "black", position = position_dodge()) 

titanic_train_data %>%
  select(Survived,SibSp) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=SibSp, fill= factor(Survived)), color = "black", position = position_dodge()) 

titanic_train_data %>%
  select(Survived, SibSp, Sex, age_groups) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=SibSp, fill= factor(Survived)), color = "black", position = position_dodge()) +
  facet_grid(Sex~age_groups)

titanic_train_data %>%
  select(Survived, Parch) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=Parch, fill= factor(Survived)), color = "black", position = position_dodge()) 

titanic_train_data %>%
  select(Survived, Parch, Sex, age_groups) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=Parch, fill= factor(Survived)), color = "black", position = position_dodge()) +
  facet_grid(Sex~age_groups)

titanic_train_data %>%
  select(Survived, Pclass, age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=Pclass, fill= factor(Survived)), color = "black", position = position_dodge()) +
  facet_grid(Sex ~ age_groups)

titanic_train_data %>%
  select(Survived, Pclass, age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=factor(Pclass), y = Age, fill= Survived), stat = "identity",  position=position_dodge()) 

titanic_train_data %>%
  select(Survived, Pclass, age_groups, Sex,Age) %>%
  ggplot(data=) + 
  geom_bar(mapping =  aes(x=Pclass, y = Age, fill= factor(Survived)), stat = "identity",  position=position_dodge()) +
  facet_grid(Sex ~ age_groups)


#####################
#method one for doing our prediction
#####################

randomForest(Survived ~  age_groups + SibSp + Parch + Sex + Pclass + Embarked, data= titanic_train_data) %>%
  predict() %>%
  table()

#####################
#method two for doing our prediction
#####################
titanic_train_data.head <- titanic_train_data

titanic_test_data.head <- titanic_train_data %>%
  mutate(Age = NA)

str(titanic_test_data.head)
str(titanic_train_data.head)

modeleAge<-titanic_train_data.head %>%
  select(Age, Fare, Parch, Survived, SibSp, Pclass, Ticket, Sex) %>%
  lm(Age ~ Fare + Parch + Survived + SibSp + Pclass + Ticket +Sex, data= .) 

predicted.Age<- predict(modeleAge , newdata =  titanic_test_data.head) 

titanic_train_data$Age<- predicted.Age

model.DT.Survive<- rpart(Survived ~ Age + Sex + Embarked + Title + Pclass, data= titanic_train_data, method = "class")

model.DT.Survive %>%
  summary()

#####################
#method three for doing our prediction
#####################
 

Model.Survive<-randomForest(Survived ~ age_groups + Sex + Embarked + Title + Pclass + FamilySize + IndividualFare + Ticket , data= titanic_train_data) 

Model.Survive %>%
  predict() %>%
  table()


predicted.Survive<- predict(Model.Survive, newdata = titanic_test_data)
predicted.Survive %>%
  table()

Model.Survive
features.Equation<-"Age + Sex + Embarked + Title"

oob.error.data <- data.frame(
  Trees=rep(1:nrow(Model.Survive$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(Model.Survive$err.rate)),
  Error=c(Model.Survive$err.rate[,"OOB"], 
          Model.Survive$err.rate[,"1"], 
          Model.Survive$err.rate[,"0"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

PassengerId<- titanic_test_data$PassengerId
output.df<- as.data.frame(PassengerId)
output.df$Survived <- predicted.Survive
write.csv(output.df, file = "Kaggle_new_submission.csv", row.names = FALSE)

