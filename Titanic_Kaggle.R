library(tidyverse)
library(rvest)
library(caret)
library(pROC)
library(caretEnsemble)

titanic_data <- read_csv("D:/Users/Gregor/Desktop/Kaggle/Titanic/train.csv") #Import training data
test_data <- read_csv("D:/Users/Gregor/Desktop/Kaggle/Titanic/test.csv") #Import testing data

## Observations
glimpse(titanic_data) #Taking a glimpse, we need to recode Pclass, Sex, and Embarked to factors. But first we will combine the two datasets

comb_data <- bind_rows(titanic_data, test_data) #We combine both datasets into one

comb_data <- comb_data %>%
  mutate(train = ifelse(PassengerId %in% 1:891, 1, 0)) #We add the train column, to differentiate between the training and testing datasets

comb_data <- comb_data %>%
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass),
         Sex = as.factor(Sex),
         Embarked = as.factor(Embarked))
  
summary(comb_data) #In addition to the 418 expected NA's in Survived, we also see a decent amount of NA's in the Age variable and a few in Embarked and Fare
  
table(comb_data$Ticket) #Taking a look at all the different combinations of tickets. Some are duplicated.
#We see that most tickets are just numbers, while some have additional info (PC, SOTON, SOC,...). Some are also repeated several times

duplicated_tickets <- comb_data %>% #We filter out the duplicated tickets
  group_by(Ticket) %>%
  filter(n() > 1) %>%
  ungroup()

ggplot(comb_data, aes(x = Fare)) +
  geom_density() +
  geom_density(data = duplicated_tickets, aes(x = Fare, color = "red")) #Comparing the distributions, the duplicated tickets are mostly more expensive

comb_data <- comb_data %>%
  mutate(dup_tic = as.factor(ifelse(PassengerId %in% duplicated_tickets$PassengerId, 1, 0)))

ggplot(comb_data, aes(x = Age, y = dup_tic)) +
  geom_violin() 
#The most striking difference is that almost all of the kids were in the category with duplicated tickets, hinting that duplicated tickets might be family or group related.

duplicated_tickets %>%
  filter(grepl("CA", Ticket))

duplicated_tickets %>%
  filter(grepl("S.O.C", Ticket)) #Looking at a couple of examples, it appears that our theory of group tickets might be right. We can see some families forming groups for instance.

comb_data <- comb_data %>% #To get a fairer idea of ticket price, we divide the ticket cost by the number of people on it
  group_by(Ticket) %>%
  mutate(total = n()) %>%
  mutate(fare_real = Fare / total) %>%
  ungroup()

comb_data %>%
  filter(train == 1) %>%
  ggplot(aes(x = Pclass)) +
  geom_bar(aes(fill = Survived))
#The differences in survival between the classes are pretty obvious, the 1st class having the biggest proportion of survivors and the 3rd class the lowest proportion

comb_data %>%
  filter(train == 1) %>%
  ggplot(aes(x = dup_tic)) +
    geom_bar(aes(fill = Survived))
#We can see that generally, the individual tickets had a lower chance of survival, possibly due to much lower proportion of children as seen before

comb_data %>% 
  filter(train == 1) %>%
  ggplot(aes(x = Age, y = Survived, color = Sex)) +
  geom_jitter() +
  facet_wrap(vars(Pclass))
#Dividing the data by Age, Sex, and Class we can see a strong pattern related to Sex. It suggests females were much more likely to survive.
#The pattern concerning Age is less obvious, while people under 15 years in 1st and 2nd class were almost guaranteed to survive, there is no such pattern in the 3rd class

comb_data %>% 
  filter(train == 1) %>%
  ggplot(aes(x = fare_real, y = Survived)) +
  geom_point(position = "jitter") +
  geom_boxplot(alpha = 0.6, fill = "lightblue")
#There seems to be a significant trend, that people which survived had in general more expensive tickets

comb_data %>% 
  filter(train == 1) %>%
  ggplot(aes(x = fare_real, y = Survived)) +
  geom_point(position = "jitter") +
  geom_boxplot(alpha = 0.7, fill = "lightblue") +
  facet_wrap(vars(Pclass), dir = "v")
#When we however facet it by Passenger Class, the trend is no longer there. This might suggest, that fare is not a very useful predictor after taking Pclass into account


## Feature engineering

table(comb_data$Cabin) #We see that cabins range from A to G. 
#Seems worth it to try to use it, especially given the diagram of cabins where A is on top and G on bottom - https://upload.wikimedia.org/wikipedia/commons/0/0d/Olympic_%26_Titanic_cutaway_diagram.png
#It seems reasonable to assume people on the upper decks had a higher survival rate than on the bottom. Also the number of the cabin can give us information concerning the relative location of the cabin
#on the ship. The cabin locations are available on the ship's deckplans (https://www.encyclopedia-titanica.org/titanic-deckplans/)

dup_cabs <- c("B51 B53 B55", "B52 B54 B56", "B57 B59 B63 B66", "B58 B60", "B82 B84", "B96 B98", "C22 C26", "C23 C25 C27", "C55 C57", "C62 C64", "D10 D12", "E39 E41", "F E46", "F E57", "F E69", "F G63", "F G73") #We isolate the cabins with more than 1 number


dup_ind <- which(comb_data$Cabin %in% dup_cabs) #We save these in a separate index

print(comb_data[dup_ind, ], n = 41) #We see that the cabins with more than 1 value relate to cabins occupied by family members or groups, and are located basically next to each other on the floorplan, so it should be reasonably ok to keep just one of the values

comb_data <- comb_data %>%
  separate(Cabin, c("cabin_letter", "cabin_number"), sep = 1) #first we separate the Cabin variable to the letter(floor) and number(location)

table(comb_data$cabin_letter) #We see that the first part of the ticket (the deck) has been extracted successfully
table(comb_data$cabin_number) #We have leftovers from the tickets with several cabins. We will clean those next.

regnum <- "[[:digit:]]+" 

comb_data$cabin_number <- str_extract(comb_data$cabin_number, regnum) #We extract just the digits from the cabin number column

table(comb_data$cabin_number)

data.frame(comb_data$cabin_letter[dup_ind], comb_data$cabin_number[dup_ind], titanic_data$Cabin[dup_ind]) #We can see that we successfully split the problematic columns as devised. The passengers with F were all on the F deck.

comb_data <- comb_data %>%
  mutate(cabin_number = as.integer(cabin_number))
#Now that we have the cabins split into the letters(decks) and numbers(position on boat), we can join them into a new feature. In an effort to reduce granularity, we will reduce the cabin numbers to their position
#on the deck (front, middle, back)

evens <- function(x) subset(x, x %% 2 == 0)
odds <- function(x) subset(x, x %% 2 != 0)

#Due to a complex layout of cabins on the C deck, we define them separately for later
c_front <- c(1:54, 56, 58, 60)
c_mid <- sort(c(odds(55:83), evens(62:90), odds(95:103), 104:122, evens(124:128), evens(130:140)))
c_back <- sort(c(odds(85:93), evens(92:102), odds(123:127), evens(142:148)))

comb_data <- comb_data %>%
  mutate(cabin_position = case_when(cabin_letter == "A" & cabin_number < 36 ~ "AF", #We divide the decks in three parts (front - F, middle - M, and back - B)
                                    cabin_letter == "A" & cabin_number >= 36 ~ "AB",
                                    cabin_letter == "B" & cabin_number <= 50 ~ "BF",
                                    cabin_letter == "B" & cabin_number >  50 ~ "BM",
                                    cabin_letter == "C" & cabin_number %in% c_front ~ "CF",
                                    cabin_letter == "C" & cabin_number %in% c_mid ~ "CM",
                                    cabin_letter == "C" & cabin_number %in% c_back ~ "CB",
                                    cabin_letter == "D" & cabin_number <= 50 ~ "DF",
                                    cabin_letter == "D" & cabin_number > 50 ~ "DB",
                                    cabin_letter == "D" & is.na(cabin_number) ~ "DB",
                                    cabin_letter == "E" & cabin_number <= 27 | cabin_number %in% 200:203 ~ "EF",
                                    cabin_letter == "E" & cabin_number %in% 28:76 ~ "EM",
                                    cabin_letter == "E" & cabin_number %in% 77:167 ~ "EB",
                                    cabin_letter == "F"  ~ "FB",
                                    cabin_letter == "G"  ~ "G",
                                    cabin_letter == "T" ~ "Unknown",  #As there is only one case
                                    is.na(cabin_letter) ~ "Unknown"))

#Now that we have our cabin_position variable, we can delete the cabin_number as well as transform all of the other variables in the correct type. We will keep the cabin letter (deck) as it might be a better predictor

comb_data <- comb_data %>%
  select(- c(cabin_number, total)) %>%
  mutate(Sex = factor(Sex),
         Embarked = factor(Embarked),
         cabin_position = factor(cabin_position))


comb_data$cabin_letter <- ifelse(is.na(comb_data$cabin_letter), "Unknown", comb_data$cabin_letter)
comb_data$cabin_letter <- as.factor(comb_data$cabin_letter)

table(comb_data$cabin_position) #Some of them are very rare, might be problematic
summary(comb_data)

#We continue with dealing with NA's from the Age variable. We will download a table from Wikipedia and try to join it to our original data

page <- read_html("https://en.wikipedia.org/wiki/Passengers_of_the_Titanic") #Wikipedia page about Titanic passengers

alltables <- page %>% html_table(fill = T) #We read the wikipedia website contents

table1 <- alltables[[2]] #We extract the 3 tables that contain information about the passengers
table2 <- alltables[[3]]
table3 <- alltables[[4]]

head(table1)
head(comb_data)

name_remove <- c("Mr.", "Miss.", "Master.", "and", "chauffeur", "maid", "nurse", "valet", "manservant", "Reverend", " dragoman", "clerk", "secretary") #We list common words, which were parts of names, but weren't names

titanic_data_names <- comb_data #We copy the datset before manipulation

titanic_data_names$Name <- str_remove_all(titanic_data_names$Name, paste(name_remove, collapse = "|")) #We remove all the titles from the names
titanic_data_names$Name <- gsub("[[:punct:]]", "", titanic_data_names$Name) #We remove all the punctuation from the names

table3 <- select(table3, - 'Home country') %>% #We remove the additional infromation from table 3, which is not important for us
  mutate(Age = age) %>%
  select(- age)

wikitable <- bind_rows(table1, table2, table3) #Bind all three tables together

wikitable <- wikitable %>% #Selecting just the name and the age from the wikipedia tables
  select(Name, Age)

wikitable$Name <- str_remove_all(wikitable$Name, paste(name_remove, collapse = "|"))
wikitable$Name <- gsub("[[:punct:]]", "", wikitable$Name)
wikitable$Name <- str_remove_all(wikitable$Name, "[:digit:]") #Cleaning the entries

join_table <- left_join(titanic_data_names, wikitable, by = "Name", suffix = c(".kaggle", ".wiki")) #We join the two tables, prefering the original
sum(is.na(join_table$Age.wiki)) #We see that joining didn't work perfectly. There is 800 resulting NA's 
view(join_table) #One case of data missing from the original dataset and a bad join is for instance Passenger ID 20

join_table[20, ] #Taking a look at it, we focus on the name
join_table[20, ]$Name

wikitable[grepl("Masselmani", wikitable$Name), ] #Searching for Masselmani finds no match in the data from Wikipedia
wikitable[grepl("Fatima", wikitable$Name), ] #Searching for first name, we can find the match. The reason is a slightly differently spelled surname.

join_table[(is.na(join_table$Age.kaggle) & is.na(join_table$Age.wiki)), ] #In total we have 154 cases, where we dont have the data in the originial dataset, nor was there found a match in the wikipedia one

join_table$Age.wiki <- as.numeric(join_table$Age.wiki)

join_table <- join_table %>%
  mutate(Age = case_when(!is.na(Age.kaggle) ~ Age.kaggle,
                         is.na(Age.kaggle) & !is.na(Age.wiki) ~ Age.wiki,
                         TRUE ~ NA_real_)) %>%
  select(-c(Age.kaggle, Age.wiki)) #We supplement the original values with the ones we got from wikipedia, where applicable

sum(is.na(join_table$Age)) #We have 156 missing Age values in the joined table
sum(is.na(comb_data$Age)) #There was 263 missing Age values in total, so it worked to some extent

#Instead of manually filling in the rest of the 156 NAs, we're going to impute the values based on the Pclass and the number of siblings
which(duplicated(join_table)) #Check for duplicates after joining

join_table[697:698,] #Taking a look at the two duplicates to confirm
join_table[893:894, ]
join_table[c(698, 894), ] #We see there are Two Kelly James's in the dataset

join_table <- join_table[-c(698,894), ] #Removing the duplicates
which(duplicated(join_table)) #No duplicates left

titanic_data_clean <- join_table

median(titanic_data_clean$Age, na.rm = T) #Median age of whole training dataset is 28

avg_ages <- titanic_data_clean %>% #We make a table grouped by Passenger Class and number of siblings on board
  drop_na() %>%
  group_by(Pclass, SibSp) %>%
  summarize(avimp = round(median(Age))) %>%
  ungroup()

avg_ages #we can see the median age values based on the Class and number of siblings which we will use to impute the missing data with

titanic_data_clean <- titanic_data_clean %>%
  mutate(Age = case_when(!is.na(Age) ~ Age,
                         is.na(Age) & Pclass == 1 & SibSp == 0 ~ avg_ages[[1,3]], #We manually impute based on the grouped values
                         is.na(Age) & Pclass == 1 & SibSp == 1 ~ avg_ages[[2,3]],
                         is.na(Age) & Pclass == 1 & SibSp == 2 ~ avg_ages[[3,3]],
                         is.na(Age) & Pclass == 1 & SibSp >= 3 ~ avg_ages[[4,3]],
                         is.na(Age) & Pclass == 2 & SibSp == 0 ~ avg_ages[[5,3]],
                         is.na(Age) & Pclass == 2 & SibSp == 1 ~ avg_ages[[6,3]],
                         is.na(Age) & Pclass == 2 & SibSp == 2 ~ avg_ages[[7,3]],
                         is.na(Age) & Pclass == 2 & SibSp >= 3 ~ avg_ages[[8,3]],
                         is.na(Age) & Pclass == 3 & SibSp == 0 ~ avg_ages[[9,3]],
                         is.na(Age) & Pclass == 3 & SibSp == 1 ~ avg_ages[[10,3]],
                         is.na(Age) & Pclass == 3 & SibSp == 2 ~ avg_ages[[11,3]],
                         is.na(Age) & Pclass == 3 & SibSp == 3 ~ avg_ages[[12,3]],
                         is.na(Age) & Pclass == 3 & SibSp == 4 ~ avg_ages[[13,3]],
                         is.na(Age) & Pclass == 3 & SibSp == 5 ~ avg_ages[[14,3]],
                         is.na(Age) & Pclass == 3 & SibSp >= 6 ~ avg_ages[[15,3]],
                         TRUE ~ NA_real_),
         Embarked = ifelse(is.na(Embarked), "S", Embarked),
         fare_real = ifelse(is.na(fare_real), median(fare_real, na.rm = T), fare_real)) #We also fill in the 2 NA's from Embarked with the most common value, and the missing fare_real value


titanic_data_clean[1:891, ] %>%
  group_by(cabin_position) %>%
  mutate(sur_rate = mean(as.numeric(Survived) - 1), tot = n()) %>%
  ungroup() %>%
  ggplot(aes(x = cabin_position, y = sur_rate)) +
    geom_point() +
    geom_text(aes(label = tot, vjust = -1), size = 3) #Looking at the survival distribution by the cabin_position


titanic_data_clean[1:891, ] %>%
  group_by(cabin_letter) %>%
  mutate(sur_rate = mean(as.numeric(Survived) - 1), tot = n()) %>%
  ungroup() %>%
  ggplot(aes(x = cabin_letter, y = sur_rate)) +
  geom_point() +
  geom_text(aes(label = tot, vjust = -1), size = 3) #Looking at the survival distribution by deck


titanic_data_clean$Embarked <- as.factor(titanic_data_clean$Embarked) #Transform it to a factor. We now have a clean dataset without NAs


ggplot(titanic_data_clean[1:891, ], aes(x = Survived, y = Age)) +
  geom_violin()

titanic_data_clean$child <- as.factor(ifelse(titanic_data_clean$Age <= 15, 1, 0)) #We also define a new variable children, which is 1 for every person under 15 years old

age_survival <- titanic_data_clean[1:891, ] %>%
                  group_by(Age) %>%
                  summarise(survival = mean(as.numeric(Survived) - 1), tot = n())

ggplot(age_survival, aes(x = Age, y = survival)) +
  geom_point(aes(size = tot)) +
  geom_vline(xintercept = 15) + #Before the cutoff at age 15, we see some higher survival rates 
  geom_hline(yintercept = mean(as.numeric(titanic_data_clean[1:891, ]$Survived) - 1))


titanic_data_mod <- titanic_data_clean %>% 
  select(-c(PassengerId, Name, Ticket, Embarked, Fare)) %>% #We remove variables we won't use in modeling
  mutate(Survived = as.factor(Survived), #We turn the survived and Pclass variables to factors
         Pclass = as.factor(Pclass))

summary(titanic_data_mod) #Everything seems clean and ready for modelling.


#We split the combined dataset back into the training and test dataset
mod_train <- subset(titanic_data_mod, train == 1)
mod_test <- subset(titanic_data_mod, train == 0)

mod_train <- select(mod_train, -train)
mod_test <- select(mod_test, -train)


##### Modeling using caret

set.seed(2058)
train_index <- createDataPartition(mod_train$Survived, p = 0.7, #Creating a partition to evaluate our models
                                   list = F,
                                   times = 1)


train <- mod_train[train_index, ]
test <- mod_train[-train_index, ]

fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

xgb_grid <- expand.grid(max_depth = 1:10,
                        eta = seq(0,1,0.05),
                        gamma = seq(0, 20, 2),
                        min_child_weight = seq(0, 20, 2),
                        subsample = c(0.1, 0.3, 0.5))

#XGBoost
set.seed(845)
xgbfit1 <- train(Survived ~ ., data = train[, -6],
                 method = "xgbTree",
                 trControl = fit_control)

xgbfit1$finalModel

xgbprobs <- predict(xgbfit1, newdata = test, type = "prob")[, 2]
xgbpreds <- predict(xgbfit1, newdata = test)

mean(xgbpreds == test$Survived) #84.6% accuracy
auc(test$Survived, xgbprobs) #AUC of 0.877

set.seed(751)
xgbfit2 <- train(Survived ~ ., data = train[, -9],
                 method = "xgbTree",
                 trControl = fit_control)

xgbprobs2 <- predict(xgbfit2, newdata = test, type = "prob")[, 2]
xgbpreds2 <- ifelse(xgbprobs2 > 0.5, 1, 0)

mean(xgbpreds2 == test$Survived) #We get 84.3% Accuracy
auc(test$Survived, xgbprobs2) #AUC of 0.878

xgresults <- resamples(list(cabins = xgbfit1, deck = xgbfit2))
summary(xgresults)
dotplot(xgresults)

#GLM
set.seed(753)
glmfit1 <- train(Survived ~ ., data = train[, -6],
                 method = "glmnet",
                 trControl = fit_control)


glmpreds <- predict(glmfit1, newdata = test)
glmprobs <- predict(glmfit1, newdata = test, type = "prob")[, 2]
mean(glmpreds == test$Survived) #80.5% accuracy
auc(test$Survived, glmprobs) #AUC of 0.873

set.seed(754)
glmfit2 <- train(Survived ~ ., data = train[, -9],
                 method = "glmnet",
                 trControl = fit_control)

glmpreds2 <- predict(glmfit2, newdata = test)
glmprobs2 <- predict(glmfit2, newdata = test, type = "prob")[, 2]
mean(glmpreds2 == test$Survived) #78.2% accuracy
auc(test$Survived, glmprobs2) #AUC of 0.864


#Random Forrest
rftrain <- train
levels(rftrain$Survived) <- c("Died", "Survived") #Add names so that we can get class probabilities with Ranger

set.seed(755)
rffit1 <- train(Survived ~ ., data = rftrain[, -6],
                method = "ranger",
                trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, classProbs = T))

rfpreds <- as.numeric(predict(rffit1, newdata = test)) - 1
rfprobs <- predict(rffit1, newdata = test, type = "prob")[, 2]
mean(rfpreds == test$Survived) #84.6% accuracy
auc(test$Survived, rfprobs) #AUC of 0.862


set.seed(756)
rffit2 <- train(Survived ~ ., data = rftrain[, -9],
                method = "ranger",
                trControl = trainControl(method="repeatedcv", number = 10, repeats = 10, classProbs = T))

rfpreds2 <- as.numeric(predict(rffit2, newdata = test)) - 1
rfprobs2 <- predict(rffit2, newdata = test, type = "prob")[, 2]
mean(rfpreds2 == test$Survived) #83.9% accuracy
auc(test$Survived, rfprobs2) #AUC of 0.866



#Bayes GLM
set.seed(757)
bayglmfit <- train(Survived ~ ., data = train[, -6],
                method = "bayesglm",
                trControl = fit_control)

bayglmpreds <- predict(bayglmfit, newdata = test)
bayglmprobs <- predict(bayglmfit, newdata = test, type = "prob")[, 2]
mean(bayglmpreds == test$Survived) #80.1% accuracy
auc(test$Survived, bayglmprobs) #AUC of 0.871



#Neural Networks
set.seed(758)
nnfit <- train(Survived ~ ., data = train[, -6],
               method = "avNNet",
               trControl = fit_control)

nnpreds <- predict(nnfit, newdata = test)
nnprobs <- predict(nnfit, newdata = test, type = "prob")[, 2]
mean(nnpreds == test$Survived) #80.5% accuracy
auc(test$Survived, nnprobs) #AUC of 0.864



#GLM Boost
set.seed(759)
glmbfit <- train(Survived ~ ., data = train[, -6],
               method = "glmboost",
               trControl = fit_control)

glmbpreds <- predict(glmbfit, newdata = test)
glmbprobs <- predict(glmbfit, newdata = test, type = "prob")[, 2]
mean(glmbpreds == test$Survived) #79.4% accuracy
auc(test$Survived, glmbprobs) #AUC of 0.868



#ADA Boost
set.seed(760)
adafit <- train(Survived ~ ., data = train[, -6],
                 method = "ada",
                 trControl = fit_control)

adapreds <- predict(adafit, newdata = test)
adaprobs <- predict(adafit, newdata = test, type = "prob")[, 2]
mean(adapreds == test$Survived) #82.4% accuracy
auc(test$Survived, adaprobs) #AUC of 0.876


#SVM model
set.seed(761)
svmfit <- train(Survived ~ ., data = train[, -6],
                method = "svmRadialSigma",
                trControl = fit_control,
                prob.model = TRUE)

svmpreds <- predict(svmfit, newdata = test)
svmprobs <- predict(svmfit, newdata = test, type = "prob")[, 2]
mean(svmpreds == test$Survived) #83.1% Accuracy
auc(test$Survived, svmprobs) #0.831 AUC


#LDA model
set.seed(762)
ldafit <- train(Survived ~ ., data = train[, -6],
                method = "lda",
                trControl = fit_control)

ldapreds <- predict(ldafit, newdata = test)
ldaprobs <- predict(ldafit, newdata = test, type = "prob")[, 2]
mean(ldapreds == test$Survived) #80.1% Accuracy
auc(test$Survived, ldaprobs) #0.867 AUC



#KNN model
set.seed(763)
knnfit <- train(Survived ~ ., data = train[, -6],
                method = "knn",
                trControl = fit_control)

knnpreds <- predict(knnfit, newdata = test)
knnprobs <- predict(knnfit, newdata = test, type = "prob")[, 2]
mean(knnpreds == test$Survived) #73% Accuracy
auc(test$Survived, knnprobs) #AUC of 0.80


#Stacked model
stack_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = T, classProbs = T)
algorithm_list <- c("xgbTree", "glmnet", "ranger", "bayesglm", "avNNet", "glmboost", "ada", "svmRadialSigma", "lda", "knn")

set.seed(764)
stack_models <- caretList(Survived ~ ., data = rftrain[, -6], 
                          trControl = stack_control,
                          methodList = algorithm_list)

stack_results <- resamples(stack_models)
summary(stack_results) #We can see many models performing similarly well, with ranger and XGBoost and NN performing the best
dotplot(stack_results)

modelCor(stack_results) #As expected, there is strong correlation (> 0.75) between XGBoost and ranger and the various types of GLMs
splom(stack_results)

#Due to the correlations, we will choose the algorithms that are the best performing and least correlated. So we choose xgbTree, beacuse it has lower correlation with the rest than ranger
#Out of the GLM's we choose glmboost, because it has the lowest correlation with avNNet, avNNet and knn.

stack_models_sel <- stack_models[c(1,5,6,10)]

set.seed(765)
stack_glm <- caretStack(stack_models_sel, method = "glmnet", metric = "Accuracy", trControl = stack_control)
print(stack_glm)

stack_preds <- as.numeric(predict(stack_glm, newdata = test)) - 1
stack_probs <- 1 - predict(stack_glm, newdata = test, type = "prob")

mean(stack_preds == test$Survived) #We get 83.5% Accuracy
auc(test$Survived, stack_probs) #We get an AUC of 0.888


#Training on whole training data
final_dat <- mod_train
final_dat <- final_dat %>%
  select(-c(cabin_letter, dup_tic))
levels(final_dat$Survived ) <- c("Died", "Survived")


set.seed(766)
stack_models_final <- caretList(Survived ~ ., data = final_dat, 
                          trControl = stack_control,
                          methodList = algorithm_list)

final_resamples <- resamples(stack_models_final)
summary(final_resamples)  
dotplot(final_resamples)

modelCor(final_resamples)
splom(final_resamples)


#Based on the training on the whole dataset, we come up with a slightly modified final algorithm selection
final_algorithm_list <- c("xgbTree", "avNNet", "glmboost", "svmRadialSigma")

set.seed(767)
selected_final_models <- caretList(Survived ~ ., data = final_dat,
                               trControl = stack_control,
                               methodList = final_algorithm_list)


set.seed(768)
final_model <- caretStack(selected_final_models, method = "glm", metric = "Accuracy", trControl = stack_control)
print(final_model)

testprobs2 <- 1 - predict(final_model, type = "prob")
testpreds2 <- ifelse(testprobs2 > 0.5, 1, 0)

mean(testpreds2 == as.numeric(final_dat$Survived) - 1) #91.6% Accuracy on the training set
auc(as.numeric(final_dat$Survived) - 1 , testpreds2) #0.899 AUC

final_probs <- 1 - predict(final_model, newdata = mod_test, type = "prob")
final_preds <- as.numeric(predict(final_model, newdata = mod_test)) - 1

submission_data <- test_data
submission_data$Survived <- final_preds

submission_data <- submission_data %>%
  select(c(PassengerId, Survived))

write.csv(submission_data, file = "gfistro_titanic_submit_3_11s2.csv", row.names = F)

