library(tidyverse)
library(rvest)

filepath_train <- "D:/Users/Gregor/Desktop/Kaggle/Titanic/train.csv"
filepath_test <- "D:/Users/Gregor/Desktop/Kaggle/Titanic/test.csv"

titanic_data <- read_csv(filepath_train) #Import training data
test_data <- read_csv(filepath_test) #Import testing data


## Observations
glimpse(titanic_data) #Taking a glimpse, we need to recode Pclass, Sex, Ticket, Cabin and Embarked to factors. Ticket and Cabin seem interesting variables.

table(titanic_data$Ticket) #Taking a look at all the different combinations of tickets.
#We see that most tickets are just numbers, while some have additional info (PC, SOTON, SOC,...). Some are also repeated several times

duplicated_tickets <- titanic_data %>% #We filter out the duplicated tickets
  group_by(Ticket) %>%
  filter(n() > 1) %>%
  ungroup()

summary(titanic_data)
summary(duplicated_tickets) #Compared to the general data, the duplicated tickets have in general higher Fares and more siblings, suggesting they might be group tickets.


## Feature engineering

duplicated_tickets_table <- duplicated_tickets %>%  #We add a total column, to see how many times each ticket is repeated
  group_by(Ticket) %>%
  mutate(total = n()) %>%
  arrange(desc(total)) %>%
  ungroup()

duplicated_tickets_table %>%
  filter(grepl("CA", Ticket))

duplicated_tickets_table %>%
  filter(grepl("S.O.C", Ticket)) #Looking at a couple of examples, it appears that our theory of group tickets might be right. We can see some families forming groups for instance.
#In order for the ticket variable to more closely resemble reality, we will divide the price of the tickets by the duplicates and save the new variable as fare_real

titanic_data_clean <- titanic_data %>%
  group_by(Ticket) %>%
  mutate(total = n()) %>%
  mutate(fare_real = Fare / total) %>%
  ungroup()


table(titanic_data_clean$Cabin) #We see that cabins range from A to G. 
#Seems worth it to try to use it, especially given the diagram of cabins where A is on top and G on bottom - https://upload.wikimedia.org/wikipedia/commons/0/0d/Olympic_%26_Titanic_cutaway_diagram.png
#It seems reasonable to assume people on the upper decks had a higher survival rate than on the bottom. Also the number of the cabin can give us information concerning the relative location of the cabin
#on the ship. The cabin locations are available on the ship's deckplans (https://www.encyclopedia-titanica.org/titanic-deckplans/)

dup_cabs <- c("B51 B53 B55", "B57 B59 B63 B66", "B58 B60", "B82 B84", "B96 B98", "C22 C26", "C23 C25 C27", "C62 C64", "D10 D12", "F E69", "F G63", "F G73") #We isolate the cabins with more than 1 number
dup_ind <- which(titanic_data_clean$Cabin %in% dup_cabs) #We save these in a separate index

print(titanic_data_clean[dup_ind, ], n = 24) #We see that the cabins with more than 1 value relate to cabins occupied by family members or such, so it should be reasonably ok to keep just one of the values

titanic_data_clean <- titanic_data_clean %>%
  separate(Cabin, c("cabin_letter", "cabin_number"), sep = 1) #first we separate the Cabin variable to the letter(floor) and number(location)

table(titanic_data_clean$cabin_letter) #We see that the first part of the ticket (the deck) has been extracted successfully
table(titanic_data_clean$cabin_number) #We have leftovers from the tickets with several cabins. We will clean those next.

regnum <- "[[:digit:]]+" 

titanic_data_clean$cabin_number <- str_extract(titanic_data_clean$cabin_number, regnum) #We extract just the digits from the cabin number column

table(titanic_data_clean$cabin_number)

data.frame(titanic_data_clean$cabin_letter[dup_ind], titanic_data_clean$cabin_number[dup_ind],titanic_data$Cabin[dup_ind]) #We can see that we successfully split the problematic columns as devised

titanic_data_clean <- titanic_data_clean %>%
  mutate(cabin_number = as.integer(cabin_number))
#Now that we have the cabins split into the letters(decks) and numbers(position on boat), we can join them into a new feature. In an effort to reduce granularity, we will reduce the cabin numbers to their position
#on the deck (front, middle, back)

evens <- function(x) subset(x, x %% 2 == 0)
odds <- function(x) subset(x, x %% 2 != 0)

#Due to a complex layout of cabins on the C deck, we define them separately for later
c_front <- c(1:54, 56, 58, 60)
c_mid <- sort(c(odds(55:83), evens(62:90), odds(95:103), 104:122, evens(124:128), evens(130:140)))
c_back <- sort(c(odds(85:93), evens(92:102), odds(123:127), evens(142:148)))

titanic_data_clean <- titanic_data_clean %>%
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
                                    cabin_letter == "G"  ~ "GB",
                                    cabin_letter == "T" ~ "T",
                                    is.na(cabin_letter) ~ "Unknown"))

#Now that we have our cabin_position variable, we can delete the cabin_letter and cabin_number as well as transform all of the other variables in the correct type.

titanic_data_clean <- titanic_data_clean %>%
  select(- c(cabin_letter, cabin_number, total)) %>%
  mutate(Sex = factor(Sex),
         Embarked = factor(Embarked),
         cabin_position = factor(cabin_position))

summary(titanic_data_clean)


page <- read_html("https://en.wikipedia.org/wiki/Passengers_of_the_Titanic") #Wikipedia page about Titanic passengers

alltables <- page %>% html_table(fill = T) #We read the wikipedia website contents

table1 <- alltables[[2]] #We extract the 3 tables that contain information about the passengers
table2 <- alltables[[3]]
table3 <- alltables[[4]]

head(table1)
head(titanic_data_clean)

name_remove <- c("Mr.", "Miss.", "Master.", "and", "chauffeur", "maid", "nurse", "valet", "manservant", "Reverend", " dragoman", "clerk", "secretary") #We list common words, which were parts of names, but weren't names

titanic_data_names <- titanic_data_clean

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
sum(is.na(join_table$Age.wiki)) #We see that joining didn't work perfectly. There is 544 resulting NA's 
view(join_table) #One case of data missing from the original dataset and a bad join is for instance Passenger ID 20

join_table[20, ] #Taking a look at it, we focus on the name
join_table[20, ]$Name

wikitable[grepl("Masselmani", wikitable$Name), ] #Searching for Masselmani finds no match in the data from Wikipedia
wikitable[grepl("Fatima", wikitable$Name), ] #Searching for first name, we can find the match. The reason is a slightly differently spelled surname.

join_table[(is.na(join_table$Age.kaggle) & is.na(join_table$Age.wiki)), ] #In total we have 99 cases, where we dont have the data in the originial dataset, nor was there found a match in the wikipedia one

#join_table$Age.wiki <- as.numeric(join_table$Age.wiki)

join_table <- join_table %>%
  mutate(Age = case_when(!is.na(Age.kaggle) ~ Age.kaggle,
                         is.na(Age.kaggle) & !is.na(Age.wiki) ~ Age.wiki,
                         TRUE ~ NA_real_)) %>%
  select(-c(Age.kaggle, Age.wiki)) #We supplement the original values with the ones we got from wikipedia, where applicable

#Instead of manually filling in the rest 102 NAs, we're going to impute the values based on the Pclass and the number of siblings
titanic_data_clean <- join_table

mean(titanic_data_clean$Age, na.rm = T) #Mean age of whole training dataset is 29.7

avg_ages <- titanic_data_clean %>% #We make a table grouped by Passenger Class and number of siblings on board
  drop_na() %>%
  group_by(Pclass, SibSp) %>%
  summarize(avimp = round(mean(Age))) %>%
  ungroup()

#avg_ages$avimp <- as.numeric(avg_ages$avimp)

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
         Embarked = ifelse(is.na(Embarked), "S", Embarked)) #We also fill in the 2 NA's from Embarked with the most common value

titanic_data_clean$Embarked <- as.factor(titanic_data_clean$Embarked) #Transform it to a factor. We now have a clean dataset without NAs

titanic_data_mod <- titanic_data_clean %>% 
  select(-c(Name, Ticket, Embarked, Fare)) %>% #We remove variables we won't use in modeling
  mutate(Survived = as.factor(Survived), #We turn the survived and Pclass variables to factors
         Pclass = as.factor(Pclass))

summary(titanic_data_mod) #Everything seems clean and ready for modelling.
