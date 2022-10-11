library(tidyverse)

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
                                    cabin_letter == "E" & cabin_number <= 27 | cabin_number %in% 200:203 ~ "EF",
                                    cabin_letter == "E" & cabin_number %in% 28:76 ~ "EM",
                                    cabin_letter == "E" & cabin_number %in% 77:107 ~ "EB",
                                    cabin_letter == "F"  ~ "FB",
                                    cabin_letter == "G"  ~ "GB",
                                    is.na(cabin_letter) ~ "Other"))

#Now that we have our cabin_position variable, we can delete the cabin_letter and cabin_number as well as transform all of the other variables in the correct type.

titanic_data_clean <- titanic_data_clean %>%
  select(- c(cabin_letter, cabin_number, total)) %>%
  mutate(Sex = factor(Sex),
         Embarked = factor(Embarked),
         cabin_position = factor(cabin_position))