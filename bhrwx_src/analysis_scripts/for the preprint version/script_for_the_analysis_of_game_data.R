# R code for: Experiential learning social exclusion study
# Author: ##
# Date: February 23, 2022

# note: Please save the script in the directory you will be working 
# and then update the working directory accordingly.
# make sure to update the directions for the data file as well! 

##call the package from the library 
library(tidyverse); library(qualtRics); library(psych); library(magrittr); library(lme4); library(jtools); library(pscl); library(cowplot); library("sjstats"); library(lmerTest); library(careless); library(sjPlot); library(openxlsx)

options(scipen = 999) 


# NOTE
# This is a secondary analysis of data provided to us by a
# dutch non-profit organization
# We have removed any potentially identifying information from the data 
# Below is the lines of code that we used the anonymize the data ----
# first we load the raw data
#raw_data <- read.csv("experiential_learning/data/game_data/raw_game_data.csv") 
# We removed the nicknames participants entered to the game interface
# we also removed the "ID" variable that was created by the game 
#anon_data <- select(raw_data, -name, -dat.id)
# There is a session ID variable indicating which participants played the game in the same session.
# We also need to remove this variable.
# Yet, we need a variable indicating the number of participants in each session. 
# we first calculate that variable with the below piece of code
#df <- anon_data %>%
  #group_by(dat.sessionid) %>%
  #summarise(session_n = n())
#df
# merge the data frame with the main data set by the session id 
# we now have a session_n variable indicating how many people were in each session.
#anon_data <- merge(anon_data, df, by = "dat.sessionid")

# We then also remove the "dat.sessionid" variable 
#anon_data <- select(anon_data, -dat.sessionid)

# we export this version of the data
#write.xlsx(anon_data, "anonymized_game_data.xlsx")


# load and clean the data ----
data <- read.xlsx("game_data/anonymized_game_data.xlsx") #update the file direction when doing your analysis based on where the data file is in relation to the location of this script

# rename variables 
data <- rename(data, 
                   belonging = belong,
                   meaningful_existence = invisible,
                   self_esteem = good,
                   control = boss,
                   anger = angry,
                   gender = sex)

# create a subject variable 
data$subject <- 1:nrow(data)

# save gender and age as a factor 
data$gender <- as.factor(data$gender)

# data cleaning ----
# Remove participants  who were in sessions with less than 3 people 
# because the game can only be played with 3 or more people. 
glimpse(data) # current n = 18,855
data <- subset(data, session_n > 2 & session_n < 7)
glimpse(data) # n after removal = 17,801 #removed n  1054

# create a new data a set with a narrower age range only people between and including 12 and 19 yo. 
data <- subset(data, age > 11 & age < 20)
glimpse(data)  # n after removal = 15,363, #removed n = 2438

# create new variable for the IRV = intra-individual response variability
data$irv <- irv(
  dplyr::select(
  data, belonging, control, meaningful_existence, self_esteem
  ), 
  na.rm = TRUE, 
  split = FALSE)

# we apply a conservative criteria and exclude people with the highest (3.46) IRV score
# and the lowest IRV score (0)
# highest IRV is 3.464102
# before doing this we have 15,363 participants 
data <- subset(data, irv > 0 & irv < 3.46)
glimpse(data) # this removes 1298 participants

# create a need threat Score ----
data <- mutate(data,
               belongingR = 8- belonging,
               self_esteemR = 8 - self_esteem,
               controlR = 8 - control)

data$need_threat <- rowMeans(select(data, belongingR, meaningful_existence, self_esteemR, controlR), na.rm = TRUE)

# demographics ----
# check the gender distribution 
table(data$gender)

# tabulate age
describe(data$age)

# count participants by age 
df <- data %>%
  group_by(age) %>%
  summarise(counts = n())

# save age as factor
df$age <- as.factor(df$age)

ggplot(df, aes(x = age, y = counts)) +
  geom_bar(fill = "gray41", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)+ 
  xlab("Age") + 
  ylab("Count") + 
  theme_sjplot2()

# save the age graph 
ggsave("experiential_learning/graphs/age_distribution.png", width = 6, height = 4.5, dpi = 1000)

# calculate alpha for need threat ----
alpha_needthreat <- dplyr::select(data, belongingR, meaningful_existence, self_esteemR, controlR)
psych::alpha(alpha_needthreat)

# NEED THREAT vs midpoint?  ----
overall_mid <- t.test(data$need_threat, mu = 4)
overall_mid
effectsize::cohens_d(data$need_threat ~ 1, mu = 4)

describe(data$need_threat)

# ANGER vs 1? ----
anger_1 <- t.test(data$anger, mu = 1)
anger_1
effectsize::cohens_d(data$anger ~ 1, mu = 1)


# CROSS_CUTTING variables ------
# participant GENDER on NEED THREAT
# focusing only on male and female participants 
# because the number of people who indicate "other" or "missing answers" are very limited 
data_gender <- subset(data, gender != "O")

t.test(need_threat ~ gender, data = data_gender) #run the test

effectsize::cohens_d(need_threat ~ gender, data = data_gender) #calculate eff size 

describeBy(data_gender$need_threat, group = data_gender$gender) #descriptives

# Participants GENDER on ANGER
t.test(anger ~ gender, data = data_gender) #run test

effectsize::cohens_d(anger ~ gender, data = data_gender) #calculate eff size

describeBy(data_gender$anger, group = data_gender$gender) #descriptives


# participant AGE on NEED threat
cor.test(data$need_threat, data$age,  method = "pearson", use = "complete.obs")

# participant AGE on ANGER
cor.test(data$anger, data$age,  method = "pearson", use = "complete.obs")


# AVATAR choice on NEED THREAT
data$avatar <- as.factor(data$avatar) #save avatar choice as factor 
table(data$avatar) #check the distribution of pp for each avatar

avatar.lm <- lm(formula = need_threat ~ avatar, data = data) #run the model
avatar.anova <- car::Anova(avatar.lm, type = 3) #test
avatar.anova # results 
anova_stats(avatar.anova) #results

describe.by(data$need_threat, group = data$avatar) #descriptives

# AVATAR choice on ANGER
avatar.lm.anger <- lm(formula = anger ~ avatar, data = data) #run the model
avatar.anova.anger <- car::Anova(avatar.lm.anger, type = 3) # test
avatar.anova.anger # results 
anova_stats(avatar.anova.anger) #results

describeBy(data$anger, group = data$avatar) #descriptives


# Session size as and NEED TRHEAT
cor.test(data$session_n, data$need_threat,  method = "pearson", use = "complete.obs")

# Session size as and ANGER
cor.test(data$session_n, data$anger,  method = "pearson", use = "complete.obs")
