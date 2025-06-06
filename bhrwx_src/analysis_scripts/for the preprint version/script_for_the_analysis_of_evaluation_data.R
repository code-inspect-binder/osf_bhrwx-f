# R code for: social ball experiential learning study
# evaluation data 
# Author: ##
# Date: February 23, 2022

# note: Please save the script in the directory you will be working 
# and then update the working directory accordingly.

## load the libraries
##call the package from the library 
library(tidyverse);library(qualtRics); library(psych); library(magrittr); library(lme4); library(jtools); library(pscl); library(cowplot); library(sjstats); library(likert); library(sjPlot)
stderror <- function(x) sd(x)/sqrt(length(x))
options(scipen = 999)

# load the data
eval_data <- readxl::read_xlsx("evaluation_data/anon_evaluation_data.xlsx") #please update file directory for the data in relation to your script

# prepping the variables ----
# Create an order (subject) variable
eval_data$order <- 1:nrow(eval_data)


# We need some variables to be factors 
names <- c("gender", "school_type")
eval_data[,names] <- lapply(eval_data[,names], factor)

# Some questions have values ranging form
# "completely agree to completely disagree" as answers (in Dutch)  
# we change them into English equivalents
eval_data[eval_data == "Helemaal eens"] <- "Completely agree"
eval_data[eval_data == "Eens"] <- "Agree"
eval_data[eval_data == "Neutraal"] <- "Neutral"
eval_data[eval_data == "Oneens"] <- "Disagree"
eval_data[eval_data == "Helemaal oneens"] <- "Completely disagree"
eval_data[eval_data == "Niet van toepassing"] <- NA

# questions about talking about the program also have answer values 
# that are in Dutch, we change them to English 
eval_data[eval_data == "1 of 2 keer"] <- "One or two times"
eval_data[eval_data == "3 tot 5 keer"] <- "Three to five times"
eval_data[eval_data == "Meer dan 5 keer"] <- "More than five times"
eval_data[eval_data == "Niet"] <- "Not at all"

# Same for yes/no items
eval_data[eval_data == "Nee"] <- "No"
eval_data[eval_data == "Ja"] <- "Yes"

# FIGURE 3 -----

# Save the variables that will be used in Figure 3 in a new data frame
figure_3 <- select(eval_data, starts_with("exclusion"), -exclusion_rating, starts_with("followup"), facilitator_think)

# Turn questions into factors with the correct factor order for answer values
figure_3 <- figure_3 %>% 
  mutate(across(where(is_character), factor,
                levels = c("Completely disagree", "Disagree", "Neutral", "Agree", "Completely agree")))

figure_3_items <- as.data.frame(figure_3) # save as a data frame

#changing the name of the items English 
names(figure_3_items) <-c(
  exclusion_thinking = "I got to think about social exlcusion.",
  exclusion_insight_effect = "I have gained insight into the effect of being socially excluded.",
  exclusion_insight_role = "I have gained insight into my own role in situations where people get sociall excluded.",
  followup_leuk = "The debriefing lesson was nice.",
  followup_nuttig = "The debriefing lesson was useful.",
  followup_onzining = "The debriefing lesson was nonsensical (R).",
  followup_interessant = "The debriefing lesson was interesting.",
  followup_experience = "In the debriefing lesson we  shared our experiences in the containers.",
  followup_atmosphere = "In the debriefing lesson we discussed how is the environment in our class.",
  followup_agreement = "In the debriefing lesson we made (new) agreements about how we treat each other.",
  followup_prejudice = "In the debriefing lesson we further reflected on prejudice.",
  facilitator_think = "The facilitator asked me questions that made me think.")

likert_frame <- likert(figure_3_items) 

p<- plot(likert_frame, 
     group.order = names(figure_3_items),
     colors = c("gray81", "gray71", "gray61", "gray51", "gray41"),
     text.size = 4) #plot the items

p + legend_style(pos = "bottom", base.theme = theme_sjplot2(base_size = 19)) #apply aesthetics 

ggsave("likert_all.pdf", width = 15, height = 10, dpi = 600) #save the graph

# FIGURE 4 -------
#save the variables that we'll be using in figure 3 in another data frame  

figure_4 <- select(eval_data, starts_with("talk"))

# turn them into factors with the correct factor order
figure_4 <- figure_4 %>% 
  mutate(across(where(is_character), factor,
                levels = c("Not at all", "One or two times", "Three to five times", "More than five times")))

figure_4_items <- as.data.frame(figure_3)

names(figure_4_items) <-c(
  talk_classmates = "Na de expeditie [Hoe vaak heb je het er met klasgenoten over gehad?]",
  talk_home = "Na de expeditie [Hoe vaak heb je het er thuis over gehad (met bijv. je ouders, broer of zus)?]",
  talk_friends = "Na de expeditie [Hoe vaak heb je het er met vrienden buiten school (bijv. van de sportclub) over gehad?]")

# item names in english 
names(figure_4_items) <-c(
  talk_classmates = "After the program, how often did you talk about it with your classmates?",
  talk_home = "After the program, how often did you talk about it at home (e.g., with your parents, brother or sister)?",
  talk_friends = "After the program, how often did you talk about it with your friends outside your school (e.g., from the sports club)?")

likert_frame_figure4 <- likert(figure_4_items)
p<- plot(likert_frame_figure4, 
     group.order = names(figure_4_items),
     center = 1.5,
     colors = c("gray51", "gray61", "gray71", "gray81"),
     text.size = 4)
p + legend_style(pos = "bottom", base.theme = theme_sjplot2(base_size = 17))

ggsave("likert_talking.pdf", width = 15, height = 4, dpi = 600)



# Did participants do something different after the training? 
# save variables as factor
eval_data$witness_situation <- as.factor(eval_data$witness_situation)
eval_data$done_different <- as.factor(eval_data$done_different)

# group participants based on their answers to the questions 
df <- eval_data %>%
  group_by(witness_situation, done_different) %>%
  summarise(counts = n())
df

# tabulate the results to see the breakdown of participants
# who have witnessed something and done something different
table(eval_data$witness_situation)

# check Descriptive statistics  -----
# gender breakdown
df <- eval_data %>%
  group_by(gender) %>%
  summarise(counts = n())
df


# show breakdown of school type 
# there is no age variable, so we extrapolate from the school type and year 
# breakdown of school type
df <- eval_data %>%
  group_by(school_type) %>%
  summarise(counts = n())
df

# breakdown of school year by school type
df <- eval_data %>%
  group_by(school_type, school_year) %>%
  summarise(counts = n())
df
