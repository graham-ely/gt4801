
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("skimr")
library(tidyverse)
library(readr)
library(skimr)
library(ggplot2)

# Load in data
field_data <- read_csv("Most-Recent-Field-Data-Elements.csv")
# View(field_data)


##############
#  Cleaning  #
##############

# grab all complete cases, then eliminate any rows with "PrivacySupressed" in any field.
# some of the below are redundant in this case bc by the end most rows with missing data are gone already.
clean_fields <- field_data[complete.cases(field_data),]
filter_1 <- filter(field_data, COUNT!="PrivacySuppressed")
filter_2 <- filter(filter_1, DEBTMEDIAN!="PrivacySuppressed")
filter_3 <- filter(filter_2, DEBTPAYMENT10YR!="PrivacySuppressed")
filter_4 <- filter(filter_3, DEBTMEAN!="PrivacySuppressed")
filter_5 <- filter(filter_4, TITLEIVCOUNT!="PrivacySuppressed")
filter_6 <- filter(filter_5, EARNINGSCOUNT!="PrivacySuppressed")
filter_7 <- filter(filter_6, MD_EARN_WNE!="PrivacySuppressed")

# make sure columns are properly typed as numeric—some weren't originally
to_numeric <- transform(filter_7, OPEID6 = as.numeric(OPEID6), 
               CIPCODE = as.numeric(CIPCODE),
               COUNT = as.numeric(COUNT),
               DEBTMEDIAN = as.numeric(DEBTMEDIAN),
               DEBTPAYMENT10YR = as.numeric(DEBTPAYMENT10YR),
               DEBTMEAN = as.numeric(DEBTMEAN),
               TITLEIVCOUNT = as.numeric(TITLEIVCOUNT),
               EARNINGSCOUNT = as.numeric(EARNINGSCOUNT),
               MD_EARN_WNE = as.numeric(MD_EARN_WNE),
               IPEDSCOUNT1 = as.numeric(IPEDSCOUNT1),
               IPEDSCOUNT2 = as.numeric(IPEDSCOUNT2))

filter_8 <- filter(to_numeric, !is.na(IPEDSCOUNT1))
filter_9 <- filter(filter_8, !is.na(IPEDSCOUNT2))


clean_data <- filter_9

# clean up our environment
rm( clean_fields, filter_1, filter_2, filter_3, filter_4, filter_5, filter_6, filter_7, filter_8, filter_9, to_numeric )

summary( clean_data )

#################
#  Exploration  #
#################

# goal: graph #s of top 5 schools and majors in the clean set
skim(clean_data)

# Grab counts of majors and schools
majors  <- table(clean_data$CIPDESC)
schools <- table(clean_data$INSTNM)

# Convert to dataframe and rename columns
majors  <- as.data.frame(majors)
majors  <- majors %>% 
  rename(
    major = Var1,
    freq = Freq
  )
schools  <- as.data.frame(schools)
schools  <- schools %>% 
  rename(
    school = Var1,
    freq = Freq
  )

# Sort by frequency
majors  <- arrange(majors, desc(freq))
schools <- arrange(schools, desc(freq))

# Bar chart of top 5 values in each set
majors %>% 
  arrange(desc(freq)) %>%
  slice(1:5) %>%
  ggplot(., aes(x=major, y=freq, fill=major))+
  geom_col()

schools %>% 
  arrange(desc(freq)) %>%
  slice(1:5) %>%
  ggplot(., aes(x=school, y=freq, fill=school))+
  geom_col()

#############################
#  Analysis & Visualization #
#############################

# start with some linear regression

# debt vs. public/private school and degree level
collegeDebtModel1 <- lm(DEBTMEAN ~ CONTROL + CREDDESC, data=clean_data)
summary( collegeDebtModel1 )

# package with cleaned-up summary
# install.packages("jtools")
library(jtools)

summ(collegeDebtModel1)

# plot the variables at play
ggplot( clean_data, aes(x=DEBTMEAN, y=CREDDESC, color=CONTROL)) +
  geom_point()

cat_plot( collegeDebtModel1, pred=CREDDESC, modx=CONTROL)



# plot the correlation strength of degree level, mean debt, and mean earnings
library(corrplot)
corMatrix <- clean_data[,c(8,13,16)]
corrplot(cor(corMatrix))


