library(tidyverse)
library(mosaic)
library(ggplot2)
library(plyr)
library(aod)


seru <- read.csv("SERU2017.csv")


# First I concatenate all the different operating systems for mobile and pcs. Side note, I put iPad and iPod touch in
# with mobile to keep things binary for the moment. There were too few tokens to make a third level (i.e., tablet). 

mobile <- c("iPhone", "Windows Phone 10.0", "Windows Phone 8.1", "Android 4.1.2", 
            "Android 4.3", "Android 4.4.2", "Android 4.4.4", "Android 5.0", "Android 5.0.1", 
            "Android 5.0.2", "Android 5.1", "Android 5.1.1", "Android 6.0", "Android 6.0.1", 
            "Android 7.0", "Android 7.1.1", "Android 7.1.2", "iPad", "iPod touch")

pc <- c("CrOS armv7l 8743.85.0" , "CrOS armv7l 8872.76.0", "CrOS armv7l 9000.91.0", "CrOS armv7l 9202.60.0",
        "CrOS armv7l 9202.64.0", "CrOS x86_64 8872.70.0", "CrOS x86_64 8872.76.0", "CrOS x86_64 9000.91.0", 
        "CrOS x86_64 9202.56.1", "CrOS x86_64 9202.60.0", "CrOS x86_64 9202.64.0", "CrOS x86_64 9334.18.0", 
        "Fedora", "FreeBSD amd64", "Linux x86_64", "Macintosh", "MSIE 9.0", "Ubuntu", "Windows NT 10.0", 
        "Windows NT 5.1", "Windows NT 6.0", "Windows NT 6.1", "Windows NT 6.2", "Windows NT 6.3")

os <- c(mobile, pc)

# Here I select our variables of interest and filter seru to only look at New Brunswick campus. I then mutate "finish" 
# and "OS1" 
survey <- seru %>%
  select(FINISHED, OS1, CAMPUS_Supplemental) %>%
  filter(CAMPUS_Supplemental == "New Brunswick") %>%
  mutate(finished = factor(dplyr::case_when(
    FINISHED  == "0" ~ 0,
    FINISHED == "1" ~ 1
  ), levels = c(0:1), labels = c("No", "Yes"))) %>%
  mutate(mobile = OS1 %in% mobile,
         pc = OS1 %in% pc)


# After cleaning up the data set, we're ready to run a logistic regression. I first fit the model with mobile phones. 
test1 <- glm(formula = finished ~ mobile, family = "binomial"(link = "logit"), 
             data = survey)

summary(test1)


# After, I try it out for pc. 
test2 <- glm(formula = finished ~ pc, family = "binomial"(link = "logit"), 
             data = survey)
summary(test2)