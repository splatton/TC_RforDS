#First, let's tell our IDE to NOT save anything into the environment when quitting

#install.packages("usethis")

usethis::use_blank_slate()

#Look at your working directory

getwd()

#you can set your working directory with setwd()

source("LessonScripts/Lesson5.R")

#install git on your computer

#Use the usethis package to set up git with R

library(usethis)
use_git_config(user.name = "Jane Doe", user.email = "jane@example.org")

usethis::git_default_branch_configure()

usethis::create_github_token()

#Generate token

#Leave window open and copy the token

gitcreds::gitcreds_set()

#Enter token here

#Restart session

#If you have problems getting github on your computer https://happygitwithr.com/rstudio-see-git

