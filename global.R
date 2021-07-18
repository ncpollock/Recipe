# GLOBAL START ------------------------------------
library(googlesheets4)
library(shiny)
library(shinydashboard) # just for some aesthetics eg box()
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
# library(shinythemes) # consider bslib instead
library(bslib) # add bs_themer() to server

gs4_deauth() # query Google Sheets without authenticating as a user.

# read in data from Google Sheets ------------------------------------------
# imagine this is the URL or ID of a Sheet readable by anyone (with a link)
ss <- "https://docs.google.com/spreadsheets/d/1_n0pWIyRXhggoUIzoMC12mwhB-LlCeXD5GaQFXrrpsA/edit?usp=sharing"
food.df <- read_sheet(ss,"Food")
step.df <- read_sheet(ss,"Step")
ingredient.df <- read_sheet(ss,"Ingredient")
food_ing.df <- read_sheet(ss,"Food_Ingredient")
mealtype.df <- read_sheet(ss,"MealType")
ingredienttype.df <- read_sheet(ss,"IngredientType")

# bring in derived / calculated columns
v.food.df <- food.df %>%
  inner_join(step.df %>% 
               group_by(Food_ID) %>% 
               summarise(total_time = sum(Time)
                         , steps = n())
            , by = c("ID" = "Food_ID")) %>%
  inner_join(mealtype.df,by = c("MealType_ID" = "ID"))

v.ingredient.df <- ""
v.food_ing.df <- ""
  

# v.all.df <- food.df %>%
#   inner_join(step.df,by = c("ID" = "Food_ID"))

# if I want to write or read private sheet
# https://stackoverflow.com/questions/63535190/connect-to-googlesheets-via-shiny-in-r-with-googlesheets4
# https://googlesheets4.tidyverse.org/articles/articles/auth.html

# global stylings ----------------------------------
# https://shiny.rstudio.com/articles/themes.html


# style the Data Tables header.
dt_header <- JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#252525', 'color': '#FFFFFF'});",
  "}")

# options -----------------------------------------------------------
options(shiny.maxRequestSize=1000^3,
        shiny.sanitize.errors = TRUE
        # , shiny.trace = TRUE # print to R console?
        , DT.options = list(
          initComplete = dt_header
        #   , pageLength = 25
        , lengthMenu = c(5,20,50)
        #   # , bPaginate=TRUE
        , bFilter=FALSE
          # searching=FALSE
        #   # ordering=FALSE,
        #   # dom = 't'
        )
        )

# END ------------------------------