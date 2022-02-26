# GLOBAL START ------------------------------------
library(shiny)
library(shinydashboard) # just for some aesthetics eg box()
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
# library(shinythemes) # consider bslib instead
library(bslib) # add bs_themer() to server
library(shinyWidgets) # for better checkbox inputs
library(DBI)

# server connection details
db_server <- "recipes.postgres.database.azure.com"
db_name <- "recipe"
db_user <- "goodfood"

con <- dbConnect(RPostgres::Postgres(),
                 host = db_server,
                 dbname = db_name,
                 user = db_user,
                 password = db_password)

# pool <- dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = db_name,
#   host = db_server,
#   username = db_user,
#   password = db_password
# )

# read in data from Google Sheets ------------------------------------------
# imagine this is the URL or ID of a Sheet readable by anyone (with a link)
food.df <- tbl(con,'food') %>% collect()
step.df <- tbl(con,'step') %>% collect()
ingredient.df <- tbl(con,'ingredient') %>% collect()
food_ing.df <- tbl(con,'food_ingredient') %>% collect()
foodtype.df <- tbl(con,'foodtype') %>% collect() # previously mealtype.df
ingredienttype.df <- tbl(con,'ingredienttype') %>% collect()
measure.df <- tbl(con,'measure') %>% collect()

# bring in derived / calculated columns
v.food.df <- food.df %>%
  inner_join(step.df %>% 
               group_by(food_id) %>% 
               summarise(total_time = sum(actiontime)
                         , steps = n())
            , by = c("id" = "food_id")) %>%
  inner_join(foodtype.df,by = c("foodtype_id" = "id"))

# v.ingredient.df <- ""
v.food_ing.df <- food_ing.df %>%
  inner_join(ingredient.df, by = c("ingredient_id" = "id")) %>%
  inner_join(ingredienttype.df, by = c("ingredienttype_id" = "id")) %>%
  inner_join(measure.df, by = c("measure_id" = "id"))
  

# stylings ----------------------------------
# https://shiny.rstudio.com/articles/themes.html
# light_electric_blue <- "#7DF9FF"
electric_blue <- "#0892d0"

# style the Data Tables header.
dt_header <- JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#252525', 'color': '#FFFFFF'});",
  "}")

site_footer <- div(id="site-footer",class = 'bg-dark'
                   , p(strong("Developed by: "),
                       br(),
                       a(href="https://ncpollock.github.io/"
                        ,target="_blank"
                        ,"Noah C. Pollock"),
                       br(),
                       a(href = "https://github.com/ncpollock/"
                        ,target="_blank"
                        ,"Code on GitHub"),
                       align="center"))

my_navbar_info <- gsub("[\r\n]", "",
                       div(
                         a(href="https://ncpollock.github.io/"
                           ,target="_blank"
                           , img(src="headshot.jpg",id="face-img",align="right"))
                         # icon("user fa-pull-right fa-3x") # generic user icon instead of my face
                         , strong("Developed by: "),
                         br(),
                         a(href="https://ncpollock.github.io/"
                           ,target="_blank"
                           ,"Noah C. Pollock")
                         ,style = "float:right;padding-top:5px;white-space:nowrap;"))

my_navbar_script <- HTML(paste0("var header = $('.navbar> .container-fluid');header.append('"
       , my_navbar_info,"');console.log(header)"))

i_checkmark <- "<i class='fa fa-check-circle' style='color:green;'></i>"

# options -----------------------------------------------------------
options(shiny.maxRequestSize=1000^3,
        shiny.sanitize.errors = TRUE
        # , shiny.trace = TRUE # print to R console?
        , DT.options = list(
          initComplete = dt_header
        #   , pageLength = 50
        , lengthMenu = c(5,20,50)
        #   # , bPaginate=TRUE
        , bFilter=FALSE
          # searching=FALSE
        #   # ordering=FALSE,
        #   # dom = 't'
        )
        )


# FUNCTIONS ------------------------------------------------------------

# unit coversions e.g., tablespoons to ounces
conversion.df <- data.frame(
  measure = c('Millileter'
              ,'Teaspoon'
              ,'Tablespoon'
              ,'Ounce'
              ,'Cup'
              ,'Pint'
              ,'Quart'
              ,'Gallon'
  )
  , value = c(3785.41,768,256,128,16,8,4,1)
)

m.conv <- function(i.value,i.measure = 'Cup'){
  # convert between measures of volume
  # i.value = 1, i.measure = 'Cup'
  
  conv.value = (conversion.df %>%
    filter(measure == i.measure) %>%
    mutate(conv.value = i.value/value))$conv.value
  
  conversion.df %>%
    filter(measure != i.measure) %>%
    mutate(value = round(conv.value*value,2))
  
}


# dynamically add input buttons into datatable
init_buttons <- function(n,id_pref, ...){
  # n: number of buttons
  # id_pref: id prefix eg delete_button
  # thanks to: https://stefanengineering.com/2019/07/06/delete-rows-from-shiny-dt-datatable/
  lapply(1:n, function(x){
    as.character(actionButton(paste0(id_pref,"_",x)
                              , label = NULL
                              # static onclick event so that a single reactive can observe when any button is pressed
                              , onclick = paste0('Shiny.setInputValue(\"',id_pref,'\",  this.id, {priority: "event"})')
                              , ...
    ))
  })
}

# extracts index from dynamically generated inputs with pattern input_name_{number}
# eg delete_button_23
get_id_from_input <- function(inp_name) {
  result <- as.integer(sub(".*_([0-9]+)", "\\1", inp_name))
  if (! is.na(result)) result
}


# END ------------------------------