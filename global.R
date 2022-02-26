# GLOBAL START ------------------------------------
library(shiny)
library(shinydashboard) # just for some aesthetics eg box()
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(bslib)
library(shinyWidgets) # for better checkbox inputs
library(DBI)

con <- dbConnect(RPostgres::Postgres(),
                 host = db_server,
                 dbname = db_name,
                 user = db_user,
                 password = db_password)

# for use in UI, this will need to be in the server eventually
  # ie when I create admin page for adding new types
foodtype.df = tbl(con,'foodtype') %>% collect()
foodtype <- foodtype.df$id
names(foodtype) <- foodtype.df$foodtype

# consider pool
# pool <- dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = db_name,
#   host = db_server,
#   username = db_user,
#   password = db_password
# )

# stylings ----------------------------------
# https://shiny.rstudio.com/articles/themes.html
# light_electric_blue <- "#7DF9FF"
electric_blue <- "#0892d0"
databar_color <- electric_blue

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

# meant to replace boolean values in datatables
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

# extracts index from dynamically generated inputs e.g., init_buttons 
  # with pattern input_name_{number} eg delete_button_23
get_id_from_input <- function(inp_name) {
  result <- as.integer(sub(".*_([0-9]+)", "\\1", inp_name))
  if (! is.na(result)) result
}


# END ------------------------------