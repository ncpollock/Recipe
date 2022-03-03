# GLOBAL START ------------------------------------
# add debug-friendly UI elements e.g., surrogate primary keys
debug_mode <- TRUE

library(shiny)
library(shinydashboard) # just for some aesthetics eg box()
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(bslib)
library(shinyWidgets) # for better checkbox inputs
library(DBI)

con_config <- config::get("dbcon")
con <- dbConnect(con_config$driver,
                 host = con_config$host,
                 dbname = con_config$dbname,
                 user = con_config$user,
                 password = con_config$pwd)

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

# liquid unit conversions
  # rethink to include solids e.g., pounds to ounces
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
  if(n>0){
  lapply(1:n, function(x){
    as.character(actionButton(paste0(id_pref,"_",x)
                              , label = NULL
                              # static onclick event so that a single reactive can observe when any button is pressed
                              , onclick = paste0('Shiny.setInputValue(\"',id_pref,'\",  this.id, {priority: "event"})')
                              , ...
    ))
  })
  } # n > 0
}

# extracts index from dynamically generated inputs e.g., init_buttons 
  # with pattern input_name_{number} eg delete_button_23
get_id_from_input <- function(inp_name) {
  result <- as.integer(sub(".*_([0-9]+)", "\\1", inp_name))
  if (! is.na(result)) result
}


renderAdminDT <- function(tdata,btn_suffix){
  renderDT({
    tdata$Delete <- init_buttons(nrow(tdata),paste0("delete_",btn_suffix), icon = icon("trash"), class = "btn-danger")
    tdata$Edit <- init_buttons(nrow(tdata),paste0("edit_",btn_suffix), icon = icon("pencil-alt"), class = "btn-warning")
    dis_but_order <- list(targets = (ncol(tdata)-1):ncol(tdata)-1, orderable = FALSE) # disable ordering on buttons
    
    datatable(tdata, rownames = FALSE, selection = 'none'
              , options = list(pageLength = 30
                               , columnDefs = list(dis_but_order)
                               , dom = 'tp'
                               )
              ) %>%
                formatStyle(names(tdata), verticalAlign='middle')
  })

}

# END ------------------------------