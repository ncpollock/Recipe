# START --------------------------------------------------
shinyUI(
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css")
    , navbarPage(id = "tabs"
               # theme = shinytheme("cosmo"),
               , theme = bs_theme(bootswatch = "cosmo"
                                  , bg = "white"
                                  , fg = "#131D2D"
                                  , primary = 'orange'
                                  # , dark = 'red'
                                  )
               # title = p(strong("Recipes"),style=paste0("color:",c_1)),
               , title = "Recipes"
               , windowTitle = "Recipes"
               # , footer = site_footer
               , tabPanel(
                 'Browse', icon = icon("search")
                 , tags$script(my_navbar_script) # adds my name in navbar
                 , tags$table(style = "width: 100%",class = 'bg-dark',
                              tags$tr(tags$td(style = "width: 15%",h1("Filters:")),
                                      tags$td(style = "width: 25%"
                                                # , align = "center"
                                              , selectInput("MealType", label="Meal Type",
                                                          choices = c('All',mealtype.df$MealType), 
                                                          selected = 'All')),
                                        tags$td(style = "width: 25%",sliderInput('total_time','Prep + Cook Time',10,max(v.food.df$total_time),59,step = 5)),
                                      tags$td(style = "width: 25%",materialSwitch('meal_prep','Meal Prep Only?',status = 'primary'))
                                      
                                      ) # tr: table row
                              ) # input table
                 # , fluidRow(class = 'bg-dark' # style = "background-color: #0892d0;" # id="filters",class="sticky"
                 #           , column(2,h1("Filters:"))
                 #           , column(2,selectInput("MealType", label="Meal Type",
                 #                                  choices = c('All',mealtype.df$MealType), 
                 #                                  selected = 'All')) # mealtype.df$MealType[3]
                 #           , column(3,sliderInput('total_time','Prep + Cook Time',10,max(v.food.df$total_time),59,step = 5))
                 #           , column(2,materialSwitch('meal_prep','Meal Prep Only?',status = 'primary'))
                 # ) # input fluidRow
                 , fluidPage( # gives some padding
                   fluidRow(DTOutput("recipes"))
                   , br(),hr(style = "width: 85%;margin: auto;height: 5px;background-color: orange;"),br()
                   , uiOutput('servingUI')
                   , fluidRow(DTOutput("ingredients")
                              , DTOutput("steps")
                               ) # mainPanel
                 ) # fluidPage
                 ) # tabPanel
               
               , tabPanel(
                 'Plan', icon = icon("calendar")
                 , tabsetPanel(
                   tabPanel("Calendar"
                            , fluidPage(
                              sidebarLayout(
                                sidebarPanel(width=3
                                             , selectInput("timeslot", label="Time Slot",
                                                           choices = list("11:00 - 12:00" = 11, "12:00 - 1:00" = 12, "1:00 - 2:00" = 13), 
                                                           selected = 12)
                                             , p("Make a selection....")
                                             , actionButton("submit_sched", "Submit")
                                             
                                ),
                                
                                mainPanel(
                                  # DT::dataTableOutput("calendar_dt")
                                  br(), br()
                                )
                              )
                            )),
                   tabPanel("Shopping List", 
                            fluidPage(fluidRow(
                              p("This feature is still in development. 
                   See the list of ingredients needed in order to cook all meals for the month."),
                              uiOutput("scheduled_meets"))))
                 )),
               
               tabPanel(
                 'Analytics', icon = icon("bar-chart")
                 , h1("Popular Ingredients")
                 , h1("Market Basket Analysis for Ingredients")
                 # , DT::dataTableOutput("analytics_dt")
               ),
               tabPanel(
                 'About/Help', icon = icon("question-circle")
                 , sidebarLayout(
                   sidebarPanel(
                     box(width = 12,collapsible = FALSE, collapsed = FALSE,
                           title = "Site Administrator",
                           p(strong("Name: "),"Noah Pollock"),
                           p(strong("Email: "),"noahpollock@mycompany.com"))
                     , box(width = 12,collapsible = FALSE, collapsed = FALSE,
                           title = "Data Policy",
                           p("The data entered on this site is retained in a private Google Sheet and is 
            used exclusively for providing access to and maintaining the full functionality of this site. 
            Data entered is never deliberately shared or distributed beyond individual users gaining access. 
            Please contact the Site Administrator for more information or to request that 
            your data be deleted."))
                   )
                   , mainPanel(h1("What is this?")
                               , p("This web application helps users choose and create simple meals.")
                                , h1("Resources")
                                , tags$ul(
                                  tags$li(tags$b("Research:")," ")
                                  , tags$li(tags$b("More Detailed:")," ")
                                  , tags$li(tags$b("Advanced Topics:")," "))
                 ) # mainPanel
                     ) # sideBarLayout
                   ) # tabPanel
               , tabPanel('Source Code',icon = icon("sign-out"),tags$style("float: right;"))
    ) # navBarPage
    , site_footer
  ))
# END -----------------------------------------------------------------------