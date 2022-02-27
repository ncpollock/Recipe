# START --------------------------------------------------
shinyUI(
  tagList(
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"))
    tags$head(includeCSS("www/my_style.css"))
    , navbarPage(id = "tabs"
               # theme = shinytheme("cosmo"),
               , theme = bs_theme(bootswatch = "cosmo"
                                  , bg = "white"
                                  , fg = "#131D2D"
                                  , primary = 'orange'
                                  , info = "#0892d0"
                                  # , dark = 'red'
                                  )
               # title = p(strong("Recipes"),style=paste0("color:",c_1)),
               , title = "RecipEase"
               , windowTitle = "RecipEase"
               # , footer = site_footer
               , tabPanel(
                 'Browse', icon = icon("search")
                 , tags$script(my_navbar_script) # adds my name in navbar
                 , fluidRow(class = 'bg-info'
                           , column(2,h1("Filters:"))
                           , column(2,selectInput("foodtype", label="Food Type",
                                                  choices = c('All',foodtype),
                                                  selected = 'All')) # foodtype.df$foodtype[3]
                           , column(3,sliderInput('total_time','Prep + Cook Time',10,90,60,step = 5))
                           , column(2,br()
                                    , tags$label(class="control-label","Meal Prep Only?"),br()
                                    , materialSwitch('meal_prep','',status = 'primary',inline = TRUE))
                 ) # input fluidRow
                 , br(),fluidPage( # gives some padding
                   fluidRow(DTOutput("recipes"))
                   , br(),hr(),br()
                   , fluidRow(
                     column(6,fluidRow(column(6,uiOutput('servingUI'))
                            , column(6,actionButton("measure_conv","Amount Conversions",icon("calculator")) ))
                    , DTOutput("ingredients"))
                   , column(6,DTOutput("steps") ) )
                   # , fluidRow(DTOutput("ingredients")
                   #            , DTOutput("steps")
                   #             ) # fluidRow
                 ) # fluidPage
                 ) # tabPanel Browse
               
               # , tabPanel(
               #   'Plan', icon = icon("calendar")
               #   , tabsetPanel(
               #     tabPanel("Calendar"
               #              , fluidPage(
               #                sidebarLayout(
               #                  sidebarPanel(width=3
               #                               , selectInput("timeslot", label="Time Slot",
               #                                             choices = list("11:00 - 12:00" = 11, "12:00 - 1:00" = 12, "1:00 - 2:00" = 13), 
               #                                             selected = 12)
               #                               , p("Make a selection....")
               #                               , actionButton("submit_sched", "Submit")
               #                               
               #                  ),
               #                  
               #                  mainPanel(
               #                    # DT::dataTableOutput("calendar_dt")
               #                    br(), br()
               #                  )
               #                )
               #              )) # plan calendar,
               #     tabPanel("Shopping List", 
               #              fluidPage(fluidRow(
               #                p("This feature is still in development. 
               #     See the list of ingredients needed in order to cook all meals for the month."),
               #                uiOutput("scheduled_meets")))) # plan shopping list
               #   )) # Plan
               
               # , tabPanel(
               #   'Analytics', icon = icon("chart-bar")
               #   , h1("Popular Ingredients")
               #   , h1("Market Basket Analysis for Ingredients")
               #   # , DT::dataTableOutput("analytics_dt")
               # ) # analytics
               , tabPanel(
                 'About/Help', icon = icon("question-circle")
                 , sidebarLayout(
                   sidebarPanel(
                     box(width = 12,collapsible = FALSE, collapsed = FALSE,
                           title = "Site Administrator",
                           p(strong("Name: "),"Noah Pollock")
                           # ,p(strong("Email: "),"REDACTED")
                         )
                     , box(width = 12,collapsible = FALSE, collapsed = FALSE,
                           title = "Data Policy",
                           p("The data entered on this site is retained in a cloud-hosted database and is 
            used exclusively for providing access to and maintaining the full functionality of this site. 
            Anyone is free to benefit from the published recipes on this site; however, only authorized users
                             are permitted to add, edit, or delete recipe information."))
                   )
                   , mainPanel(h1("What is this?")
                               , p("This web application helps people choose and create simple meals.")
                                , h1("Resources")
                                , tags$ul(
                                  tags$li(a("NIH Food Groups",href = "https://www.nia.nih.gov/health/know-your-food-groups",target="_blank"))
                                  , tags$li(a("Where to Find Other Recipes",href = "https://www.google.com",target="_blank"))
                                  )
                 ) # mainPanel
                     ) # sideBarLayout
                   ) # tabPanel
               , tabPanel('Admin.',icon = icon("tools"),value = "admin"
                          #tabbox with 'Food','Ingredients','Steps'
                          , div(id = "sign-in"
                                , textInput('username',HTML(paste(icon("user-circle"),'Username')),
                                            placeholder = 'Enter your username...')
                            , passwordInput('admin_pass',HTML(paste(icon("lock"),'Password')),
                                            placeholder = 'Enter your password...')
                            , actionButton("sign_in", "Sign In", icon = icon("unlock-alt"))
                          ) # div
                          , tabsetPanel(id = "admin_tabs"
                            , tabPanel("Food Type"
                                    , p("List of food types and the ability to add, edit, or delete.")
                                     ) # Food tabPanel
                            , tabPanel("Ingredient Type")
                            , tabPanel("Ingredient")
                            , tabPanel("Measure")
                          )
                         ) # tabPanel admin
               , tabPanel('Source Code',icon = icon("sign-out-alt"),tags$style("float: right;"),p("To be added..."))
    ) # navBarPage
    , site_footer
  ))
# END -----------------------------------------------------------------------