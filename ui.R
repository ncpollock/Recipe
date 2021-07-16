# START --------------------------------------------------
shinyUI(
  tagList(
    # tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css")
    navbarPage(id = "tabs"
               # theme = shinytheme("cosmo"),
               # , theme = bs_theme() 
               , theme = bs_theme(bootswatch = "cosmo", bg = "white", fg = "#131D2D")
               # title = p(strong("REM"),style=paste0("color:",c_1)),
               , title = "Recipes"
               , windowTitle = "Recipes",
               # , footer = div(id="site-footer")
               tabPanel(
                 'Browse', icon = icon("user")
                 , sidebarLayout(
                   sidebarPanel(
                     textInput('meal_search',
                               'Search for a meal',
                               # value = 'chicken', # predefined value for testing
                               placeholder = 'chicken')
                     , box(width = 12,collapsible = FALSE, collapsed = FALSE,
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
                   , mainPanel(splitLayout(div(p("More.",br()
                                                 ,"Stuff.",br()
                                                 ,"Here.",br(),style="font-size: 200%; font-wieght: bold;")
                                               ,p("Here. Too.")
                                               ,align = "left")
                                           ,p(icon("connectdevelop",class = "fa-10x"))))
                 )),
               
               tabPanel(
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
                 'About/Help', icon = icon("question-circle"),
                 fluidPage(
                     h1("What is this?")
                     , p("This web application helps users choose and create simple meals.")
                     , h1("Resources")
                     , tags$ul(
                       tags$li(tags$b("Research:")," ")
                       , tags$li(tags$b("More Detailed:")," ")
                       , tags$li(tags$b("Advanced Topics:")," ")
                     )
                   ))
               , tabPanel('Source Code',icon = icon("sign-out"),tags$style("float: right;"))
    )
  ))
# END -----------------------------------------------------------------------