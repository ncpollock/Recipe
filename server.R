# SERVER START -------------------------------------

#TO-DO
# need to fix
# build
    # tooltip for meal description instead of column on table.

shinyServer(function(input, output, session) {
    
    observe({
        session$onSessionEnded(function() {
           gs4_deauth() # sign out of googlesheets4?
        })})
    
    # bs_themer() # for testing shiny themes
    
    # values to track for changes and trigger actions
    rv <- reactiveValues()
    
    v.food.df.r <- reactive({
        
        if(input$MealType != "All"){
            v.food.df <- v.food.df %>%
                filter(MealType == input$MealType)
        }
        
        if(input$meal_prep == TRUE){
            v.food.df <- v.food.df %>%
                filter(Meal_Prep == 1)
        }
        
        v.food.df <- v.food.df %>% 
            filter(total_time <= input$total_time)
        
        v.food.df
    })
    
    v.food_ing.df.r <- reactive({
        s_food <- v.food.df.r()[input$recipes_rows_selected,]
        
        v.food_ing.df <- v.food_ing.df %>%
            filter(Food_ID == s_food$ID) %>%
            mutate(IngredientType = paste0("<i class='fa fa-",icon
                                           ,"' style='color:",color,";'></i>")
                   , amount = QTY*QTY_Multiplier*(input$servings/s_food$Serving)
                   , amount_desc = paste(amount,Measurement,sep = ' - ')) 
        
        v.food_ing.df
    })
    
# browse -------------------------------------------------------------------------------
    
    output$recipes <- renderDT({
        
        tdata <- v.food.df.r() %>%
            select(-MealType_ID,-Serving,-MealType) %>%
            mutate(Meal_Prep = ifelse(Meal_Prep == 1,i_checkmark, NA)) %>%
            select(-Description)
            # relocate(Description, .after = last_col())

        datatable(tdata, rownames = FALSE
                  , selection = list(mode = 'single',target = 'row',selected = 1)
                  , escape = FALSE
                  # , class = 'bg-dark'
                  , colnames = c('Food',
                               'Good for Meal Prep?', # make this an Icon!
                               'Prep + Cook Time (Minutes)',
                               'Steps'
                               # , 'Description'
                               )
                  , list(searching = TRUE
                         , columnDefs = list(list(visible=FALSE, targets=0)))
                  ) %>%
                formatStyle(
                    'total_time',
                    background = styleColorBar(range(0,max(tdata$total_time)), 'primary'),
                    # align = 'bottom',
                    backgroundColor = NA,
                    backgroundSize = '100% 85%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'left') %>%
            formatStyle(columns = "Food",
                        color = 'black',
                        fontWeight = 'bold') %>%
            formatStyle(names(tdata), verticalAlign='middle')
     
    }) # recipes
    
    
    output$servingUI <- renderUI({
        numericInput('servings','Servings',v.food.df.r()$Serving[input$recipes_rows_selected]
                     ,1,20)
    }) # servingUI
    
    output$steps <- renderDT({
        
        validate(
            need(input$recipes_rows_selected > 0
            , "Select a food item to see cooking steps!"))
        
        tdata <- step.df %>%
            filter(Food_ID == v.food.df.r()$ID[input$recipes_rows_selected]) %>%
            select(-Food_ID,-Time) %>%
            relocate(Number, .after = ID) %>%
            arrange(Number)
        
        datatable(tdata, rownames = FALSE, selection = 'none', escape = FALSE
                  , colnames = c('Number',
                                 # 'Time',
                                 'Instruction')
                  , extensions = 'Buttons'
                  , options = list(
                      pageLength = nrow(tdata)
                      , ordering = FALSE
                      , buttons = c('copy')
                      , dom = 'tB'
                      , columnDefs = list(list(visible=FALSE, targets=0) # hide first column
                                          , list(className = 'dt-center',targets = 1) # center second column
                                          ))
        ) %>%
            formatStyle(columns = "Number",
                        color = 'black',
                        fontWeight = 'bold',
                        fontSize = '150%'
                        ) 
        
    }) # steps
    
    output$ingredients <- renderDT({
        
        validate(
            need(input$recipes_rows_selected > 0
                 , "Select a food item to see ingredients!"))

        tdata <- v.food_ing.df.r() %>%
            select(IngredientType, Ingredient, amount_desc)
        
        datatable(tdata, rownames = FALSE
                  , selection = 'single'
                  , escape = -0
                  , colnames = c('Type' # make this icon
                                 , 'Ingredient' 
                                 , 'Amount')
                  , options = list(
                      pageLength = nrow(tdata)
                      # , initComplete = NA to remove header stylings
                      , ordering=FALSE
                      , dom = 't')
        ) %>%
            formatStyle(columns = c("IngredientType", "Ingredient"),
                        color = 'black',
                        fontWeight = 'bold',
                        fontSize = '150%'
            )
        
    }) # ingredients
    
    output$conversions <- renderDT({
        
        # subset for selected ingredient
        ing.r <- v.food_ing.df.r()[input$ingredients_rows_selected,]
        
        # only show conversion when a conversion exists.
        validate(
            need(ing.r$Measurement %in% conversion.df$measure
                 , "There are no conversions available for this measurement."))

        
        # generate conversion table
        tdata <- m.conv(ing.r$amount
                        , ing.r$Measurement)
        
        datatable(tdata, rownames = FALSE
                  , selection = 'none'
                  # , colnames = c('Type'
                  #                , 'Amount')
                  , options = list(
                      pageLength = nrow(tdata)
                      # , initComplete = NA to remove header stylings
                      , ordering=FALSE
                      , dom = 't'))
    })
    
    # show measurement conversions
    observeEvent(input$ingredients_rows_selected, {
        # when DT ingredients row is clicked
        showModal(modalDialog(
            title = "Measurement Conversions",
            DTOutput('conversions'),
            easyClose = TRUE
        ))
    }) # observeEvent

# Sign-In ###############################################
# Sign-Out tab ######################################################################################

    # hide all admin tabs until sign in
    observe({
        if(input$sign_in == 0 | is.null(input$sign_in)){
            hideTab(inputId = "admin_tabs", target = 'Food')
            hideTab(inputId = "admin_tabs", target = 'Ingredients')
            hideTab(inputId = "admin_tabs", target = 'Steps')
            
            # should put insertUI here to add bland user icon, then observe sign in to swap with user image!
        } else {
            showModal(modalDialog(
                title = "Did you succesfully sign in with Google?"
                # img(src=paste0(input$username, ".jpg"),class="user-img"),
                , p("Logged in as: ",gs4_user())
                , width = "100%"
                , easyClose = FALSE
                , footer = div(
                    actionButton('sign_in_success','Yes, it worked!')
                    , actionButton('sign_in_fail',icon = icon("exclamation-triangle")
                                   ,'No, I am not signed in',class = "btn-warning"))
            )) # showModal
        }
    })
    
observeEvent(input$sign_in, {
    gs4_deauth()
    gs4_auth(email = input$gmail)
})    

observeEvent(input$sign_in_success, {
    rv$user <- gs4_user()
    
    # remove login section
    removeUI("#sign-in",immediate = TRUE)
    
    # insert user email in navbar to indicate logged in user
    insertUI(
        selector = "#tabs",
        where = "afterEnd",
        ui = div(id = "logged-user"
                 , column(8,strong("Logged in as: "),br()
                          , rv$user,style = "padding-top:2px;")
                 , style = "color:white;float:right;padding-top:5px;white-space:nowrap;")
    )
    
    # close the modal window
    removeModal()
}) 

observeEvent(input$sign_in_fail, {
    # close the modal window
    removeModal()
}) 

# when user changes
observeEvent(rv$user, {
    # if authenticated as Steph or I
    if(gs4_user() %in% c('ncpolloc@gmail.com','slfagan1103@gmail.com')){
        showTab(inputId = "admin_tabs", target = 'Food')
        showTab(inputId = "admin_tabs", target = 'Ingredients')
        showTab(inputId = "admin_tabs", target = 'Steps')
    }
}) 



# observeEvent(input$tabs,{
#     if(input$tabs == "admin") {2
#         # Authenticate into Googlesheets for edit privileges
#         gs4_deauth()
#         
#     } else if(input$tabs == "out") {
#         # sign out
#         gs4_deauth()
#         
#         # tell user they've logged out.
#         showModal(modalDialog(
#             title = div(icon("check-circle-o"),style = "color: green;"," You have signed out!"),
#             style = 'background-color:lightGreen;',
#             footer = NULL,
#             easyClose = TRUE
#         ))
#     }
#     }) # observer tabs


}) # shinyServer 
# END ------------------------

