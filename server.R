# SERVER START -------------------------------------

#TO-DO
    # add food description as a tooltip over Food selection table

shinyServer(function(input, output, session) {
    
    observe({
        session$onSessionEnded(function() {
            RPostgres::dbDisconnect(con) # disconnect from DB
        })})
    
    # bs_themer() # for testing shiny themes
    
    # values to track for changes and trigger actions
    rv <- reactiveValues()
    
    v.food.df.r <- reactive({
        
        if(input$foodtype != "All"){
            v.food.df <- v.food.df %>%
                filter(foodtype == input$foodtype)
        }
        
        if(input$meal_prep == TRUE){
            v.food.df <- v.food.df %>%
                filter(meal_prep == 1)
        }
        
        v.food.df <- v.food.df %>% 
            filter(total_time <= input$total_time)
        
        v.food.df
    })
    
    v.food_ing.df.r <- reactive({
        s_food <- v.food.df.r()[input$recipes_rows_selected,]
        
        v.food_ing.df <- v.food_ing.df %>%
            filter(food_id == s_food$id) %>%
            mutate(ingredienttype = paste0("<i class='fa fa-",icon
                                           ,"' style='color:",color,";'></i>")
                   , amount = qty*(input$servings/s_food$serving)
                   , amount_desc = gsub( fixed = T, ' - NA',''
                                        , paste(amount,measurement,sep = ' - '))) 
        
        v.food_ing.df
    })
    
# browse -------------------------------------------------------------------------------
    
    output$recipes <- renderDT({
        
        tdata <- v.food.df.r() %>%
            select(-foodtype_id,-serving,-foodtype,-created_date) %>%
            mutate(meal_prep = ifelse(meal_prep == 1,i_checkmark, NA)) %>%
            select(-description)
            # relocate(description, .after = last_col())

        tdata_cols <- c('Food',
                        'Good for Meal Prep?', # make this an Icon!
                        'Prep + Cook Time (Minutes)',
                        'Steps')
        
        # eventually wrap in if logged in
        if(1 == 1 ){ # logged in
        tdata$Delete <- init_buttons(nrow(tdata),"delete_food", icon = icon("trash"), class = "delete_btn")
        tdata_cols <- c(tdata_cols,'')
        }
        
        datatable(tdata, rownames = FALSE
                  , selection = list(mode = 'single',target = 'row',selected = 1)
                  , escape = FALSE
                  # , class = 'bg-dark'
                  , colnames = tdata_cols
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
            formatStyle(columns = "food",
                        color = 'black',
                        fontWeight = 'bold') %>%
            formatStyle(names(tdata), verticalAlign='middle')
     
    }) # recipes
    
    
    output$servingUI <- renderUI({
        numericInput('servings','Servings',v.food.df.r()$serving[input$recipes_rows_selected]
                     ,1,20)
    }) # servingUI
    
    output$steps <- renderDT({
        
        validate(
            need(input$recipes_rows_selected > 0
            , "Select a food item to see cooking steps!"))
        
        tdata <- step.df %>%
            filter(food_id == v.food.df.r()$id[input$recipes_rows_selected]) %>%
            select(-food_id,-actiontime) %>%
            relocate(instruction_order, .after = id) %>%
            arrange(instruction_order)
        
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
            formatStyle(columns = "instruction_order",
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
            select(ingredienttype, amount_desc, ingredient)
        
        datatable(tdata, rownames = FALSE
                  , selection = 'single'
                  , escape = -0
                  , colnames = c('Type' # icon
                                 , 'Amount'
                                 , 'Ingredient')
                  , options = list(
                      pageLength = nrow(tdata)
                      # , initComplete = NA to remove header stylings
                      , ordering=FALSE
                      , dom = 't')
        ) %>%
            formatStyle(columns = c("ingredienttype", "ingredient"),
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
            need(ing.r$measurement %in% conversion.df$measure
                 , "There are no conversions available for this measurement."))

        
        # generate conversion table
        tdata <- m.conv(ing.r$amount
                        , ing.r$measurement)
        
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
            title = "measurement Conversions",
            DTOutput('conversions'),
            easyClose = TRUE
        ))
    }) # observeEvent

# Sign-In ###############################################

    # hide all admin tabs until sign in
    observe({
        if(input$sign_in == 0 | is.null(input$sign_in)){
            hideTab("admin_tabs", target = 'Food')
            hideTab("admin_tabs", target = 'Ingredients')
            hideTab("admin_tabs", target = 'Steps')
            
            # should put insertUI here to add bland user icon, then observe sign in to swap with user image!
        }
    })
    
observeEvent(input$sign_in, {
    if(input$admin_pass == "testing"){
    
    rv$user <= 'Tester'
        
    # remove login section
    removeUI("#sign-in",immediate = TRUE)
    
    # insert user email in navbar to indicate logged in user
    insertUI(
        selector = "#tabs",
        where = "afterEnd",
        ui = div(id = "logged-user"
                 , column(8,strong("Logged in as:"),br()
                          , rv$user)
                 , style = "color:white;float:right;padding-top:5px;white-space:nowrap;")
    )
    
    showTab("admin_tabs", target = 'Food')
    showTab("admin_tabs", target = 'Ingredients')
    showTab("admin_tabs", target = 'Steps')
    
    } else { 
        showModal(modalDialog(
            title = "Invalid Password!"
            , icon("exclamation-triangle")
            , width = "100%"
            , easyClose = TRUE
        )) # showModal
    } # else

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

# Admin Actions #########################################################

observeEvent(input$delete_food, {
    rowNum <- get_id_from_input(input$delete_food)
    rv$food_to_delete <- rv$active_games$id[[rowNum]] # START HERE!
    
    showModal(modalDialog(
        title = tags$b("Are you sure you want to delete this Food?",style="color:red;")
        , paste("Food ID:",rv$food_to_delete)
        , fluidRow(column(6,actionButton("confirm_delete_food","Delete Food",icon("trash"), class = "delete_btn"))
                   , column(6,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent delete_game

# if confirmed then delete in DB
observeEvent(input$confirm_delete_food, {
    dbExecute(rv$con
              , glue_sql(
                  "DELETE FROM food
                WHERE id = {food_id*}"
                  , food_id = rv$food_to_delete
                  , .con = rv$con
              )
    )
    
    removeModal()
    
    # update datatable
    rv$active_games <- tbl(rv$con, 'active_games') %>% collect()
    
}) # observeEvent confirm_delete_game


}) # shinyServer 
# END ------------------------

