# SERVER START -------------------------------------

#TO-DO
    # add food description as a tooltip over Food selection table
    # fix button colors
    # fix delete SQL

shinyServer(function(input, output, session) {
    
    observe({
        session$onSessionEnded(function() {
            RPostgres::dbDisconnect(con) # disconnect from DB
        })})
    
    # bs_themer() # for testing shiny themes
    
    # values to track for changes and trigger actions
    rv <- reactiveValues(
        food.df = tbl(con,'food') %>% collect()
        , step.df = tbl(con,'step') %>% collect()
        # , ingredient.df = tbl(con,'ingredient') %>% collect()
        # , food_ing.df = tbl(con,'food_ingredient') %>% collect()
        , foodtype.df = tbl(con,'foodtype') %>% collect()
        # , ingredienttype.df = tbl(con,'ingredienttype') %>% collect()
        # , measure.df = tbl(con,'measure') %>% collect()
        , v.food.df = tbl(con,'v_food') %>% collect()
        , v.food_ing.df = tbl(con,'v_food_ing') %>% collect()
    )
    
    v.food.df.r <- reactive({
        
        v.food.df.r = rv$v.food.df %>%
            filter(total_time <= input$total_time) 
        
        if(input$foodtype != "All"){
            v.food.df.r = v.food.df.r %>%
                filter(foodtype_id == input$foodtype)
        }
        
        if(input$meal_prep == TRUE){ 
            v.food.df.r = v.food.df.r %>%
                filter(meal_prep == input$meal_prep)
        }

        v.food.df.r
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
    # Food ##########################
    output$recipes <- renderDT({
        
        tdata <- v.food.df.r() %>%
            select(-foodtype_id,-serving,-steps,-foodtype,-created_date) %>%
            mutate(meal_prep = ifelse(meal_prep == 1,i_checkmark, NA)) %>%
            select(-description)
            # relocate(description, .after = last_col())

        tdata_cols <- c('Food',
                        'Good for Meal Prep?',
                        'Prep + Cook Time (Minutes)')
        
        # eventually wrap in if logged in
        if(1 == 1 ){ # logged in
        tdata$Delete <- init_buttons(nrow(tdata),"delete_food", icon = icon("trash"), class = "btn-danger")
        tdata$Edit <- init_buttons(nrow(tdata),"edit_food", icon = icon("pencil-alt"), class = "btn-warning")
        tdata_cols <- c(tdata_cols,'','')
        }
        
        datatable(tdata, rownames = FALSE
                  , selection = list(mode = 'single',target = 'row',selected = 1)
                  , escape = c(-3)
                  # , class = 'bg-dark'
                  , colnames = tdata_cols
                  , list(searching = TRUE
                         , columnDefs = list(
                             list(visible = FALSE, targets=0)
                             , list(targets = 4:5, orderable = FALSE)
                             , list(className = 'dt-center',targets = 3:ncol(tdata)-1)
                             ))
                  ) %>%
                formatStyle(
                    'total_time',
                    background = styleColorBar(range(0,max(tdata$total_time)), databar_color,angle = 270),
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
        
        tdata <- rv$step.df %>%
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
    
    # Ingredients #########################
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
    rv$food_to_delete <- v.food.df.r()$id[[rowNum]] 
    
    showModal(modalDialog(
        title = tags$b("Are you sure you want to delete this Food?",style="color:red;")
        , p(paste("Food ID:",rv$food_to_delete)
            , paste("Food:",v.food.df.r()$food[[rowNum]]) )
        , fluidRow(column(6,actionButton("confirm_delete_food","Delete Food",icon("trash"), class = "delete_btn"))
                   , column(6,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent delete_food

# if confirmed then delete in DB
observeEvent(input$confirm_delete_food, {
    dbExecute(con, sqlInterpolate(con,
                  "DELETE FROM food WHERE id = ?food_id;"
                  , food_id = rv$food_to_delete
              )) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$v.food.df <- tbl(con,'v_food') %>% collect()
    
}) # observeEvent confirm_delete_food

observeEvent(input$edit_food, {
    rowNum <- get_id_from_input(input$edit_food)
    rv$food_to_edit <- v.food.df.r()$id[[rowNum]] 
    
    select_foodtype <- rv$foodtype.df$id
    names(select_foodtype) <- rv$foodtype.df$foodtype
    
    showModal(modalDialog(
        title = tags$b("Edit Food")
        , paste("Food ID:",rv$food_to_edit)
        , textInput('food_edit',
                  'Food'
                  , value = v.food.df.r()$food[[rowNum]]
                  , placeholder = 'A basic name for the food.'
                  , width = "100%")
        , textAreaInput('description_edit',
                      'Description',height = "70px",width = "100%" # because max-width is 100%!
                      , value = v.food.df.r()$description[[rowNum]]
                      , placeholder = 'A brief but informative description...')
        , numericInput('serving_edit','Servings',4,0,40,1,width = '100%')
        , checkboxInput('meal_prep_edit','Good for Meal Prep?',v.food.df.r()$meal_prep[[rowNum]])
        , selectInput('food_foodtype_edit','Type',select_foodtype,v.food.df.r()$foodtype_id[[rowNum]])
        , fluidRow(column(6,actionButton("confirm_edit_food","Save Edits",icon("pencil-alt"), class = "btn-success"))
                   , column(6,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent delete_game

# if confirmed then delete in DB
observeEvent(input$confirm_edit_food, {
    dbExecute(con, sqlInterpolate(con,
               "UPDATE food
                SET foodtype_id = ?foodtype_id
                    , food = ?food
                    , description = ?description
                    , serving = ?serving
                    , meal_prep = ?meal_prep
                WHERE id = ?food_id;"
                , food_id = rv$food_to_edit
                , foodtype_id = input$food_foodtype_edit
                , food = input$food_edit
                , description = input$description_edit
                , serving = input$serving_edit
                , meal_prep = as.character(input$meal_prep_edit))
    ) # dbExecute

    removeModal()
    
    # update datatable
    rv$v.food.df <- tbl(con,'v_food') %>% collect()
    
}) # observeEvent confirm_delete_game

}) # shinyServer 
# END ------------------------

