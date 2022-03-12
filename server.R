# SERVER START -------------------------------------

#TO-DO
    # fix errors when buttons are selected, it's like a shadow row is selected
    # auto-select DT?
    # suggested calendar
    # add and polish basic recipes:
        # egg sandwiches, orange chicken, pot roast.

# V2
    # reactivePoll for everything to implement concurrent use
        # this will also fix failture of edits on admin page to refresh browse page.
    # continue to optimize query and reactivevalue timing
    # refine CSS
    # add food description as a tooltip over Food selection table

# V3
    # consider making edits directly to datatable instead of modals.
    # handle database errors

# Optional
    # show ingredient type icon when editing / adding ingredients and food_ingredients
    # reworkd v.food.df.r to use dbgetquery

shinyServer(function(input, output, session) {
    
    observe({
        session$onSessionEnded(function() {
            RPostgres::dbDisconnect(con) # disconnect from DB
        })})
    
    # bs_themer() # for testing shiny themes
    
    # values to track for changes and trigger actions
    rv <- reactiveValues(
        admin = 0 # for CUD when signed in.
        # , food.df = tbl(con,'food') %>% collect()
        , reval_step.df = TRUE
        , reval_ingredient.df = TRUE
        , reval_foodtype.df = TRUE
        , reval_ing_type.df = TRUE
        , reval_measure.df = TRUE
        , reval_v.food.df = TRUE
        , reval_v.food_ing.df = TRUE
    )
    
    foodtype.df <- reactive({
        rv$reval_foodtype.df
        ft.df = tbl(con,'foodtype') %>% select(-created_date,-mod_date) %>% collect()
        ft.df
    })
    # values for selectInputs
    foodtype <- reactive({
        # foodtype.df() instead of rv$foodtype.df?
        f <- foodtype.df()$id
        names(f) <- foodtype.df()$foodtype
        # f <- rv$foodtype.df$id
        # names(f) <- foodtype.df$foodtype
        f
    })
    
    measure.df <- reactive({
        rv$reval_measure.df
        m.df = tbl(con,'measure') %>% select(-created_date,-mod_date) %>% collect()
        m.df
    })
    # values for selectInputs
    measure <- reactive({
        m <- measure.df()$id
        names(m) <- measure.df()$measurement
        m
    })
    
    ingredient.df <- reactive({
        rv$reval_ingredient.df
        i.df <- tbl(con,'ingredient') %>% 
            arrange(ingredient) %>%
            select(-created_date,-mod_date) %>% 
            select(-calorie,-protein,-carb,-fat) %>% collect()
        i.df
    })
    ingredient <- reactive({
        ing <- ingredient.df()$id
        names(ing) <- ingredient.df()$ingredient
        ing
    })
    
    ing_type.df <- reactive({
        rv$reval_ing_type.df
        it.df <- tbl(con,'ingredienttype') %>% select(-created_date,-mod_date) %>% collect()
        it.df
    })
    ing_type <- reactive({
        it <- ing_type.df()$id
        names(it) <- ing_type.df()$ingredienttype
        it
    })
    
    v.food.df.r <- reactive({
        
        # might be able to optimize by doing this all at the DB in SQL
            # or dplyr, eg v.food.df = tbl(con,'v_food') %>% collect()
            # may be better to use dbGetquery...
        
        rv$reval_v.food.df # to trigger dependency
        
        # v.food.df.r = rv$v.food.df %>%
        #     filter(total_time <= input$total_time)
        v.food.df.r = tbl(con,'v_food') %>%
            select(-created_date,-mod_date) %>%
            filter(total_time <= local(input$total_time)) %>%
            collect()
        
        if(input$foodtype != "All"){
            v.food.df.r = v.food.df.r %>%
                filter(foodtype_id == input$foodtype)
        }

        if(input$meal_prep == TRUE){
            v.food.df.r = v.food.df.r %>%
                filter(meal_prep == input$meal_prep)
        }
        
        # v.food.df.r = tbl(con,'v_food') %>%
        #     filter(total_time <= local(input$total_time)
        #            , (local(input$foodtype) == "All"
        #               | foodtype_id == local(input$foodtype))
        #            , (local(input$meal_prep) == FALSE
        #               | meal_prep == local(input$meal_prep))) %>%
        #     collect()
        
        v.food.df.r
    })
    
    v.food_ing.df.r <- reactive({
        
        rv$reval_v.food_ing.df # to trigger dependency
        
        s_food <- v.food.df.r()[input$recipes_rows_selected,]

        v.food_ing.df <- dbGetQuery(con, sqlInterpolate(con,
            "SELECT * FROM v_food_ing
             WHERE food_id = ?f_id;"
            , f_id = s_food$id)) %>%
            select(-created_date,-mod_date) %>%
            mutate(ingredienttype = paste0("<i class='fa fa-",icon
                                           ,"' style='color:",color,";'></i>")
                   , amount = qty*(input$servings/s_food$serving)
                   , amount_desc = paste(amount,measurement,sep = ' - '))
                       
        v.food_ing.df             
    })
    
    step.df.r <- reactive({
        
        rv$reval_step.df # to trigger dependency
        
        s_food <- v.food.df.r()[input$recipes_rows_selected,]
        
        step.df <- dbGetQuery(con, sqlInterpolate(con,
             "SELECT * FROM step
             WHERE food_id = ?f_id
             ORDER BY instruction_order;"
             , f_id = s_food$id)) %>%
            select(-created_date,-mod_date)
        
        step.df             
    })
    
# Browse -------------------------------------------------------------------------------
    # _Filters ----------------------------
    output$servingUI <- renderUI({
        numericInput('servings','Servings',v.food.df.r()$serving[input$recipes_rows_selected]
                     ,1,20)
    }) # servingUI
    output$foodtypeUI <- renderUI({
        selectInput("foodtype", label="Food Type",
                    choices = c('All',foodtype()),
                    selected = 'All')
    })
    
    # _Food ##########################
    output$recipes <- renderDT({
        
        tdata <- v.food.df.r() %>%
            select(-foodtype_id,-serving,-steps,-foodtype) %>%
            mutate(meal_prep = ifelse(meal_prep == 1,i_checkmark, NA)) %>%
            select(-description)
            # relocate(description, .after = last_col())

        tdata_cols <- c('Food',
                        'Good for Meal Prep?',
                        'Prep + Cook Time (Minutes)')
        
        dis_but_order <- list() # disable ordering on buttons fields if they exist
        
        if(rv$admin == 1 ){ # if logged in as admin
        tdata$Delete <- init_buttons(nrow(tdata),"delete_food", icon = icon("trash"), class = "btn-danger")
        tdata$Edit <- init_buttons(nrow(tdata),"edit_food", icon = icon("pencil-alt"), class = "btn-warning")
        tdata_cols <- c(tdata_cols,'','')
        dis_but_order <- list(targets = 4:5, orderable = FALSE) # disable ordering on buttons
        }
        
        datatable(tdata, rownames = FALSE
                  , selection = list(mode = 'single',target = 'row',selected = 1)
                  , escape = c(-3)
                  # , class = 'bg-dark'
                  , colnames = tdata_cols
                  , list(searching = TRUE
                         , columnDefs = list(
                             list(visible = FALSE, targets=0)
                             , dis_but_order
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
    
    # _Steps #############################
    
    output$steps <- renderDT({
        
        validate(
            need(input$recipes_rows_selected > 0
            , "Select a food item to see cooking steps!"))
        
        tdata <- step.df.r() %>%
            select(-food_id,-actiontime) %>%
            relocate(instruction_order, .after = id)
        
        tdata_cols <- c('Step',
                        'Instruction')
        
        dis_but_order <- list() # disable ordering on buttons fields if they exist
        
        if(rv$admin == 1 ){ # if logged in as admin
            tdata$Delete <- init_buttons(nrow(tdata),"delete_step", icon = icon("trash"), class = "btn-danger")
            tdata$Edit <- init_buttons(nrow(tdata),"edit_step", icon = icon("pencil-alt"), class = "btn-warning")
            tdata_cols <- c(tdata_cols,'','')
            dis_but_order <- list(targets = 2:3, orderable = FALSE) # disable ordering on buttons
        }
        
        datatable(tdata, rownames = FALSE, selection = 'none', escape = FALSE
                  , colnames = tdata_cols
                  , extensions = 'Buttons'
                  , options = list(
                      pageLength = nrow(tdata)
                      , ordering = FALSE
                      , buttons = c('copy')
                      , dom = 'tB'
                      , columnDefs = list(list(visible=FALSE, targets=0) # hide first column
                                          , list(className = 'dt-center',targets = 1) # center second column
                                          , dis_but_order
                                          ))
        ) %>%
            formatStyle(columns = "instruction_order",
                        color = 'black',
                        fontWeight = 'bold',
                        fontSize = '150%'
                        ) 
        
    }) # steps
    
    # _Ingredients #########################
    output$ingredients <- renderDT({
        
        validate(
            need(input$recipes_rows_selected > 0
                 , "Select a food item to see ingredients!"))

        tdata <- v.food_ing.df.r() %>%
            select(ingredienttype, amount_desc, ingredient)
        
        tdata_cols <- c('Type' # icon
                        , 'Amount'
                        , 'Ingredient')
        
        if(rv$admin == 1 ){ # if logged in as admin
            tdata$Delete <- init_buttons(nrow(tdata),"delete_food_ing", icon = icon("trash"), class = "btn-danger")
            tdata$Edit <- init_buttons(nrow(tdata),"edit_food_ing", icon = icon("pencil-alt"), class = "btn-warning")
            tdata_cols <- c(tdata_cols,'','')
        }
        
        datatable(tdata, rownames = FALSE
                  , selection = 'single'
                  , escape = -0
                  , colnames = tdata_cols
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
    
    # _Measurements #####################################
    output$conversions <- renderDT({
        
        # subset for selected ingredient
        ing.r <- v.food_ing.df.r()[input$ingredients_rows_selected,]
        
        # only show conversion when a conversion exists.
        validate(
            need(ing.r$measurement %in% conversion.df$measure
                 , glue("There are no conversions available for {ing.r$measurement}")))
        
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
    observeEvent(input$measure_conv, {
        # subset for selected ingredient
        ing.r <- v.food_ing.df.r()[input$ingredients_rows_selected,]
        
        # when DT ingredients row is clicked
        showModal(modalDialog(
            title = "Measurement Conversions"
            , p("Conversions for: ",ing.r$amount_desc)
            , DTOutput('conversions')
            , footer = NULL, easyClose = TRUE
        ))
    }) # observeEvent

# Sign-In ###############################################

    # hide all admin tabs until sign in
    observe({
        if(input$sign_in == 0 | is.null(input$sign_in)){
            hideTab("admin_tabs", target = 'Food Type')
            hideTab("admin_tabs", target = 'Ingredient Type')
            hideTab("admin_tabs", target = 'Ingredient')
            hideTab("admin_tabs", target = 'Measure')
        }
    })
    
    observeEvent(input$sign_in, {
        
        updateNavbarPage(session, "tabs", selected = "Browse")
        
        can_connect <- dbCanConnect(con_config$driver,
                                    host = con_config$host,
                                    dbname = con_config$dbname,
                                    user = input$username,
                                    password = input$admin_pass)
        
        # if user failed to connect, tell them.
        if(!can_connect | input$username == "" | input$admin_pass == ""){
            updateNavbarPage(session, "tabs", selected = "Admin")
            showModal(modalDialog(
                title = div(icon('times-circle-o')," Login Failed!",style="color:red;")
                , "Check that your username and password are correct.
            If the problem continues, contact the Site Administrator."
                , style = 'background-color:lightPink;'
                , footer = NULL
                , easyClose = TRUE
            ))
        } else { # successful login
            rv$con_admin <- dbConnect(con_config$driver,
                         host = con_config$host,
                         dbname = con_config$dbname,
                         user = input$username,
                         password = input$admin_pass)
            
            rv$admin = 1
            
        # remove login section
        removeUI("#sign-in",immediate = TRUE)
    
        # insert user email in navbar to indicate logged in user
        insertUI(
            selector = "#tabs",
            where = "afterEnd",
            ui = div(id = "logged-user"
                     , column(8,strong("Logged in as Admin."))
                     , style = "color:white;float:right;padding-top:5px;white-space:nowrap;")
        ) # insertUI
        
        showTab("admin_tabs", target = 'Food Type',select = TRUE,session)
        showTab("admin_tabs", target = 'Ingredient Type')
        showTab("admin_tabs", target = 'Ingredient')
        showTab("admin_tabs", target = 'Measure')
        updateTabsetPanel(session,"admin_tabs",selected = "Measure")
        # rv$reval_foodtype.df <- !rv$reval_foodtype.df
    } # else
}) # observeEvent sign_in


# observeEvent(input$tabs,{
#     if(input$tabs == "admin") {2
#         
#     } else if(input$tabs == "out") {
#         # sign out
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

# Delete Buttons ####################################################
    # _Delete Food ----------------------------
observeEvent(input$delete_food, {
    rowNum <- get_id_from_input(input$delete_food)
    rv$food_to_delete <- v.food.df.r()$id[[rowNum]] 
    
    showModal(modalDialog(
        title = tags$b("Are you sure you want to delete this Food?",style="color:red;")
        , class = "delete"
        , p(paste("Food ID:",rv$food_to_delete)
            , paste("Food:",v.food.df.r()$food[[rowNum]]) )
        , fluidRow(column(6,actionButton("confirm_delete_food","Delete Food",icon("trash"), class = "btn-danger"))
                   , column(6,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent delete_food

# if confirmed then delete in DB
observeEvent(input$confirm_delete_food, {
    dbExecute(rv$con_admin, sqlInterpolate(rv$con_admin,
                  "DELETE FROM food WHERE id = ?food_id;"
                  , food_id = rv$food_to_delete
              )) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$reval_v.food.df <- !rv$reval_v.food.df
}) # observeEvent confirm_delete_food

# _Delete Step -----------------------------------------------------
observeEvent(input$delete_step, {
    rowNum <- get_id_from_input(input$delete_step)
    step_id.c <- step.df.r() %>% pull(id)
    rv$step_to_delete <- step_id.c[[rowNum]] 
    
    if(length(step_id.c) == 1){ # there is only one step left
        showModal(modalDialog(
            title = tags$b("You can't delete ALL the steps!",style="color:red;")
            , p("Try editing this step instead so the instructions are helpful.")
            , footer = NULL , easyClose = TRUE
        ))
    } else { # more than one step
    showModal(modalDialog(
        title = tags$b("Are you sure you want to delete this Step?",style="color:red;")
        , class = "delete"
        , "Step ID: ",rv$step_to_delete, br()
        , "Step Order: "
        , step.df.r() %>% filter(id == rv$step_to_delete) %>% pull(instruction_order)
        , fluidRow(column(6,actionButton("confirm_delete_step","Delete Step",icon("trash"), class = "btn-danger"))
                   , column(6,modalButton("Cancel",icon("times"))))
        , footer = NULL , easyClose = TRUE)) # showModal
    } # if else
}) # observeEvent delete_step

# if confirmed then delete in DB
observeEvent(input$confirm_delete_step, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,"DELETE FROM step WHERE id = ?id;"
        , id = rv$step_to_delete)) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$reval_step.df <- !rv$reval_step.df
}) # observeEvent confirm_delete_step

# _Delete Food_Ing ------------------------------------
observeEvent(input$delete_food_ing, {
    rowNum <- get_id_from_input(input$delete_food_ing)
    rv$food_ing_to_delete <- v.food_ing.df.r()[rowNum,]
    
    showModal(modalDialog(
        title = tags$b("Are you sure you want to delete this Ingredient for this Food?",style="color:red;")
        , class = "delete"
        , "Food ID: ",rv$food_ing_to_delete$food_id, br()
        , "Ingredient ID: ", rv$food_ing_to_delete$ingredient_id, br()
        , "Ingredient: "
        , rv$food_ing_to_delete$ingredient, br()
        , fluidRow(column(6,actionButton("confirm_delete_food_ing","Delete Ingredient",icon("trash"), class = "btn-danger"))
                   , column(6,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent delete_step

# if confirmed then delete in DB
observeEvent(input$confirm_delete_food_ing, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,"DELETE FROM food_ingredient 
            WHERE food_id = ?f_id AND ingredient_id = ?ing_id;"
        , f_id = rv$food_ing_to_delete$food_id
        , ing_id = rv$food_ing_to_delete$ingredient_id)) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$reval_v.food_ing.df <- !rv$reval_v.food_ing.df
}) # observeEvent confirm_delete_food_ing

# Edit Buttons --------------------------------------------------------------
# _Edit Food --------------------
observeEvent(input$edit_food, {
    rowNum <- get_id_from_input(input$edit_food)
    rv$food_to_edit <- v.food.df.r()$id[[rowNum]] 
    
    showModal(modalDialog(
        title = tags$b("Edit Food")
        , class = "edit"
        , ifelse(debug_mode,paste("Food ID: ",rv$food_to_edit),'')
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
        , selectInput('food_foodtype_edit','Type',foodtype(),v.food.df.r()$foodtype_id[[rowNum]])
        , fluidRow(column(4,actionButton("confirm_edit_food","Save Edits",icon("pencil-alt"), class = "btn-warning"))
                   , column(4,actionButton("add_food", "Add Food", icon = icon("plus"), class = "btn-success"))
                   , column(4,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent edit_food

# if confirmed then update in DB
observeEvent(input$confirm_edit_food, {
    dbExecute(rv$con_admin, sqlInterpolate(rv$con_admin,
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
    rv$reval_v.food.df <- !rv$reval_v.food.df
}) # observeEvent confirm_delete_food

# _Edit Food_ing -------------------
output$amountUI <- renderUI({ # to handle food measurments
    numericInput('amount_edit'
                 , glue('Amount ({ingredient.df() %>% 
                              filter(id == input$food_ing_edit) %>%
                              inner_join(measure.df(),by=c("measure_id"="id")) %>%
                              pull(measurement)})')
                 ,rv$food_ing_to_edit$qty,0,120,1,width = '100%')
})

observeEvent(input$edit_food_ing, {
    rowNum <- get_id_from_input(input$edit_food_ing)
    rv$food_ing_to_edit <- v.food_ing.df.r()[rowNum,]
    
    showModal(modalDialog(
        title = tags$b("Edit ",rv$food_ing_to_edit$ingredient)
        , class = "edit"
        , if(debug_mode){div(
            "Food ID: ",rv$food_ing_to_edit$food_id, br()
            , "Ingredient ID: ", rv$food_ing_to_edit$ingredient_id, br())} else {""}
        , selectInput('food_ing_edit','Ingredient',ingredient(),rv$food_ing_to_edit$ingredient_id)
        , uiOutput('amountUI')
        , checkboxInput('food_ing_opt_edit','Optional?',rv$food_ing_to_edit$optional)
        , fluidRow(column(4,actionButton("confirm_edit_food_ing","Save Edits",icon("pencil-alt"), class = "btn-warning"))
                   , column(5,actionButton("add_food_ing", "Add Ingredient", icon = icon("plus"), class = "btn-success"))
                   , column(3,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent edit_food_ing

observeEvent(input$confirm_edit_food_ing, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "UPDATE food_ingredient
        SET qty = ?QTY
                , optional = ?optional
                , ingredient_id = ?ing_edit_id
        WHERE food_id = ?f_id AND ingredient_id = ?ing_id;"
        , f_id = rv$food_ing_to_edit$food_id
        , ing_id = rv$food_ing_to_edit$ingredient_id
        , ing_edit_id = input$food_ing_edit
        , QTY = input$amount_edit
        , optional = as.character(input$food_ing_opt_edit))) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$reval_v.food_ing.df <- !rv$reval_v.food_ing.df
}) # observeEvent confirm_edit_food_ing

# _Edit Step -------------------
observeEvent(input$edit_step, {
    rowNum <- get_id_from_input(input$edit_step)
    rv$step_to_edit <- step.df.r()[rowNum,]
    
    showModal(modalDialog(
        title = tags$b("Edit Step: ",rv$step_to_edit$instruction_order)
        , class = "edit"
        , numericInput('actiontime_edit','Total Minutes to Complete This Step'
                       , rv$step_to_edit$actiontime,0,120,1,width = '100%')
        , sliderInput('step_order_edit','Order #',1,max(step.df.r()$instruction_order) + 1
                      , rv$step_to_edit$instruction_order,1)
        , textAreaInput('instruction_edit',
                        'Instructions',height = "120px",width = "100%" # because max-width is 100%!
                        , value = rv$step_to_edit$instruction
                        , placeholder = 'Clear and detailed. Can sometimes be several sentences long.')
        , fluidRow(column(4,actionButton("confirm_edit_step","Save Edits",icon("pencil-alt"), class = "btn-warning"))
                   , column(4,actionButton("add_step", "Add Step", icon = icon("plus"), class = "btn-success"))
                   , column(4,modalButton("Cancel",icon("times"))))
        , footer = NULL, easyClose = TRUE
    ))
}) # observeEvent edit_step

observeEvent(input$confirm_edit_step, { # if confirmed then update in DB
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "UPDATE step
        SET actiontime = ?at
                , instruction_order = ?inst_ord
                , instruction = ?inst
                , mod_date = now()
        WHERE id = ?s_id;"
        , s_id = rv$step_to_edit$id
        , at = input$actiontime_edit
        , inst_ord = input$step_order_edit
        , inst = input$instruction_edit)) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$reval_step.df <- !rv$reval_step.df
}) # observeEvent confirm_edit_step

# Add Buttons ######################
# _Add Food ----------------------------
observeEvent(input$add_food, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin
        , "INSERT INTO food(foodtype_id,food,description,serving,meal_prep)
            VALUES(?foodtype_id,?food,?description,?serving,?meal_prep);"
       , foodtype_id = input$food_foodtype_edit
       , food = input$food_edit
       , description = input$description_edit
       , serving = input$serving_edit
       , meal_prep = as.character(input$meal_prep_edit))
    ) # dbExecute
    
    removeModal()
    
    # refresh tables from DB
    rv$reval_v.food.df <- !rv$reval_v.food.df
    rv$reval_step.df <- !rv$reval_step.df
    rv$reval_v.food_ing.df <- !rv$reval_v.food_ing.df
}) # observeEvent add_food

# _Add Step ------------------------------------
observeEvent(input$add_step, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "INSERT INTO step(food_id,actiontime,instruction_order,instruction)
        VALUES(?food_id,?at,?inst_ord,?inst);"
        , food_id = rv$step_to_edit$food_id
        , at = input$actiontime_edit
        , inst_ord = input$step_order_edit
        , inst = input$instruction_edit)) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$reval_step.df <- !rv$reval_step.df
}) # observeEvent add_step

# _Add Food_Ing ---------------------------------
observeEvent(input$add_food_ing, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "INSERT INTO food_ingredient(food_id,ingredient_id,qty,optional)
        VALUES(?f_id,?ing_id,?QTY,?optional);"
        , f_id = rv$food_ing_to_edit$food_id
        , ing_id = input$food_ing_edit
        , QTY = input$amount_edit
        , optional = as.character(input$food_ing_opt_edit))) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$reval_v.food_ing.df <- !rv$reval_v.food_ing.df
}) # observeEvent confirm_edit_food_ing

# Admin --------------------------------------------------
# _renderDT -------------
renderAdminDT <- function(tdata,btn_suffix){
    renderDT({

        tdata <- tdata()
        tdata$Delete <- init_buttons(nrow(tdata),paste0("delete_",btn_suffix), icon = icon("trash"), class = "btn-danger")
        tdata$Edit <- init_buttons(nrow(tdata),paste0("edit_",btn_suffix), icon = icon("pencil-alt"), class = "btn-warning")
        dis_but_order <- list(targets = (ncol(tdata)-1):ncol(tdata)-1, orderable = FALSE) # disable ordering on buttons
        
        # show icons for ingredienttype
        # maay work to do this in the modal window instead...
        # escape_col <- -which(colnames(tdata) == "icon")
        # if(escape_col < 0) {
        icon_handle <- ifelse("icon" %in% names(tdata)
                              ,-which(colnames(tdata) == "icon")
                              ,FALSE)
        # if("icon" %in% names(tdata)) {
        if(icon_handle) { # everything but false will be evaluated
            tdata <- tdata %>% mutate(icon = glue("<i class='fa fa-{icon}"
                   ,"' style='color: {color};'></i>"))
        }
        
        datatable(tdata, rownames = FALSE, selection = 'none'
                  , escape = icon_handle
                  , options = list(pageLength = 25
                                   , searching = TRUE
                                   , columnDefs = list(dis_but_order)
                                   # , dom = 'tp'
                  )
        ) %>%
            formatStyle(names(tdata), verticalAlign='middle')
    }) } # renderAdminDT
output$foodtype <- renderAdminDT(reactive(foodtype.df()),'foodtype')
output$ing_type <- renderAdminDT(reactive(ing_type.df()),'ing_type')
output$ingredient <- renderAdminDT(reactive(ingredient.df()),'ingredient')
output$measure <- renderAdminDT(reactive(measure.df()),'measure')

# _Delete Buttons ------------------------

# function to reuse for all admin page.
observeDelete <- function(tdata,btn_suffix,name){
    observeEvent(input[[paste0('delete_',btn_suffix)]], {
        tdata <- tdata() # NEEDED to identify as reactive within a function!
        rowNum <- get_id_from_input(input[[paste0('delete_',btn_suffix)]])
        rv[[paste0(btn_suffix,'_to_delete')]] <- tdata[rowNum,]
        
        showModal(modalDialog(
            title = glue("Are you sure you want to delete {tdata[rowNum,][2]}")
            , class = "delete"
            , h4(paste0(name,":"),tdata[rowNum,][2]), br()
            , if(debug_mode){div(
                "Internal ID: ", tdata[rowNum,]$id, br()
                , p("reval_: ",rv[[glue('reval_{btn_suffix}.df')]]), br()
                , p("input$tabs: ",input$tabs),br()
                , p("Rows: ",nrow(tdata)),br()
                , p("Max Mod:", max(tdata$mod_date)))} else {""}
            , fluidRow(column(6,actionButton(paste0("confirm_delete_",btn_suffix)
                                             ,paste("Delete",name),icon("trash"), class = "btn-danger"))
                       , column(6,modalButton("Cancel",icon("times"))))
            , footer = NULL, easyClose = TRUE
        ))
    }) } # observeDelete

observeDelete(reactive(foodtype.df()),'foodtype','Food Type')
observeDelete(reactive(ing_type.df()),'ing_type','Ing. Type')
observeDelete(reactive(ingredient.df()),'ingredient','Ingredient')
observeDelete(reactive(measure.df()),'measure','Measure')

observeConfDelete <- function(btn_suffix,table_name = NA){
    observeEvent(input[[paste0('confirm_delete_',btn_suffix)]], {
    if(is.na(table_name)) table_name <- btn_suffix
    
    dbExecute(rv$con_admin, sqlInterpolate(rv$con_admin,
           glue("DELETE FROM {table_name} WHERE id = ?id;")
           , id = rv[[paste0(btn_suffix,'_to_delete')]]$id
    )) # dbExecute
    
    removeModal()
    
    # update datatable
    rv[[glue('reval_{btn_suffix}.df')]] <- !rv[[glue('reval_{btn_suffix}.df')]]
}) } # observeConfDelete

observeConfDelete('foodtype')
observeConfDelete('ing_type','ingredienttype')
observeConfDelete('ingredient')
observeConfDelete('measure')

# _Edit Buttons ------------------------
# function to reuse for all admin page.
observeEdit <- function(tdata,btn_suffix,name){
    observeEvent(input[[paste0('edit_',btn_suffix)]], {
        tdata <- tdata()
        rowNum <- get_id_from_input(input[[paste0('edit_',btn_suffix)]])
        rv[[paste0(btn_suffix,'_to_edit')]] <- tdata[rowNum,]
        
        if(btn_suffix == 'ingredient'){ # since ingredient is a M:M table.
            input_list <- list(
                textInput('ingredient_edit','Ingredient'
                          , value = tdata[rowNum,'ingredient'], width = "100%")
                , selectInput("measure_id_edit", label="How should it be measured?",
                              choices = measure(),width = "100%",
                              selected = tdata[rowNum,'measure_id'])
                , selectInput("substitute_id_edit", label="What can it be substituted with?",
                              choices = c('Nothing' = NA,ingredient()),width = "100%",
                              selected = tdata[rowNum,'substitute_id'])
                , selectInput("ingredienttype_id_edit", label="Ingredient Type",
                              choices = ing_type(),width = "100%",
                              selected = tdata[rowNum,'ingredienttype_id'])
            )
        } else { #all other admin tables.
        input_list <- lapply(names(tdata[,-1]), function(i) {
            if(is.numeric(tdata[[i]])){
                numericInput(glue('{i}_edit'),i,value = as.integer(tdata[rowNum,i])
                             , min = 1, step = 1)
            } else {  # not numeric
                textInput(glue('{i}_edit'),i
                          , value = tdata[rowNum,i], width = "100%")
                }
            })
        }
        
        showModal(modalDialog(
            title = glue("Edit {tdata[rowNum,2]}, or create a new {name}.")
            , class = "edit"
            , h4(paste0(name,":"),tdata[rowNum,2]), br()
            , if(debug_mode){div(
                "Internal ID: ", tdata[rowNum,]$id, br()
                , p("reval_: ",rv[[glue('reval_{btn_suffix}.df')]]), br()
                , p("input$tabs: ",input$tabs),br()
                , p("Rows: ",nrow(tdata)),br()
                , p("Max Mod:", max(tdata$mod_date)),br()
                , tdata[rowNum,])} else {""}
            , tagList(input_list)
            , fluidRow(column(4,actionButton(paste0("confirm_edit_",btn_suffix),"Save Edits",icon("pencil-alt"), class = "btn-warning"))
                       , column(4,actionButton(glue("add_{btn_suffix}"), glue("Add {name}"), icon = icon("plus"), class = "btn-success"))
                       , column(4,modalButton("Cancel",icon("times"))))
            , footer = NULL, easyClose = TRUE
        ))
    }) } # observeDelete
observeEdit(reactive(foodtype.df()),'foodtype','Food Type')
observeEdit(reactive(ing_type.df()),'ing_type','Ing. Type')
observeEdit(reactive(ingredient.df()),'ingredient','Ingredient')
observeEdit(reactive(measure.df()),'measure','Measure')

# these could be wrapped in a function, but each query must be unique
    # since table structures differ. These are short enough that it
    # is worth the transparency to write each one out entirely.
observeEvent(input$confirm_edit_foodtype, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "UPDATE foodtype
        SET foodtype = ?ft
                , mod_date = now()
        WHERE id = ?id;"
        , id = rv$foodtype_to_edit$id
        , ft = input$foodtype_edit)) # dbExecute
    
    removeModal()
    
    rv$reval_foodtype.df <- !rv$reval_foodtype.df # update datatable
}) # observeEvent confirm_edit_foodtype

observeEvent(input$add_foodtype, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "INSERT INTO foodtype(foodtype,mod_date)
        VALUES(?ft, now());"
        , ft = input$foodtype_edit)) # dbExecute
    
    removeModal()
    
    rv$reval_foodtype.df <- !rv$reval_foodtype.df # update datatable
}) # observeEvent add_foodtype

observeEvent(input$confirm_edit_ing_type, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "UPDATE ingredienttype
        SET ingredienttype = ?it
                , icon = ?icon
                , color = ?color
                , mod_date = now()
        WHERE id = ?id;"
        , id = rv$ing_type_to_edit$id
        , it = input$ingredienttype_edit
        , icon = input$icon_edit
        , color = input$color_edit)) # dbExecute
    
    removeModal()
    
    rv$reval_ing_type.df <- !rv$reval_ing_type.df # update datatable
}) # observeEvent confirm_edit_ing_type

observeEvent(input$add_ing_type, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "INSERT INTO ingredienttype(ingredienttype,icon,color,mod_date)
        VALUES(?it,?icon,?color,now());"
        , it = input$ingredienttype_edit
        , icon = input$icon_edit
        , color = input$color_edit)) # dbExecute
    
    removeModal()
    
    rv$reval_ing_type.df <- !rv$reval_ing_type.df # update datatable
}) # observeEvent add_ing_type

observeEvent(input$confirm_edit_ingredient, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "UPDATE ingredient
        SET ingredient = ?i
                , measure_id = ?m_id
                , substitute_id = ?s_id
                , ingredienttype_id = ?it_id
                , mod_date = now()
        WHERE id = ?id;"
        , id = rv$ingredient_to_edit$id
        , i = input$ingredient_edit
        , m_id = input$measure_id_edit
        , s_id = as.integer(input$substitute_id_edit)
        , it_id = input$ingredienttype_id_edit)) # dbExecute
    # eventually add calorie, protein, carb, and fat
    
    removeModal()
    
    rv$reval_ingredient.df <- !rv$reval_ingredient.df # update datatable
}) # observeEvent confirm_edit_ingredient

observeEvent(input$add_ingredient, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "INSERT INTO ingredient(ingredient,measure_id,substitute_id,ingredienttype_id,mod_date)
        VALUES(?i,?m_id,?s_id,?it_id,now());"
        , i = input$ingredient_edit
        , m_id = input$measure_id_edit
        , s_id = as.integer(input$substitute_id_edit)
        , it_id = input$ingredienttype_id_edit)) # dbExecute
    # eventually add calorie, protein, carb, and fat
    
    removeModal()
    
    rv$reval_ingredient.df <- !rv$reval_ingredient.df # update datatable
}) # observeEvent add_ingredient

observeEvent(input$confirm_edit_measure, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "UPDATE measure
        SET measurement = ?m
                , mod_date = now()
        WHERE id = ?id;"
        , id = rv$measure_to_edit$id
        , m = input$measurement_edit)) # dbExecute
    
    removeModal()
    
    rv$reval_measure.df <- !rv$reval_measure.df # update datatable
}) # observeEvent confirm_edit_measure

observeEvent(input$add_measure, {
    
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "INSERT INTO measure(measurement,mod_date)
        VALUES(?m,now());"
        , m = input$measurement_edit)) # dbExecute
    
    removeModal()
    
    rv$reval_measure.df <- !rv$reval_measure.df # update datatable
}) # observeEvent add_measure

# Sandbox -------------------

# _reactivePoll ----------
# ing_type.df <- reactivePoll(2500, session,
#                                  # This function returns the time
#                                  checkFunc = function() {
#                                      if (input$tabs == 'admin'){
#                                          q <- dbGetQuery(con, "SELECT COUNT(*) count, MAX(mod_date) max_mod  FROM ingredienttype;")
#                                          return(paste0(q$count,q$max_mod))
#                                      } else {""}
#                                  },
#                                  # This function returns data
#                                  valueFunc = function() {
#                                      # tbl(con,'ingredient') %>% collect()
#                                      q <- dbGetQuery(con, "SELECT * FROM ingredienttype;") %>% 
#                                          select(-created_date,-mod_date)
#                                      return(q)
#                                  }
# )

# Plan -------------------------------------------------
# _Calendar ---------------
output$calendar <- renderDT({
    
    food.df <- tbl(con,'food') %>%
        filter(foodtype_id == 3) %>% # only dinners
        mutate(leftovers = ifelse(serving > 3,TRUE,FALSE)) %>%
        select(-created_date,-mod_date) %>%
        collect()
    
    s_date <- as.Date(input$cal_month)
    date_df <- tibble(
        date = seq(floor_date(s_date,'month')
                   , ceiling_date(s_date,'month')-1, by=1)
        # , month = months(s_date)
        # , month_abb = months(s_date,TRUE)
        , weekday = factor(weekdays(date), levels = days_of_week)
        , week_of_year = strftime(date,format = "%U")
        , day = strftime(date,format = "%d")) %>%
        # join recipes in here, instead of spreading day, 
        # spread day_details with html including the day and the recipe.
        mutate(food = sample(food.df$food,nrow(.),TRUE)
               , day_detail = glue("{day}<br/><br/>{food}"))
    # replace predefined days e.g., egg sandwhiches on Wednesdays
    # if has leftovers, then lag one and replace
    
    tdata <- date_df %>%
        arrange(weekday) %>%
        select(-date,-food,-day) %>%
        group_by(weekday) %>%
        spread(weekday,day_detail) %>%
        ungroup() %>%
        select(-week_of_year)
               # ,-month,-month_abb)
    
    datatable(tdata
              , colnames = days_of_week
              , escape = FALSE
              , rownames = FALSE #add to dt global options
              , selection = list(target = 'cell') # eventually pre-select current day?
              , options = list(
                  dom = 't'
                  , pageLength = length(unique(date_df$week_of_year))
                  , columnDefs = list(
                      list(targets = "_all", orderable = FALSE) # disable column ording
                      , list(className = 'dt-center',targets="_all")) # center all
              )
    ) %>% # instead, color based on cooking vs leftovers.
        # formatStyle(
        #   'abb','index',
        #   target = 'row',
        #   backgroundColor = styleEqual(
        #     unique(tdata$index), rep(c('gray','white'),2)) # should always be four months
        # ) %>%
        # formatStyle('abb'
        #             ,fontWeight = 'bold'
        #             ,backgroundColor = 'black') %>%
        formatStyle(names(tdata)
                    ,fontSize = '14pt') %>%
        formatStyle(names(tdata), verticalAlign='top')
    
}) # recipes

}) # shinyServer 
# END ------------------------

