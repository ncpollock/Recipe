# SERVER START -------------------------------------

#TO-DO
    # add food description as a tooltip over Food selection table
    # refine CSS
    # build reactive structure for input lists
    # prevent sign-in from being clicked multiple times.
    # fix errors when buttons are selected, it's like a shadow row is selected
    # auto-select DT

# Optional
    # show ing type icon when editing / adding ingredients.

shinyServer(function(input, output, session) {
    
    observe({
        session$onSessionEnded(function() {
            RPostgres::dbDisconnect(con) # disconnect from DB
        })})
    
    # bs_themer() # for testing shiny themes
    
    # values to track for changes and trigger actions
    rv <- reactiveValues(
        admin = 0 # for CUD when signed in.
        , food.df = tbl(con,'food') %>% collect()
        , step.df = tbl(con,'step') %>% collect() # should separate this and limit to just the step for the selected food
        , ingredient.df = tbl(con,'ingredient') %>% collect()
        # , food_ing.df = tbl(con,'food_ingredient') %>% collect()
        , foodtype.df = tbl(con,'foodtype') %>% collect()
        # , ingredienttype.df = tbl(con,'ingredienttype') %>% collect()
        , measure.df = tbl(con,'measure') %>% collect()
        , v.food.df = tbl(con,'v_food') %>% collect()
        , v.food_ing.df = tbl(con,'v_food_ing') %>% collect()
    )
    
    # values for selectInputs
    foodtype <- reactive({
        f <- rv$foodtype.df$id
        names(f) <- foodtype.df$foodtype
        f
    })
    ingredient <- reactive({
        ing <- rv$ingredient.df$id
        names(ing) <- rv$ingredient.df$ingredient
        ing
    })
    observe({
        # select_measure <- rv$measure.df$id
        # names(select_measure) <- rv$measure.df$measurement
    })
    
    v.food.df.r <- reactive({
        
        # might be able to optimize by doing this all at the DB in SQL
            # or dplyr, eg v.food.df = tbl(con,'v_food') %>% collect()
        
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
        
        v.food_ing.df <- rv$v.food_ing.df %>%
            filter(food_id == s_food$id) %>%
            mutate(ingredienttype = paste0("<i class='fa fa-",icon
                                           ,"' style='color:",color,";'></i>")
                   , amount = qty*(input$servings/s_food$serving)
                   , measurement = replace_na(measurement,'Whole')
                   , amount_desc = paste(amount,measurement,sep = ' - '))
                   # , amount_desc = gsub( fixed = T, ' - NA',''
                   #                      , paste(amount,measurement,sep = ' - '))) 
        
        v.food_ing.df
    })
    
# Browse -------------------------------------------------------------------------------
    # _Food ##########################
    output$recipes <- renderDT({
        
        tdata <- v.food.df.r() %>%
            select(-foodtype_id,-serving,-steps,-foodtype,-created_date) %>%
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
    
    
    output$servingUI <- renderUI({
        numericInput('servings','Servings',v.food.df.r()$serving[input$recipes_rows_selected]
                     ,1,20)
    }) # servingUI
    
    # _Steps #############################
    
    output$steps <- renderDT({
        
        # input$confirm_edit_step # to trigger table refresh
        
        validate(
            need(input$recipes_rows_selected > 0
            , "Select a food item to see cooking steps!"))
        
        tdata <- rv$step.df %>%
            filter(food_id == v.food.df.r()$id[input$recipes_rows_selected]) %>%
            select(-food_id,-actiontime) %>%
            relocate(instruction_order, .after = id) %>%
            arrange(instruction_order)
        
        tdata_cols <- c('Order',
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
        
        # insert add new buttons on main page
        # still need these because someone could delete 
            # all the food_ingredients or all the steps for a food
            # alternatively, I could set a trigger to add when food is added
            # plus a check constraint to prevent the last one being deleted.
        # insertUI(
        #     selector = "#recipes",
        #     where = "afterEnd",
        #     ui = actionButton("add_food", "Add Food", icon = icon("plus"), class = "btn-success")
        # ) # insertUI
        # insertUI(
        #     selector = "#ingredients",
        #     where = "afterEnd",
        #     ui = actionButton("add_food_ingredients", "Add Ingredient", icon = icon("plus"), class = "btn-success")
        # ) # insertUI
        # insertUI(
        #     selector = "#steps",
        #     where = "afterEnd",
        #     ui = actionButton("add_steps", "Add Step", icon = icon("plus"), class = "btn-success")
        # ) # insertUI
        
        showTab("admin_tabs", target = 'Food Type')
        showTab("admin_tabs", target = 'Ingredient Type')
        showTab("admin_tabs", target = 'Ingredient')
        showTab("admin_tabs", target = 'Measure')
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
    rv$v.food.df <- tbl(con,'v_food') %>% collect()
}) # observeEvent confirm_delete_food

# _Delete Step -----------------------------------------------------
observeEvent(input$delete_step, {
    rowNum <- get_id_from_input(input$delete_step)
    rv$step_to_delete <- (rv$step.df %>%
        filter(food_id == v.food.df.r()$id[input$recipes_rows_selected]) %>%
        arrange(instruction_order) %>%
        pull(id) )[[rowNum]] 
    
    showModal(modalDialog(
        title = tags$b("Are you sure you want to delete this Step?",style="color:red;")
        , class = "delete"
        , "Step ID: ",rv$step_to_delete, br()
        , "Step Order: "
        , rv$step.df %>% filter(id == rv$step_to_delete) %>% pull(instruction_order)
        , fluidRow(column(6,actionButton("confirm_delete_step","Delete Step",icon("trash"), class = "btn-danger"))
                   , column(6,modalButton("Cancel",icon("times"))))
        , footer = NULL
        , easyClose = TRUE
    ))
}) # observeEvent delete_step

# if confirmed then delete in DB
observeEvent(input$confirm_delete_step, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,"DELETE FROM step WHERE id = ?id;"
        , id = rv$step_to_delete)) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$step.df <- tbl(con,'step') %>% collect()
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
    rv$v.food_ing.df <- tbl(con,'v_food_ing') %>% collect()
}) # observeEvent confirm_delete_food_ing

# Edit Buttons --------------------------------------------------------------
# _Edit Food --------------------
observeEvent(input$edit_food, {
    rowNum <- get_id_from_input(input$edit_food)
    rv$food_to_edit <- v.food.df.r()$id[[rowNum]] 
    
    # select_foodtype <- rv$foodtype.df$id
    # names(select_foodtype) <- rv$foodtype.df$foodtype
    
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
        , selectInput('food_foodtype_edit','Type',foodtype()
                      # ,select_foodtype
                      ,v.food.df.r()$foodtype_id[[rowNum]])
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
    rv$v.food.df <- tbl(con,'v_food') %>% collect()
}) # observeEvent confirm_delete_food

# _Edit Food_ing -------------------
output$amountUI <- renderUI({ # to handle food measurments
    numericInput('amount_edit'
                 , glue('Amount ({rv$ingredient.df %>% 
                              filter(id == input$food_ing_edit) %>%
                              inner_join(rv$measure.df,by=c("measure_id"="id")) %>%
                              pull(measurement)})')
                 ,rv$food_ing_to_edit$qty,0,120,1,width = '100%')
})

observeEvent(input$edit_food_ing, {
    rowNum <- get_id_from_input(input$edit_food_ing)
    rv$food_ing_to_edit <- v.food_ing.df.r()[rowNum,]
    
    showModal(modalDialog(
        title = tags$b("Edit ",rv$food_ing_to_edit$ingredient)
        , class = "edit"
        , "Food ID: ",rv$food_ing_to_edit$food_id, br()
        , "Ingredient ID: ", rv$food_ing_to_edit$ingredient_id, br()
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
    rv$v.food_ing.df <- tbl(con,'v_food_ing') %>% collect()
}) # observeEvent confirm_edit_food_ing

# _Edit Step -------------------
observeEvent(input$edit_step, {
    rowNum <- get_id_from_input(input$edit_step)
    # this should be a reactive value instead to avoid multiple calculations.
    steps_for_food.df <- rv$step.df %>%
        filter(food_id == v.food.df.r()$id[input$recipes_rows_selected]) %>%
        arrange(instruction_order)
    rv$step_to_edit <- (steps_for_food.df)[rowNum,]
    
    showModal(modalDialog(
        title = tags$b("Edit Step: ",rv$step_to_edit$instruction_order)
        , class = "edit"
        , numericInput('actiontime_edit','Total Minutes to Complete This Step'
                       , rv$step_to_edit$actiontime,0,120,1,width = '100%')
        , sliderInput('step_order_edit','Order #',1,max(steps_for_food.df$instruction_order) + 1
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
        WHERE id = ?s_id;"
        , s_id = rv$step_to_edit$id
        , at = input$actiontime_edit
        , inst_ord = input$step_order_edit
        , inst = input$instruction_edit)) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$step.df <- tbl(con,'step') %>% collect()
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
    
    # update datatable
    rv$v.food.df <- tbl(con,'v_food') %>% collect()
}) # observeEvent add_food

# _Add Step ------------------------------------
observeEvent(input$add_step, {
    dbExecute(rv$con_admin, sqlInterpolate(
        rv$con_admin,
        "INSERT INTO step(actiontime,instruction_order,instruction)
        VALUES(?at,?inst_ord,?inst);"
        , at = input$actiontime_edit
        , inst_ord = input$step_order_edit
        , inst = input$instruction_edit)) # dbExecute
    
    removeModal()
    
    # update datatable
    rv$step.df <- tbl(con,'step') %>% collect()
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
    rv$v.food_ing.df <- tbl(con,'v_food_ing') %>% collect()
}) # observeEvent confirm_edit_food_ing

}) # shinyServer 
# END ------------------------

