# SERVER START -------------------------------------

#TO-DO --------
# need to fix
    # food.df$ID[input$recipes_rows_selected]
        # fix selecting the appropriate food when table is filtered or sorted
        # need to create reactive dataframes like I did in FDS dashboard
        # so I can reuse dataframes affected by main filters.
# v.food.df %>%
#     filter(MealType == input$MealType
#            | input$MealType == "All") %>%
#     filter(total_time <= input$total_time)
# build
    # a measurement conversion modal dialog
        # when any quantity is hovered, see many conversions!

shinyServer(function(input, output, session) {
    
    # bs_themer() # for testing shiny themes
    
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
    
# browse -------------------------------------------------------------------------------
    
    output$recipes <- renderDT({
        
        tdata <- v.food.df.r() %>%
            select(-MealType_ID,-Serving,-MealType) %>%
            mutate(Meal_Prep = ifelse(Meal_Prep == 1,i_checkmark, NA)) %>%
            relocate(Description, .after = last_col())

        datatable(tdata, rownames = FALSE
                  , selection = list(mode = 'single',target = 'row',selected = 1)
                  , escape = FALSE
                  # , class = 'bg-dark'
                  , colnames = c('Meal',
                               'Good for Meal Prep?', # make this an Icon!
                               'Prep + Cook Time (Minutes)',
                               'Steps',
                               'Description')
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
     
    })
    
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
                  , options = list(
                      pageLength = nrow(tdata)
                      , ordering = FALSE
                      , dom = 't'
                      , columnDefs = list(list(visible=FALSE, targets=0) # hide first column
                                          , list(className = 'dt-center',targets = 1) # center second column
                                          ))
        ) %>%
            formatStyle(columns = "Number",
                        color = 'black',
                        fontWeight = 'bold',
                        fontSize = '150%'
                        ) 
        
    })
    
    output$ingredients <- renderDT({
        
        validate(
            need(input$recipes_rows_selected > 0
                 , "Select a food item to see ingredients!"))
        
        tdata <- v.food_ing.df %>%
            filter(Food_ID == v.food.df.r()$ID[input$recipes_rows_selected]) %>%
            mutate(IngredientType = paste0("<i class='fa fa-",icon
                                           ,"' style='color:",color,";'></i>")
                   , amount = paste(QTY,Measurement,sep = ' - ')) %>%
            # would need to perform any adjustments here for serving size changes?
            select(IngredientType, Ingredient, amount)
        
        datatable(tdata, rownames = FALSE, selection = 'none', escape = -0
                  , colnames = c('Type' # make this icon
                                 , 'Ingredient' 
                                 , 'Amount')
                  , options = list(
                      pageLength = nrow(tdata)
                      , initComplete = NA
                      , ordering=FALSE
                      , dom = 't')
        ) %>%
            formatStyle(columns = c("IngredientType", "Ingredient"),
                        color = 'black',
                        fontWeight = 'bold',
                        fontSize = '150%'
            )
        
    })
    
})
# END ------------------------

