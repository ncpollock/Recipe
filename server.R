# SERVER START -------------------------------------

#TO-DO --------
# need to fix
    # food.df$ID[input$recipes_rows_selected]
        # fix selecting the appropriate food when table is filtered or sorted

shinyServer(function(input, output, session) {
    
    # bs_themer() # for testing shiny themes
    
    
    output$recipes <- renderDT({
        
        tdata <- v.food.df %>%
            filter(MealType == input$MealType
                   | input$MealType == "All") %>%
            filter(total_time <= input$total_time) %>%
            # filter(Meal_Prep == input$meal_prep) %>%
            select(-MealType_ID,-Serving,-MealType) %>%
            # mutate Meal_Prep as an icon
            relocate(Description, .after = last_col())

        datatable(tdata, rownames = FALSE, selection = 'single', escape = FALSE
                  , colnames = c('Meal',
                               'Good for Meal Prep?', # max this an Icon!
                               'Prep + Cook Time (Minutes)',
                               'Steps',
                               'Description')
                  , list(searching = TRUE
                         , columnDefs = list(list(visible=FALSE, targets=0)))
                  ) %>%
                formatStyle(
                    'total_time',
                    background = styleColorBar(range(0,max(tdata$total_time)), 'blue'),
                    align = 'bottom',
                    backgroundSize = '100% 85%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'left') %>%
            formatStyle(columns = "Food",
                        color = 'black',
                        fontWeight = 'bold') %>%
            formatStyle(names(tdata), 'vertical-align'='top')
     
    })
    
    output$steps <- renderDT({
        
        validate(
            need(food.df$ID[input$recipes_rows_selected] > 0
            , "Select a food item to see cooking steps!"))
        
        tdata <- step.df %>%
            filter(Food_ID == food.df$ID[input$recipes_rows_selected]) %>%
            select(-Food_ID) %>%
            relocate(Number, .after = ID) %>%
            arrange(Number)
        
        datatable(tdata, rownames = FALSE, selection = 'none', escape = FALSE
                  , colnames = c('Number',
                                 'Time', # max this an Icon!
                                 'Instruction')
                  , options = list(
                      pageLength = nrow(tdata)
                      , ordering=FALSE
                      , dom = 't'
                      , columnDefs = list(list(visible=FALSE, targets=0)))
        ) %>%
            formatStyle(
                'Time',
                background = styleColorBar(range(0,max(tdata$Time)), 'blue'),
                align = 'bottom',
                backgroundSize = '100% 45%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left') %>%
            formatStyle(columns = "Number",
                        color = 'black',
                        fontWeight = 'bold',
                        fontSize = '24px'
                        ) %>%
            formatStyle(names(tdata), 'vertical-align'='top')
        
    })
    
    output$ingredients <- renderDT({
        
        validate(
            need(food.df$ID[input$recipes_rows_selected] > 0
                 , "Select a food item to see ingredients!"))
        
        tdata <- v.food_ing.df %>%
            filter(Food_ID == food.df$ID[input$recipes_rows_selected]) %>%
            mutate(IngredientType = paste0("<i class='fa fa-",icon
                                           ,"' style='color:",color,";'></i>")) %>%
            # would need to perform any adjustments here for serving size changes?
            select(IngredientType, Ingredient, Measurement, QTY)
        
        datatable(tdata, rownames = FALSE, selection = 'none', escape = -0
                  , colnames = c('Type' # make this icon
                                 , 'Ingredient' 
                                 , 'Measure'
                                 , 'Amount')
                  , options = list(
                      pageLength = nrow(tdata)
                      , ordering=FALSE
                      , dom = 't')
        ) %>%
            formatStyle(columns = c("IngredientType", "Ingredient"),
                        color = 'black',
                        fontWeight = 'bold',
                        fontSize = '24px'
            )
        
    })
    
})
# END ------------------------

