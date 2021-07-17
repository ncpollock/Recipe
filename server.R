# SERVER START -------------------------------------
shinyServer(function(input, output, session) {
    
    # bs_themer() # for testing shiny themes
    
    
    output$recipes <- renderDT({
        
        tdata <- v.food.df %>%
            # filter(Meal_Prep == input$meal_prep, total_time < input$total_time) %>%
            select(-MealType_ID) %>%
            # mutate Meal_Prep as an icon
            relocate(Description, .after = last_col())

        datatable(tdata, rownames = FALSE, selection = 'single', escape = FALSE
                  , colnames = c('Meal',
                               'Good for Meal Prep?', # max this an Icon!
                               'Prep + Cook Time (Minutes)',
                               'Description')
                  , list(columnDefs = list(list(visible=FALSE, targets=0)))
                  ) %>%
                formatStyle(
                    'total_time',
                    background = styleColorBar(range(0,max(tdata$total_time)), 'blue'),
                    align = 'bottom',
                    backgroundSize = '100% 45%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'left') %>%
            formatStyle(columns = "Name",
                        color = 'black',
                        fontWeight = 'bold') %>%
            formatStyle(names(tdata), 'vertical-align'='top')
     
    })
    
    output$steps <- renderDT({
        
        tdata <- step.df %>%
            filter(Food_ID == food.df$ID[input$recipes_rows_selected]) %>%
            select(-Food_ID) %>%
            relocate(Number, .after = ID) %>%
            arrange(Number)
        
        datatable(tdata, rownames = FALSE, selection = 'single', escape = FALSE
                  , colnames = c('Number',
                                 'Time', # max this an Icon!
                                 'Instruction')
                  , options = list(
                      pageLength = nrow(tdata)
                      , ordering=FALSE
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
    
})
# END ------------------------

