library(shiny)
require(lattice)

shinyServer(function(input, output) {
    obj <- cgolr_new(
            obj.dim[1], obj.dim[2],
        init.grid='crosshairs'
    )
    obj_prep1 <- reactive({
        .colfun <- allowed_cols[[input$col]]
        obj$settings <- list_append(
            .colfun(),obj$settings
        )
        init_plot(obj)
        obj
    })
    ## process button press
    obj_fin <- eventReactive(input$do, {
        ret <- obj_prep1()
        ## reset
        if (input$nstep==0) {
            init_grid_crosshairs(ret)
            ret$age <- 0
        }
        ## set new rule
        ret$settings <- cgolr_settings(rule_by_name(name=input$rule))
        cgolr_init_rules(ret)
        ## advance and return
        ret$steps(input$nstep)
        ret
    })
    ## report current state
    output$theAge <- renderText({paste0('Total Steps: ', obj_fin()$age)})
    #output$theAge <- renderText('1')
    ## prep the plot
    output$thePlot <- renderPlot({
            levelplot(obj_fin())
        },
        height=obj.dim[1], width=obj.dim[2] 
    )
  }
)
