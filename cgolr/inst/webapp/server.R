library(shiny)
require(lattice)

shinyServer(function(input, output) {
    ## initialize object
    init_fun <- function(prop_fill) {
        ret <- cgolr_new( obj.dim[1], obj.dim[2])
        if (prop_fill == 0) {
            init_grid_crosshairs(ret)
        } else {
            init_grid_random(ret, prob=prop_fill)
        } 
        return(ret)
    }
    ## user input, initialize: reset / prop_fill 
    trigger <- reactive({
        not_used <- input$reset
        ## global assignment, need the actual object
        obj <<- init_fun(input$prop_fill)
    })
    ## color, rules
    obj_prep <- reactive({
        ## trigger object reset, if present
        trigger()
        ## change rules if needed
        obj$settings <- cgolr_settings(rule_by_name(name=input$rule))
        cgolr_init_rules(obj)
        ## process color
        .colfun <- allowed_cols[[input$col]]
        ## call reactive-created as obj, not as fun
        ## this assignment fails for obj()$settings <- 
        obj$settings <- list_append(
            .colfun(),obj$settings
        )
        init_plot(obj)
        obj
    })
    
    auto_step <- reactive({
        as.numeric(input$auto_step) * 1e3
    })
    manual_step <- reactive({
        input$step
    })
    
    ## process button press
    obj_fin <- reactive({
        manual_step()
        ## check autostep interval
        if ( auto_step() != 0) {
            invalidateLater(auto_step())
        } 
        ret <- obj_prep()
        ## set new rule
        ## advance and return
        ret$steps(input$nstep)
        ret
    })
    ## report current state
    output$theAge <- renderText({paste0('Total Steps: ', obj_fin()$age)})
    ## prep the plot
    output$thePlot <- renderPlot({
            levelplot(obj_fin())
        },
        height=obj.dim[1], width=obj.dim[2] 
    )
  }
)
