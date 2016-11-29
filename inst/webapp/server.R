library(shiny)
require(lattice)

shinyServer(function(input, output) {
    ## initialize object
    init_fun <- function(prop_fill) {
        ret <- cellauto_new( obj.dim[1], obj.dim[2])
        if (prop_fill == 0) {
            init_grid_crosshairs(ret)
        } else {
            init_grid_random(ret, prob=prop_fill)
        } 
        return(ret)
    }
    ## user input, initialize: reset / prop_fill 
    trigger_reset <- reactive({
        not_used <- input$reset
        ## global assignment, need the actual object
        obj <<- init_fun(input$prop_fill)
    })
    ## color, rules
    obj_prep <- reactive({
        ## trigger object reset, if present
        trigger_reset()
        ## change rules if needed
        obj$settings <- cellauto_settings(rule_by_name(name=input$rule))
        obj$decay <- input$decay
        cellauto_init_rules(obj)
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
    
    auto_step_size <- reactive({
        as.numeric(input$auto_step) * 1e3
    })
        
    
    ## process button press
    obj_fin <- eventReactive(
        ## eventExpr
        ## take a step if either button press,
        ## or timer
        {
            ## check autostep interval
            if ( auto_step_size() != 0) {
                invalidateLater(auto_step_size())
            }
            input$step  
            trigger_reset()
        },
        ## valueExpr
        ## prep object, take steps, return object for plotting
        {
            ret <- obj_prep()
            ## set new rule
            ## advance and return
            ret$steps(input$nstep)
        ret
        }
    )
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
