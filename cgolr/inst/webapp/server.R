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
    trigger <- reactive({
        not_used <- input$reset
        ## global assignment, need the actual object
        obj <<- init_fun(input$prop_fill)
    })
    obj_prep1 <- reactive({
        ## trigger object reset
        trigger()
        ## call reactive-created as obj, not as fun
        ## this assignment fails for obj()$settings <- 
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
    ## prep the plot
    output$thePlot <- renderPlot({
            levelplot(obj_fin())
        },
        height=obj.dim[1], width=obj.dim[2] 
    )
  }
)
