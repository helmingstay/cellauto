library(shiny)
require(lattice)

shinyServer(function(input, output) {
    obj <- cgolr_new(
            obj.dim[1], obj.dim[2],
        init.grid='crosshairs'
    )
    ## process button press
    robj <- eventReactive(input$do, {
        ## reset
        if (input$nstep==0) {
            init_grid_crosshairs(obj)
        }
        ## set new rule
        obj$settings <- cgolr_settings(rule_by_name(name=input$rule))
        cgolr_init_rules(obj)
        ## advance and return
        obj$steps(input$nstep)
        obj
    })
    ## report current state
    output$theRule <- renderText({paste0('Current rule: ', robj()$settings$rule_name)})
    ## prep the plot
    output$thePlot <- renderPlot({
            levelplot(robj())
        },
        height=obj.dim[1], width=obj.dim[2] 
    )
  }
)
