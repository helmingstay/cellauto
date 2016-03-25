library(shiny)
library(cgolr)
## possible rules
allowed_rules <- rule_by_name()

source('content.R')

fluidPage(
    includeCSS(paste0(theme_dir, '/', cur_theme)),
    titlePanel("Cellular Automata Explorer (cellautex)"),
    ## user input
    fluidRow(
        column(3, wellPanel(
            sliderInput(
                "prop_fill", "Proportion Fill (0=Crosshairs)",
                min = 0, max = 1, step = 0.01, value=0
            ),
            selectInput("col", "Colors:", 
                choices = names(allowed_cols),
                selected='Reds'
            ),
            selectInput("rule", "Rule:", 
                choices = names(allowed_rules),
                selected='life'
            ),
            selectInput("auto_step", "Autostep interval (seconds, 0 disables):", 
                choices = 2*(0:5),
                selected=0
            ),
            sliderInput("nstep", "Steps (use arrow keys or pointer):", 1,
                min = 1, max = 100, step = 1
            ),
            actionButton('step', "Step"),
            actionButton('reset', "Reset"),
            textOutput('theAge')
        )),
        ## main figure
        column(8, offset=0,
            plotOutput("thePlot", height=obj.dim[1], width='80%')
        )
    ),
    fluidRow(
        column(4, HTML(content.links))
    )
)
