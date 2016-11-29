library(shiny)
library(cellauto)
## possible rules
allowed_rules <- rule_by_name()

source('content.R')

fluidPage(
    includeCSS(paste0(theme_dir, '/', cur_theme)),
    titlePanel("cellauto: Cellular Automata Explorer"),
    ## user input
    fluidRow(
        column(3, wellPanel(
            h4("Controls", align='center'),
            actionButton('step', "Advance"),
            actionButton('reset', "Restart"),
            sliderInput(
                "nstep", "Number of Steps", 
                value=1, min=1, max=100, step=1
            ),
            radioButtons(
                "auto_step", "Autostep (seconds, 0 disables)", 
                inline=T,
                c(0,2^(1:4)), selected=0
            ),
            hr(),
            h4("Configuration", align='center'),
            sliderInput(
                "prop_fill", "Init Fill Proportion (0=Crosshairs)",
                min = 0, max = 1, step = 0.01, value=0
            ),
            selectInput("rule", "Rule", 
                choices = names(allowed_rules),
                selected='life'
            ),
            radioButtons("col", "Color scheme", 
                choices = names(allowed_cols),
                inline=TRUE,
                selected='B&W'
            ),
            sliderInput(
                "decay", "Decay rate (fade after death)",
                value=0.5, min=0.01, max=1, step=0.01
            )
        )),
        ## main figure
        column(8, offset=0,
            plotOutput("thePlot", height=obj.dim[1], width='80%'),
            h4(textOutput('theAge'))
        )
    ),
    fluidRow(
        column(4, HTML(content.links))
    )
)
