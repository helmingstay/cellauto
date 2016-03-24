library(shiny)
library(cgolr)
## possible rules
allowed_rules <- rule_by_name()

links <- '<p><a href="https://en.wikipedia.org/wiki/Life-like_cellular_automaton#A_selection_of_Life-like_rules">Explanation of Rules</a> <p><a href="https://cgolr.tumblr.com/About">About</a>  <p><a href="https://cgolr.tumblr.com/Links">Links</a>'

fluidPage(
    includeCSS(paste0(theme_dir, '/', cur_theme)),
    titlePanel("Cellular Automata Explorer (cellautex)"),
    ## user input
    sidebarLayout(
        sidebarPanel(
            HTML("Initial Conditions<br>(0 = crosshairs, 1-100 = percent random fill):"),
            sliderInput(
                "prop_fill", "",
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
            sliderInput("nstep", "Steps (use arrow keys or pointer):", 1,
                min = 1, max = 100, step = 1
            ),
            textOutput('theAge'),
            actionButton('do', "Step"),
            actionButton('reset', "Reset"),
            ## helpful links
            HTML(links)
        ),
        ## main figure
        mainPanel(
            plotOutput("thePlot", height=obj.dim[1], width='80%')
        )
    )
)
