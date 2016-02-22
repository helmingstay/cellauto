library(shiny)
library(cgolr)
## possible rules
allowed_rules <- rule_by_name()

links <- '<p><a href="https://en.wikipedia.org/wiki/Life-like_cellular_automaton#A_selection_of_Life-like_rules">Explanation of Rules</a> <p><a href="https://cgolr.tumblr.com/About">About</a>  <p><a href="https://cgolr.tumblr.com/Links">Links</a>'

fluidPage(
    ## status
    titlePanel(textOutput('theAge')),
    ## user input
    sidebarLayout(
        sidebarPanel(
            selectInput("col", "Colors:", 
                choices = names(allowed_cols),
                selected='Reds'
            ),
            selectInput("rule", "Rule:", 
                choices = names(allowed_rules),
                selected='life'
            ),
            sliderInput("nstep", "Steps (0 resets, use arrow keys or pointer):", 1,
                min = 0, max = 100, step = 1
            ),
            #textOutput('theAge'),
            actionButton('do', "Step!"),
            #HTML(textOutput('theAge')),
            #htmlOutput('theAge'),
            ## helpful links
            HTML(links)
        ),
        ## main figure
        mainPanel(
            plotOutput("thePlot", height=obj.dim[1], width='80%')
        )
    )
)
