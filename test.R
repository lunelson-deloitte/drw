library(shiny)
library(bslib)


panelUI <- function(id) {
    dropdown <- uiOutput(NS(id, 'select'))
    plot <- plotOutput(NS(id, 'plot'))
    bslib::layout_columns(
        dropdown,
        plot,
        col_widths=c(4, 8)
    )
}

panelServer <- function(id, inputId, all_files, attribute) {
    moduleServer(id, function(input, output, session) {
        output$select <- renderUI({
            selectInput(
                inputId=inputId,
                label='Something here',
                choices=unique(all_files[attribute]),
                width='100%',
                multiple=TRUE
            )
        })
        output$plot <- renderPlot({
            frequency_chart(all_files=all_files, attribute=attribute)
        })
    })
}


histogramApp <- function() {
    ui <- fluidPage(
        bslib::navset_card_pill(
            nav_panel(title='Directory', panelUI("drw_ignore_directories")),
            nav_panel(title='File', panelUI("drw_ignore_filenames")),
            nav_panel(title='Extension', panelUI("drw_ignore_extensions")),
            id='navset_ignore_panel'
        )
    )
    server <- function(input, output, session) {
        all_files <- reactiveVal(create_file_dataframe('C:/Users/lunelson/Desktop/AAA/Scripts'))
        panelServer("drw_ignore_directories", inputId='drw_ignore_directories', all_files=all_files(), attribute='Directory')
        panelServer("drw_ignore_filenames", inputId='drw_ignore_filenames', all_files=all_files(), attribute='File')
        panelServer("drw_ignore_extensions", inputId='drw_ignore_extesnsions', all_files=all_files(), attribute='Extension')
    }
    shinyApp(ui, server)  
}

histogramApp()
