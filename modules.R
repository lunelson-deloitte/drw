panelUI <- function(id) {
    dropdown <- uiOutput(NS(id, 'select'))
    table <- tableOutput(NS(id, 'table'))
    plot <- plotOutput(NS(id, 'plot'))
    bslib::layout_columns(
        tagList(dropdown, tags$hr()),
        table,
        plot,
        col_widths=c(12, 5, 7)
    )
}

panelServer <- function(id, inputId, label, all_files, attribute) {
    moduleServer(id, function(input, output, session) {
        output$select <- renderUI({
            selectInput(
                inputId=inputId,
                label=tags$b(label),
                choices=unique(all_files[attribute]),
                width='100%',
                multiple=TRUE
            )
        })
        output$table <- renderTable({
            frequency_table(all_files=all_files, attribute=attribute, include_labels=TRUE)
        })
        output$plot <- renderPlot({
            frequency_chart(all_files=all_files, attribute=attribute)
        })
    })
}
