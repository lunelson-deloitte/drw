library(bslib)


PAGE_HOME <- function() {
    
    header_object <- tags$div(
        tags$b(tags$h1('Data Retention Wizard', style='text-align: center;')),
        tags$hr()
    )
    
    sidebar_object <- bslib::sidebar(
        title = tags$b(tags$h4('Configure Environment')),
        position='left',
        tags$i('Follow the instructions below to complete your request. See "Help" for more details.'),
        tags$hr(),
        bslib::card(
            bslib::card_header(tags$b('1. Configure Root Directory')),
            bslib::card_body(
                shinyFiles::shinyDirButton('drw_root', 'Select Root Directory', 'title here'),
                uiOutput('drw_root_name')
            ),
            bslib::card_footer(
                tags$i('Read "link" for more details.')
            )
        ),
        bslib::card(
            bslib::card_header(tags$b('2. Configure Root Actions')),
            bslib::card_body(
                uiOutput('drw_action_widget')
            ),
            bslib::card_footer(
                tags$i('Read "link" to learn more about what each action does.')
            )
        ),
        uiOutput('drw_confirm_button')
    )
    
    content_object <- page_fluid(
        tags$h2('Create Excel File'),
        markdown(EXCEL_INSTRUCTIONS),
        layout_columns(
            uiOutput('generate_ignore_panel'),
            uiOutput('generate_excel_panel'),
            col_widths=c(7, 5)
        ),
        uiOutput('excel_progress_bar'),
        tags$hr(),
        tags$h2('Confirm Request'),
        markdown(CONFIRM_REQUEST_INSTRUCTIONS),
        layout_columns(
            uiOutput('request_summary_panel'),
            uiOutput('request_submit_panel'),
            col_widths=c(7, 5)
        ),
    )
    
    bslib::page_fluid(
        useAttendant(),
        header_object,
        bslib::page_sidebar(
            sidebar=sidebar_object,
            content_object
        )
    )
}
