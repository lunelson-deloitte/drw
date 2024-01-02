library(shiny)
library(bslib)
library(waiter)


source('constants.R')
source('ui.R')
source('files.R')
source('plots.R')
source('modules.R')


ui <- page_navbar(
    title = tags$b("[ DRW ]"),
    theme = bs_theme('pulse', version=5),
    nav_panel(title='Home', PAGE_HOME()),
    nav_panel(title='Summary', page_fluid(p('sup'))),
    nav_spacer(),
    nav_menu(
        title='Help',
        align='right',
        'DRW Applications',
        nav_panel(title='Data Retention', page_fluid('sup')),
        nav_panel(title='Server Cleanup', page_fluid('sup')),
        '-----',
        'Frequently Asked Questions',
        nav_panel(title='How does this work?', page_fluid('sup')),
        nav_panel(title='What is the value add?', page_fluid('sup')),
        '-----',
        'Additional Help',
        nav_panel(title='Contact the Team', page_fluid('sup'))
    )
)

server <- function(input, output, session) {
    
    shinyFiles::shinyDirChoose(input, 'drw_root', session=session, roots=c(wd='.'))
    drw_root <- reactiveValues(relative_path=NULL, absolute_path=NULL)
    all_files <- reactiveValues(data=NULL)
    
    
    observeEvent(
        ignoreInit = FALSE,
        ignoreNULL = FALSE,
        eventExpr = { input$drw_root },
        handlerExpr = {
            # parse and set root directory path
            parsed_root <- shinyFiles::parseDirPath(roots=c(wd='.'), input$drw_root)
            drw_root$relative_path <- parsed_root
            # afterwards, render action items
            drw_not_exist <- is.null(drw_root$relative_path)
            drw_empty <- identical(drw_root$relative_path, character(0))
            if (!(drw_not_exist | drw_empty)) {
                drw_root$absolute_path <- tools::file_path_as_absolute(parsed_root)
                drw_action_widget <- checkboxGroupInput(
                    'drw_action',
                    label=NULL,
                    choiceNames = c('Data Retention', 'Server Cleanup', 'Dry Run'),
                    choiceValues = c('retain', 'archive', 'dryrun'),
                    inline=TRUE,
                    width='100%'
                )
                output$drw_action_widget <- renderUI({drw_action_widget})
                output$drw_root_name <- renderUI({
                    tags$div('You selected "', tags$i(drw_root$absolute_path), '"')
                })
            } else {
                output$drw_action_widget <- renderUI({tags$i('Waiting for directory...')})
            }
        }
    )
    
    observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = FALSE,
        eventExpr = { input$drw_action },
        handlerExpr = {
            drw_confirm_button <- actionButton('drw_confirm', 'Confirm Request', width='100%')
            if (vctrs::vec_size(input$drw_action) > 0) {
                output$drw_confirm_button <- renderUI(drw_confirm_button)
            } else {
                output$drw_confirm_button <- renderUI({tags$i('Waiting selection...')})
            }
        }
    )
    
    observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = FALSE,
        eventExpr = { input$drw_confirm },
        handlerExpr = {
            if (input$drw_confirm) {
                all_files$data <- create_file_dataframe(root_directory=drw_root$absolute_path)
                output$generate_ignore_panel <- renderUI({
                    # bslib::navset_card_pill(
                    #     nav_panel(title=tags$b('Directory'), renderUI(drw_ignore_directories_widget)),
                    #     nav_panel(title=tags$b('File'), renderUI(drw_ignore_filenames_widget)),
                    #     nav_panel(title=tags$b('Extension'), renderUI(drw_ignore_extensions_widget)),
                    #     nav_spacer(),
                    #     nav_panel(shiny::icon("circle-info"), shiny::markdown(IGNORE_ATTRIBUTES_INSTRUCTIONS)),
                    #     selected=shiny::icon("circle-info")
                    # )
                    bslib::navset_card_pill(
                        nav_panel(title=tags$b('Directory'), panelUI('drw_ignore_directories')),
                        nav_panel(title=tags$b('File'), panelUI('drw_ignore_filenames')),
                        nav_panel(title=tags$b('Extension'), panelUI('drw_ignore_extensions')),
                        nav_spacer(),
                        nav_panel(shiny::icon("circle-info"), shiny::markdown(IGNORE_ATTRIBUTES_INSTRUCTIONS)),
                        selected=shiny::icon("circle-info")
                    )
                })
                output$generate_excel_panel <- renderUI({
                    excel_name_widget <- textInput(
                        'excel_name',
                        label=NULL,
                        placeholder='Enter name for file (".xlsx" not necessary)',
                        width='100%'
                    )
                    excel_generate_button <- actionButton('excel_generate', 'Generate File')
                    bslib::card(
                        bslib::card_header(tags$b('Generate Excel File')),
                        bslib::card_body(
                            bslib::layout_columns(
                                excel_name_widget,
                                excel_generate_button,
                                col_widths=c(8, 4)
                            )
                        ),
                        bslib::card_footer(tags$i('Note: Name must be unique. File will be generated in root directory.')),
                    )
                })
                output$excel_progress_bar <- renderUI({attendantBar("progress-bar", striped=TRUE)})
            }
        }
    )
    
    panelServer("drw_ignore_directories", inputId='drw_ignore_directories', label='Ignore Directory(s)', all_files=all_files$data, attribute='Directory')
    panelServer("drw_ignore_filenames", inputId='drw_ignore_filenames', label='Ignore File(s)', all_files=all_files$data, attribute='File')
    panelServer("drw_ignore_extensions", inputId='drw_ignore_extesnsions', label='Ignore Extension(s)', all_files=all_files$data, attribute='Extension')
    
    observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        eventExpr = { input$excel_generate },
        handlerExpr = {
            if (!file.exists(input$excel_name)) {
                # NEED: overwrite widget
                att <- Attendant$new("progress-bar", hide_on_max = FALSE)
                att$set(0)
                all_files$data <- create_file_dataframe(
                    root_directory=drw_root$absolute_path,
                    ignore_directories=input$drw_ignore_directories,
                    ignore_filenames=input$drw_ignore_filenames,
                    ignore_extensions=input$drw_ignore_extensions
                )
                write_to_excel(
                    root_directory=drw_root$absolute_path,
                    all_files_frame=all_files$data,
                    title=input$excel_name,
                    attendant=att
                )
                Sys.sleep(1)
                att$done()
                
                output$request_summary_panel <- renderUI({
                    bslib::navset_card_pill(
                        nav_panel(title=tags$b('Table'), p('summary table of request')),
                        nav_panel(title=tags$b('Graph'), p('summary graph of request')),
                        nav_panel(title=tags$b('Text (Experimental)'), p('text summarizing key features of request')),
                        nav_spacer(),
                        nav_panel(shiny::icon("circle-info"), shiny::markdown(REQUEST_SUMMARY_INSTRUCTIONS)),
                        selected=shiny::icon("circle-info")
                    )
                })
                output$request_submit_panel <- renderUI({
                    drw_submit_button <- actionButton('drw_submit', 'Submit Request')
                    bslib::card(
                        bslib::card_header(tags$b('Submit Request')),
                        bslib::card_body(
                            SUBMIT_PANEL_INSTRUCTIONS,
                            drw_submit_button
                        )
                    )
                })
            }
        }
    )
}

shinyApp(ui, server)