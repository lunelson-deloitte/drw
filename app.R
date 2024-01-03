library(shiny)
library(bslib)
library(waiter)


source('constants.R')
source('ui.R')
source('files.R')
source('plots.R')
source('modules.R')


ui <- page_navbar(
    tags$head(tags$style(HTML( # make sidebar collapse icon more visible
        "
        .bslib-sidebar-layout > .collapse-toggle {
            padding: 100px 0;
            background-color: #cbb6f2;
        }
        "
    ))),
    title = tags$b("DRW"),
    theme = bs_theme('pulse', version=5),
    nav_panel(title='Home', PAGE_HOME()),
    nav_panel(title='Summary', page_fluid('nice')),
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
    drw_user <- reactiveValues(relative_path=NULL, absolute_path=NULL, data=NULL)
    all_files <- reactiveValues(data=NULL)
    user_files <- reactiveValues(data=NULL)
    
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
                output$drw_root_name <- renderUI({
                    tags$div('You selected "', tags$i(drw_root$absolute_path), '"')
                })
                drw_action_widget <- checkboxGroupInput(
                    'drw_action',
                    label=NULL,
                    choiceNames = c('Data Retention', 'Server Cleanup', 'Dry Run'),
                    choiceValues = c('Retain', 'Archive', 'Ignore'),
                    inline=TRUE,
                    width='100%'
                )
                output$drw_action_widget <- renderUI({drw_action_widget})
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
                if ('Retain' %in% input$drw_action) {
                    drw_retain_name_widget <- textInput(
                        'drw_retain_name',
                        label='Data Retention Folder',
                        placeholder='e.g. Data Retention'
                    )
                    output$drw_retain_name_widget <- renderUI({drw_retain_name_widget})
                } else {
                    output$drw_retain_name_widget <- NULL
                }
                if ('Archive' %in% input$drw_action) {
                    drw_archive_name_widget <- textInput(
                        'drw_archive_name',
                        label='Server Cleanup Folder',
                        placeholder='e.g. Archive Queue'
                    )
                    output$drw_archive_name_widget <- renderUI({drw_archive_name_widget})
                } else {
                    output$drw_archive_name_widget <- NULL
                }
            } else {
                output$drw_confirm_button <- renderUI({tags$i('Waiting selection...')})
                output$drw_archive_name_widget <- NULL
                output$drw_retain_name_widget <- NULL
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
                    excel_generate_button <- actionButton('excel_generate', 'Generate File', width='100%')
                    bslib::card(
                        bslib::card_header(tags$b('Generate Excel File')),
                        bslib::card_body(
                            bslib::layout_columns(
                                excel_name_widget,
                                excel_generate_button,
                                uiOutput('excel_overwrite_widget'),
                                col_widths=c(8, 4, 12)
                            )
                        ),
                        bslib::card_footer(tags$i('Note: File will be generated in root directory.'))
                    )
                })
                output$excel_progress_bar <- renderUI({attendantBar("progress-bar", striped=TRUE)})
            }
        }
    )
    
    panelServer("drw_ignore_directories", inputId='drw_ignore_directories', label='Ignore Directory(s)', all_files=all_files$data, attribute='Directory')
    panelServer("drw_ignore_filenames", inputId='drw_ignore_filenames', label='Ignore File(s)', all_files=all_files$data, attribute='File')
    panelServer("drw_ignore_extensions", inputId='drw_ignore_extensions', label='Ignore Extension(s)', all_files=all_files$data, attribute='Extension')
    
    observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        eventExpr = { input$excel_name },
        handlerExpr = {
            if (file.exists(sprintf('%s.xlsx', input$excel_name))) {
                excel_overwrite_widget <- checkboxInput('excel_overwrite', tags$b('Overwrite?'), width='100%')
                output$excel_overwrite_widget <- renderUI({
                    fluidRow(
                        column(8, tags$b('Warning:', style='color: #8f2e34; display: inline;'), 'File already exists!'),
                        column(4, excel_overwrite_widget)
                    )
                })
            } else {
                output$excel_overwrite_widget <- NULL
            }
        }
    )
    
    observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        eventExpr = { input$excel_generate },
        handlerExpr = {
            if (!is.null(input$excel_overwrite)) {
                if (file.exists(sprintf('%s.xlsx', input$excel_name))) {
                    if (!input$excel_overwrite) {
                        shiny::showModal(shiny::modalDialog(
                            title='File Conflict Warning',
                            tags$div(
                                tags$b('Warning:', style='color: #8f2e34;'),
                                shiny::markdown(OVERWRITE_PERMISSIONS_INSTRUCTIONS)
                            ),
                            footer=tags$div(
                                tags$i('Please select one of the following'),
                                actionButton('excel_overwrite_modal_close', 'Close'),
                                actionButton('excel_overwrite_modal_continue', 'Read Existing'),
                            )
                        ))
                        observeEvent(input$excel_overwrite_modal_close, {
                            removeModal()
                        })
                        observeEvent(input$excel_overwrite_modal_continue, {
                            # skip file generation, move to setting up for file read-in
                            output$excel_overwrite_widget <- NULL
                            removeModal()
                            
                            drw_user$relative_path <- sprintf('%s.xlsx', input$excel_name)
                            drw_user$absolute_path <- tools::file_path_as_absolute(drw_user$relative_path)
                            excel_read_button <- actionButton(
                                'excel_read',
                                sprintf('Read %s', drw_user$relative_path),
                                width='100%'
                            )
                            output$excel_read_button <- renderUI({
                                bslib::card(
                                    bslib::card_header(tags$b('Read Excel File')),
                                    bslib::card_body(excel_read_button),
                                    bslib::card_footer(tags$i('Note: Confirm, save, and close file before reading.'))
                                )
                            })
                        })
                    }
                }
            } else {
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
                    attendant=att,
                    overwrite=input$excel_overwrite
                )
                Sys.sleep(1)
                att$done()
    
                drw_user$relative_path <- sprintf('%s.xlsx', input$excel_name)
                drw_user$absolute_path <- tools::file_path_as_absolute(drw_user$relative_path)
                excel_read_button <- actionButton(
                    'excel_read',
                    sprintf('Read %s', drw_user$relative_path),
                    width='100%'
                )
                output$excel_read_button <- renderUI({
                    bslib::card(
                        bslib::card_header(tags$b('Read Excel File')),
                        bslib::card_body(excel_read_button),
                        bslib::card_footer(tags$i('Note: Confirm, save, and close file before reading.'))
                    )
                })
            }
        }
    )
    
    observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        eventExpr = { input$excel_read },
        handlerExpr = {
            drw_user$data <- read_excel(filename=drw_user$absolute_path)
            output$request_summary_panel <- renderUI({
                if (!is.data.frame(drw_user$data)) {
                    showModal(modalDialog(
                        title='Excel File Read-In Error',
                        tags$div(
                            'The file you provided generated the following error:',
                            drw_user$data,
                            'Please address this error in the Excel file, then reread the file like you did previously until the error is resolved.'
                        ),
                        easyClose=TRUE
                    ))
                    shiny::wellPanel(tags$b('Process halted:', style='color: #8f2e34;'), 'Please address error in read-in.')
                } else {
                    
                    bslib::navset_card_pill(
                        nav_panel(title=tags$b('Directory'), panelUI('drw_actionable_directories')),
                        nav_panel(title=tags$b('Action'), panelUI('drw_actionable_filenames')),
                        nav_panel(title=tags$b('Extension'), panelUI('drw_actionable_extensions')),
                        nav_spacer(),
                        nav_panel(shiny::icon("circle-info"), shiny::markdown(IGNORE_ATTRIBUTES_INSTRUCTIONS)),
                        selected=shiny::icon("circle-info")
                    )
                }
            })
            # output$excel_read_table <- renderTable(frequency_table(all_files=drw_user$data, attribute='Action', include_labels=TRUE))
            # output$excel_read_graph <- renderPlot(frequency_chart(all_files=drw_user$data, attribute='Action'))
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
    )
    
    panelServer("drw_actionable_directories", inputId='drw_actionable_directories', label='Filter Directory(s)', all_files=drw_user$data, attribute=c('Directory', 'Action'))
    panelServer("drw_actionable_filenames", inputId='drw_actionable_filenames', label='Filter Action(s)', all_files=drw_user$data, attribute='Action')
    panelServer("drw_actionable_extensions", inputId='drw_actionable_extensions', label='Filter Extension(s)', all_files=drw_user$data, attribute=c('Extension', 'Action'))
    
    observeEvent(
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        eventExpr = { input$drw_submit },
        handlerExpr = {
            showModal(modalDialog(
                title='Under Construction',
                'This feature is currently unavailable. Thanks for participating anyway!',
                easyClose=TRUE
            ))
        }
    )
}

shinyApp(ui, server)
