get_files <- function(root_directory, attribute=NULL) {
    all_files <- list.files(
        root_directory,
        full.names=TRUE,
        recursive=TRUE,
        pattern='^[^~]' # ignore opened files
    )
    if (!is.null(attribute)) {
        if (attribute == 'Extension') {
            extensions <- tools::file_ext(all_files)
            return(unique(extensions))
        }
        if (attribute == 'Directory') {
            directories <- dirname(all_files)
            return(directories)
        }
        if (attribute == 'File') {
            names <- tools::file_path_sans_ext(basename(all_files))
            return(names)
        }
    }
    all_files
}


create_file_dataframe <- function(
        root_directory,
        ignore_directories=NULL,
        ignore_filenames=NULL,
        ignore_extensions=NULL) {
    all_files <- get_files(root_directory = root_directory)
    if (!is.null(ignore_directories)) {
        all_files <- all_files[!purrr::map_lgl(dirname(all_files), ~ any(stringr::str_detect(.x, ignore_directories)))]
    }
    if (!is.null(ignore_filenames)) {
        all_files <- all_files[!purrr::map_lgl(tools::file_path_sans_ext(basename(all_files)), ~ any(.x == ignore_filenames))]
    }
    if (!is.null(ignore_extensions)) {
        all_files <- all_files[!purrr::map_lgl(tools::file_ext(all_files), ~ any(.x == ignore_extensions))]
    }
    tibble::tibble(
        'Directory' = dirname(all_files),
        'File' = tools::file_path_sans_ext(basename(all_files)),
        'Extension' = tools::file_ext(all_files),
        'Size' = file.size(all_files),
        'Action' = 'Ignore'
    ) %>% 
        dplyr::arrange(Directory, desc(Size), Extension, File)
}


write_to_excel <- function(
    root_directory,
    all_files_frame,
    title,
    attendant
) {
    library(openxlsx)
    attendant$set(40, text='Gathering root files')
    # all_files_frame <- create_file_dataframe(
    #     root_directory=root_directory,
    #     ignore_directories = ignore_directories,
    #     ignore_filenames = ignore_filenames,
    #     ignore_extensions = ignore_extensions
    # )
    Sys.sleep(1)
    attendant$set(80, text='Writing Excel file')
    wb <- createWorkbook(title=title)
    addWorksheet(wb, "DRW Request", tabColour='green')
    addWorksheet(wb, "DONOTEDIT", tabColour='red')
    writeDataTable(
        wb=wb,
        sheet='DRW Request',
        x=all_files_frame,
        withFilter=TRUE
    )
    writeData(
        wb=wb,
        sheet='DONOTEDIT',
        x=c('Archive', 'Retain', 'Ignore'),
    )
    dataValidation(
        wb=wb,
        sheet='DRW Request',
        rows=1:nrow(all_files_frame),
        cols=ncol(all_files_frame),
        type='list',
        value="DONOTEDIT!$A$1:$A$3"
    )
    # warning message expected: "In sprintf("<x14:dataValidation ... one argument not used by format ..."
    saveWorkbook(wb=wb, file=sprintf('%s.xlsx', file.path(root_directory, title)), overwrite=TRUE)
    Sys.sleep(1)
    attendant$set(99, text='Complete!')
}

read_excel <- function(root_directory, filename) {
    readxl::read_excel(file.path(root_directory, filename)) %>% 
        dplyr::filter(Action %in% c('Retain', 'Archive'))
}

