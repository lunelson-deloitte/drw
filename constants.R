library(magrittr)

EXCEL_INSTRUCTIONS <- "
    DRW generates an Excel file with all *tangible* files in the root directory.
    Each row contains basic file information (parent directory, basename,
    extension, file size) which is helpful for filtering the dataset.
" %>% trimws()


IGNORE_ATTRIBUTES_INSTRUCTIONS <- "
    Each tab allows you to specify which directories, files, and/or extensions
    you do **not** want to include in the list of files generated. Leaving these
    widgets empty will return all files under the root directory. However, for
    simpler processing and *ease-of-mind*, you can preemptively remove files that
    you know for a fact should be **ignored** entirely.
    
    **Note**: These are global rules. If you select to ignore '.csv' extensions,
    then *all* '.csv' files will be ignored, regardless of the values on the other
    tabs.
" %>% trimws()


CONFIRM_REQUEST_INSTRUCTIONS <- "
    After filling out the request, you will have one more chance to confirm that
    your input is correct (however you define it). Please refer to the summary
    tables and graphics to observe the change in the root directory should you
    submit your request.
" %>% trimws()


REQUEST_SUMMARY_INSTRUCTIONS <- "
    The following tabs summarize the request you completed in the previous step.
    This is a chance for you to review the impact of your request before submitting
    in the next step.
    
    If you need to make changes, either edit the same file you generated previously
    or start over again (all progress will be lost).
" %>% trimws()


SUBMIT_PANEL_INSTRUCTIONS <- "
    By clicking the all-powerful 'Submit Request' button, you hereby pledge to
    assume full responsibility for any unexpected side effects, spontaneous server
    combustions, or unforeseen Wintel shutdowns that may arise as a result of your
    submission. Data Retention Wizard is legally not liable for your actions.
" %>% trimws()