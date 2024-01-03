frequency_table <- function(all_files, attribute, top_n=10, include_labels=FALSE) {
    library(dplyr)
    library(scales)
    
    all_files <- all_files %>% 
        group_by(across(all_of(attribute))) %>% 
        summarize(
            Count = n(),
            Total = as.integer(sum(Size)),
            .groups = 'keep'
        ) %>%
        ungroup() %>% 
        mutate(
            `Count Pct` = scales::label_percent()(round(Count / sum(Count), 2)),
            `Total Pct` = scales::label_percent()(round(Total / sum(Total), 2))
        ) %>%
        select({attribute}, Count, `Count Pct`, Total, `Total Pct`) %>%
        arrange(desc(Total)) %>% 
        head(top_n)
    
    if (include_labels) {
        all_files <- all_files %>%
            mutate_at(vars(Count, Total), scales::label_comma())
    }
    
    return(all_files)
}


frequency_chart <- function(all_files, attribute, top_n=10) {
    library(ggplot2)
    library(forcats)
    
    all_files_table <- frequency_table(all_files=all_files, attribute=attribute, top_n=top_n)
    fill_value <- 'Total'
    fill_func <- scale_fill_continuous(low='#cbb6f2', high='#593196', labels=scales::label_bytes())
    legend <- theme(legend.position='none', axis.title = element_text(face='bold'))
    if ((vctrs::vec_size(attribute) > 1) & (attribute[2] == 'Action')) { # super janky but works
        fill_value <- 'Action'
        fill_func <- scale_fill_manual(values=c('Ignore'='#cbb6f2', 'Archive'='#845bc2', 'Retain'='#593196'), name='Action')
        legend <- theme(legend.position='top', legend.title=element_text(face='bold'), axis.title = element_text(face='bold'))
    }
    
    ggplot(all_files_table, aes(y=fct_reorder(.data[[attribute[1]]], Total), x=Total, fill=!!as.symbol(fill_value))) +
        geom_bar(stat='identity', position='dodge') +
        labs(y = attribute) +
        fill_func +
        theme_bw() +
        legend
}
