frequency_table <- function(all_files, attribute, top_n=10) {
    library(dplyr)
    library(scales)
    
    all_files %>% 
        group_by(.data[[attribute]]) %>% 
        summarize(
            Count = n(),
            Total = as.integer(sum(Size))
        ) %>%
        mutate(
            `Count Pct` = scales::label_percent()(round(Count / sum(Count), 2)),
            `Total Pct` = scales::label_percent()(round(Total / sum(Total), 2))
        ) %>%
        # select(Count, `Count Pct`, Total, `Total Pct`) %>%
        arrange(desc(Total)) %>% 
        head(top_n)
}


frequency_chart <- function(all_files, attribute, top_n=10) {
    library(ggplot2)
    library(forcats)
    
    all_files <- frequency_table(all_files=all_files, attribute=attribute, top_n=top_n)
    
    ggplot(all_files, aes(y=fct_reorder(.data[[attribute]], Total), x=Total, fill=Total)) +
        geom_bar(stat='identity') +
        labs(y = attribute) +
        # theme(
        #     axis.title.y=element_text(face='bold'),
        #     axis.title.x=element_text(face='bold')
        # ) +
        scale_fill_continuous(low='#a991d4', high='#593196') +
        theme_bw() +
        theme(legend.position='none')
}
