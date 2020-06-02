library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
(data_frame <- read_tsv("~/GitHub/microbiome_soil/sequences_processing/16S/16S_export_output_jrl2020/16S_stats_jrl2020.tsv") %>%
    slice(-c(1)) %>%
    rename(c( "ID"= "sample-id")) %>%
    rename(c("non_chimeric" = "non-chimeric")) %>%
    rename(c("pct_input_non_chimeric" = "percentage of input non-chimeric")) %>%
    rename(c("pct_filtered_reads" = "percentage of input passed filter"))%>%
    rename(c("pct_input" = "percentage of input merged"))
)


View(data_frame)

# convert character variables into numeric variables

(data_frame$pct_filtered_reads <- as.numeric(data_frame$pct_filtered_reads))
(data_frame$pct_input_non_chimeric <- as.numeric(data_frame$pct_input_non_chimeric))
(data_frame$non_chimeric <- as.numeric(data_frame$non_chimeric))
(data_frame$input <- as.numeric(data_frame$input))

(view(data_frame))

#SUM Up raw reads
data_frame[,2]
sum_rawreads <- sum(data_frame[,2])
sum_rawreads

#Sum up non chimeric reads
data_frame[,8]
sum_reads <- sum(data_frame[,8])
sum_reads


# Removing lines: -B_3_1, -B_5_2 
lines <- c(4,11)
new_data_frame <- data_frame[-lines,] 
new_data_frame

#Sum up rarefaction total reads
new_data_frame[,8]
sum_rarefaction <- sum(new_data_frame[,8])
sum_rarefaction


#creation of the table of reads
table_reads_16s <- data.frame(sum_rawreads, sum_reads, sum_rarefaction, row.names = "16s")
view(table_reads_16s)
                   
# Plot a first graph to see the number of read for each replicate
ggplot(data=data_frame) + geom_col(mapping = aes(x=ID, y = pct_input_non_chimeric, fill = ID))


# remove the Mock, ExtC and PCR lines
(no_test_df <- filter(data_frame, grepl("*-*-", ID)))

# plot a second graph without the three test samples (mock, PCR and EXternal contaminants)
ggplot(data=no_test_df) + geom_col(mapping = aes(x=ID, y = pct_input_non_chimeric, fill = ID))

# Separate the ID column so that we see the replicate number and the sample number of each sample ID

(no_test_df <- separate(data = no_test_df, col = "ID", sep = 3, into = c("sample_number", "replicate_number"), remove = FALSE) %>% 
        group_by(sample_number) %>%
        summarise(total_reads = sum(non_chimeric)))# Here we have the total_reads for each sample

# This graph is a bonus : we can see wich sample have the lowest/higher number of reads
ggplot(data = no_test_df, mapping = aes(x = reorder(sample_number,total_reads, FUN = median), y = total_reads)) + geom_point()
