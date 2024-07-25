setwd("/Users/joycelin/Documents/UW/Course/STAT571/Project/DataSample")
library(dplyr)

biome_processor_wrapper = function(microbiome)
{
  # below we delete the useless columns
  col_index_keep = c(1,2,3,4,5)
  for (col_index in 6:ncol(microbiome)) {
    appear_percent = sum( microbiome[,col_index] > 0 ) / nrow(microbiome )
    if (appear_percent >= 0.05)
    {
      col_index_keep = c(col_index_keep,col_index )
    }
  }
  proced_microbiome = microbiome[,col_index_keep]
  
  # set NA agvhday to very large, indicates no GvHD
  proced_microbiome[is.na(proced_microbiome$agvhday ),]$agvhday = 1000
  
  bin_microbiome <-  proced_microbiome %>%
    # grade variable is a buffer variable in which we only track of those before disease onset
    # label is the final label
    mutate(grade = if_else(sample_day < agvhday , 0 , agvhgrd),
           label = if_else(grade > 0, 1, 0),) %>%
    select(-sample_day,-agvhday,-agvhgrd,-agvhgut, -grade)
  
  return(bin_microbiome)
}


combine_data_by_tax = function(tax_index, microbiome)
{
  unique_group_names = unique(tax[,tax_index])
  unique_length = length(unique_group_names) 
  res_matrix = matrix(nrow = nrow(microbiome), ncol = unique_length)
  res_col_names = c()
  
  for (group_name_index in 1:unique_length ) {
    group_name_i = unique_group_names[group_name_index]
    indexes = as.numeric(na.omit(match(tax[tax[, tax_index] == group_name_i, ]$Species , colnames(microbiome))))
    if (length(indexes) == 0) {
      next
    }
    if (length(indexes) == 1 ) {
      res_col_names = c(res_col_names, group_name_i)
      res_matrix[, length(res_col_names)] = microbiome[, indexes]
      next
    }
    new_column_i = rowSums(microbiome[, indexes] )
    res_col_names = c(res_col_names, group_name_i)
    res_matrix[, length(res_col_names)] = new_column_i
  }
  combined_microbiome = cbind(microbiome$patientID, microbiome$label, res_matrix[, 1:length(res_col_names) ])
  colnames(combined_microbiome) = c("patientID","label", res_col_names )
  return(combined_microbiome)
}

tax = read.csv("Taxonomic Tree.csv")

### check.names is important to include ###
microbiome = read.csv("GvHD_Microbiome_Data_571.csv", check.names = F) 

# drop individuals that only has 1 observations
# for machine learning methods you might want to skip this part
# but might be of interest for models that take into account repeated measures
id_counts <- table(microbiome$patientID)
selected_ids <- names(id_counts[id_counts > 1])
microbiome <- microbiome[microbiome$patientID %in% selected_ids, ]


# note that the function combine_data_by_tax now returns a matrix instead of a dataframe
proced_microbiome = biome_processor_wrapper(microbiome)

## combined data set
combined_by_family = data.frame(combine_data_by_tax(5, proced_microbiome))
combined_by_genus = data.frame(combine_data_by_tax(6, proced_microbiome))