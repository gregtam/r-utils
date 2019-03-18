library(stringr)
library(tibble)
library(dbplyr)

date_file_path <- function(file_path, directory) {
  # Today's date represented as a string
  today_date_str <- as.character(today(), '%m%d')

  # Separate folders of file path
  file_path_vec <- str_split(file_path, '/')[[1]]

  # Append date to file name
  dated_file_name <- paste(today_date_str, file_path_vec[length(file_path_vec)],
                           sep = '_')
  file_path_vec[length(file_path_vec)] <- dated_file_name

  # Join file path vector back into a file path string
  dated_file_path <- str_flatten(file_path_vec, '/')

  return(dated_file_path)
}

transpose_numeric_tibble <- function(data_tbl, numeric_col_name) {
  # Tranposes a one row numeric tibble.

  # 1) Selects all numeric column.
  # 2) Casts integer64 values (from database) to regular integers since
  # integer64 values become weird when transposed.
  # 3) Convert to data.frame, then convert row names to a column.
  # 4) Finally, rename the default V1 column to col_name
  data_t_tbl <- data_tbl %>%
    select_if(is.numeric) %>%
    mutate_if(is.integer64, as.integer) %>%
    t %>%
    as.data.frame %>%
    rownames_to_column %>%
    as_tibble  %>%
    transmute(col_name = rowname,
              !!numeric_col_name := V1)

  return(data_t_tbl)
}

save_tbl_sql_to_db <- function(data_tbl_sql, table_name, impala, schema = '') {
  # Saves a tbl_sql to database

  # String representation of the SELECT command that corresponds to tbl_sql
  select_sql_str <- sql_render(data_tbl_sql) %>% as.character

  if (nchar(schema) > 0) {
    table_name <- paste0(schema, '.', table_name)
  }

  # SQL Query string to create the table
  create_table_str <- paste('CREATE TABLE', table_name, 'AS', select_sql_str)

  # Execute query in Impala
  dbExecute(impala, create_table_str)
}
