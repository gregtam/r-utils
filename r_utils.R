date_file_path <- function(file_path) {
  # Today's date represented as a string
  today_date_str <- as.character(today(), '%m%d')

  # Append date to file path
  dated_file_path <- paste(today_date_str, file_path, sep = '_')

  return(dated_file_path)
}
