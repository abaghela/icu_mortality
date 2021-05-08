

filt_sparse_rows_cols <- function(df, col_perc_max = 10, row_perc_max = 10) {
  # Filter columnns first
  keep_cols <- df %>% 
    map(~sum(is.na(.x))/length(.x)) %>% 
    map(~.x*100) %>% 
    keep(~.x <= col_perc_max) %>% 
    names()

  # Filter rows now
  keep_rows <- (rowSums(is.na(df))/ncol(df))*100 < row_perc_max
  
  df_filt <- df[keep_rows,keep_cols]
  return(df_filt)
}