

filt_sparse_rows_cols <- function(df, col_perc_max = 10, row_perc_max = 10) {
  # Filter columnns first
  keep_cols <- df %>% 
    map(~sum(is.na(.x))/length(.x)) %>% 
    map(~.x*100) %>% 
    keep(~.x <= col_perc_max) %>% 
    names()
  
  df_filt <- df %>% 
    dplyr::select(one_of(keep_cols))
  
  # Filter rows now
  keep_rows <- (rowSums(is.na(df_filt))/ncol(df_filt))*100 < row_perc_max
  
  df_filt <- df_filt[keep_rows,]
  return(df_filt)
}