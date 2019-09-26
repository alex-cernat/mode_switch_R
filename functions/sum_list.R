

sum_list <- function(list_data) {
  nms <- names(list_data)

  i <- 1
  map(list_data, function(x) {
    out <- sum_tab(x) %>%
      setNames(c("Var", "Code",
                 str_c("Freq_", nms[i]),
                 str_c("Perc_", nms[i])))
    i <<- i + 1
    out
  }) %>%
    reduce(full_join) %>%
    tbl_df()

}
