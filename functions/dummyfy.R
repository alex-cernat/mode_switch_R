dummyfy <- function(var) {
  if(!is.factor(var)) {
  stop("Variable is not factor type")
  }
  
  dat <- NULL
  
  for(i in seq(length(levels(var)))) {
    dat <- cbind(dat, ifelse(var == levels(var)[i], 1, 0))
  }  
  
  dat <- tbl_df(dat)
  names(dat) <- str_replace_all(levels(var), " ", "") 
  
  dat
}
