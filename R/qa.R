# qa

qa <- function(df, option1, option2){

  #dimensions
  nrow <- nrow(df)
  ncol <- ncol(df)
  dims <- glue("Rows: {nrow} Cols: {ncol}")

  #head
  head <- df %>% head()

  #
  summary_stats = tibble(
    names = paste0(names(df), ":"),
    type = sapply(df, class),
    n_rows = nrow,
    n_zero = sapply(df, function(x) sum(x == 0, na.rm = T)),
    p_zero = paste0(round(100 * sapply(df, function(x) sum(x == 0, na.rm = T)) / nrow(df), 2), "%"),
    # n_neg_one = sapply(df, function(x) sum(x == -1, na.rm = T)),
    # n_neg_two = sapply(df, function(x) sum(x == -2, na.rm = T)),
    n_na = sapply(df, function(x) sum(is.na(x))),
    p_na = paste0(round(100 * sapply(df, function(x) sum(is.na( x ))) / nrow(df), 2), "%"),
    n_inf = sapply(df, function(x) sum(is.infinite(x))),
    p_inf = paste0(round(100 * sapply(df, function(x) sum(is.infinite( x ))) / nrow(df), 2), "%"),
    n_unique = sapply(df, function(x) sum(!is.na(unique( x )))),
    min = sapply(df, function(x) ifelse(!(class(x) %in% c("character")), as.character(min(x, na.rm = TRUE)), NA)),
    max = sapply(df, function(x) ifelse(!(class(x) %in% c("character")), as.character(max(x, na.rm = TRUE)), NA)),
    mean = suppressWarnings({
      sapply(df, function(x) ifelse(!(class(x) %in% c("character")), as.character(round(mean(x, na.rm = TRUE), 3)), NA)
             )})
  )

  #distribution
  cols <- summary_stats %>%
    filter(n_unique < 15) %>%
    filter(n_unique != 1) %>% # do not care about distribution for 1 unique value
    select(names) %>% pull()

  # cols[1]

  # table(units_month_close$marketplace) %>% as.data.frame() %>% mutate(Perc = round(Freq/sum(Freq),4) * 100)

  out <- list(
    Dimensions = dims,
    Preview = head,
    Summary = summary_stats
    # ,
    # cols[1]
  )

  return(out)

}
