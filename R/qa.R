#' QA a data frame per column for number of unique values, percent of NA values, etc.
#'
#' @param df data frame / tibble / etc.
#' @param ext_summary include extra QA statistics in output
#'
#' @export
#'
#' @examples
#' qa(mtcars)
#' iris %>% qa()
qa <- function(df, ext_summary = FALSE, col_dist = FALSE){

  if(any(sapply(df, class) %in% "list")) stop("Function cannot handle type 'list' columns")

  #dimensions
  nrow <- nrow(df)
  ncol <- ncol(df)
  dims <- glue("Rows: {nrow} Cols: {ncol}")

  #head
  head <- df %>% head() %>% as_tibble()

  #summary stats
  summary_stats <- tibble(
    col_names = paste0(names(df), ":"),
    type = sapply(df, function(x) class(x)[1]),
    n_rows = nrow,
    n_zero = sapply(df, function(x) sum(x == 0, na.rm = T)),
    p_zero = paste0(round(100 * sapply(df, function(x) sum(x == 0, na.rm = T)) / nrow(df), 2), "%"),
    n_na = sapply(df, function(x) sum(is.na(x))),
    p_na = paste0(round(100 * sapply(df, function(x) sum(is.na( x ))) / nrow(df), 2), "%"),
    n_inf = sapply(df, function(x) sum(is.infinite(x))),
    p_inf = paste0(round(100 * sapply(df, function(x) sum(is.infinite( x ))) / nrow(df), 2), "%"),
    n_unique = sapply(df, function(x) sum(!is.na(unique( x )))),
    min = sapply(df, function(x) ifelse(!(class(x)[1] %in% c("character", "factor")), as.character(min(x, na.rm = TRUE)), NA)),
    max = sapply(df, function(x) ifelse(!(class(x)[1] %in% c("character", "factor")), as.character(max(x, na.rm = TRUE)), NA)),
    mean = suppressWarnings({
      sapply(df, function(x) ifelse(!(class(x)[1] %in% c("character", "POSIXct")), as.character(round(mean(x, na.rm = TRUE), 3)), NA)
             )})
  )

  if(ext_summary){
    summary_cont_stats <- tibble(
      names = paste0(names(df), ":"),
      n_neg_one = sapply(df, function(x) sum(x == -1, na.rm = T)),
      n_neg_two = sapply(df, function(x) sum(x == -2, na.rm = T)),
      n_UNKNOWN_str = sapply(df, function(x) sum(x == 'UNKNOWN', na.rm = T)),
      n_NA_fwd_slsh_str = sapply(df, function(x) sum(x == 'N/A', na.rm = T)),
      n_NA_str = sapply(df, function(x) sum(x == 'NA', na.rm = T))
    )
  }

  #distribution
  dist <- df %>% apply(., 2, table) %>%
    map(as.data.frame) %>%
    map(~arrange(., Freq %>% desc()))

  # return
  if (ext_summary == TRUE & col_dist == FALSE) {
    out <- list(
      Dimensions = dims,
      Preview = head,
      Summary = summary_stats,
      # `Column Value Distributions` = dist,
      `Extended Summary` = summary_cont_stats
    )
  } else if (ext_summary == FALSE & col_dist == TRUE) {
    out <- list(
      Dimensions = dims,
      Preview = head,
      Summary = summary_stats,
      `Column Value Distributions` = dist
      # `Extended Summary` = summary_cont_stats
    )
  } else if (ext_summary == TRUE & col_dist == TRUE) {
    out <- list(
      Dimensions = dims,
      Preview = head,
      Summary = summary_stats,
      `Column Value Distributions` = dist,
      `Extended Summary` = summary_cont_stats
    )
  } else {
    out <- list(
      Dimensions = dims,
      Preview = head,
      Summary = summary_stats
      # `Column Value Distributions` = dist
      # `Extended Summary` = summary_cont_stats
    )
  }
  return(out)

}
