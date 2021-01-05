#https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
# tidyselect::where() isn't exported for now, this is a workaround
utils::globalVariables("where")


#' Write data to an .R file as a tribble.
#'
#' @param mydata Input data in a tibble or dataframe.
#' @param destfile File to write. If it does not have a ".R" extension, one will be added.
#'
#' @return Returns the original data invisibly to enable piping.
#' @export
write_tribble <- function(mydata, destfile = NA) {
  # check input sanity
  if (!"data.frame" %in% class(mydata)) stop ("Please supply a dataframe/tibble. This won't work with vectors.")
  if (nrow(mydata) == 0) stop ("Input data has zero rows.")

  # get the name of the data provided, as per SO https://stackoverflow.com/questions/39496333/r-get-names-of-arguments-passed-in
  data_name <- as.list(match.call())$mydata

  # if no filename provided, we use the name of the input data + ".R"
  if (is.na(destfile)) destfile <- paste0(data_name, ".R")

  # make sure the filename ends with ".R"
  if (!grepl(x = destfile, pattern = "\\.R")) destfile <- paste0(destfile, ".R")

  # header line
  col_names = paste0("~", names(mydata))
  header_row <- stringr::str_c(col_names, sep = ", ", collapse = ", ") %>%
    paste0(",")

  # data rows
  data_rows <- mydata %>%
    dplyr::mutate(dplyr::across(where(is.character), function(x) paste0("\"", x, "\""))) %>%
    dplyr::mutate(dplyr::across(where(is.character), function(x) stringr::str_replace_all(x, "\\\\", "\\\\\\\\"))) %>%
    tidyr::unite("rows", dplyr::everything(), sep = ", ") %>%
    dplyr::mutate("rows" = paste0(rows, ",")) %>%
    dplyr::pull(rows)

  # put it together
  file_text <- c(
    paste0( data_name, " <- dplyr::tribble("),
    header_row,
    data_rows,
    ")"
  )

  # write to file
  file_text %>%
    readr::write_lines(file = paste0(destfile))
}

#' Save data in a tibble to file as a Markdown table
#'
#' @param mydata A tibble/dataframe to format as a Markdown table.
#' @param destfile File where output will be saved.
#'
#' @return The original data.
#' @export
write_md_table <- function(mydata, destfile = NA){
  if (!"data.frame" %in% class(mydata)) stop ("Please supply a dataframe/tibble. This won't work with vectors.")
  if (nrow(mydata) == 0) stop ("Input data has zero rows.")
  if (is.na(destfile)) stop ("Please provide a filename.")

  header_row <- stringr::str_c(names(mydata), sep = " | ", collapse = " | ")
  header_row <- paste0("| ", header_row, " |")

  separator <- paste0("|", stringr::str_c(rep("-|", ncol(mydata)), collapse=""))

  data_rows <- mydata %>%
    tidyr::unite("rows", dplyr::everything(), sep = " | ") %>%
    dplyr::pull("rows")

  data_rows <- paste0("| ", data_rows , " |")

  file_text <-  c(header_row,
                  separator,
                  data_rows)

  file_text %>%
    readr::write_lines(file = destfile)

  # return mydata invisibly so it can be used in a pipe
  invisible(mydata)
}

#' Print data in a tibble to the console as a Markdown table
#'
#' @param mydata A tibble/dataframe to format as a Markdown table.
#'
#' @return The original data.
#' @export
print_md_table <- function(mydata){
  if (!"data.frame" %in% class(mydata)) stop ("Please supply a dataframe/tibble. This won't work with vectors.")
  if (nrow(mydata) == 0) stop ("Input data has zero rows.")

  header_row <- stringr::str_c(names(mydata), sep = " | ", collapse = " | ")
  header_row <- paste0("| ", header_row, " |")

  separator <- paste0("|", stringr::str_c(rep("-|", ncol(mydata)), collapse=""))

  data_rows <- mydata %>%
    tidyr::unite("rows", dplyr::everything(), sep = " | ") %>%
    dplyr::pull("rows")

  data_rows <- paste0("| ", data_rows , " |")

  file_text <-  c(header_row,
                  separator,
                  data_rows)

  file_text %>%
    stringr::str_c(collapse = "\n") %>%
    message()

  # return mydata invisibly so it can be used in a pipe
  invisible(mydata)
}


rows <- NULL
