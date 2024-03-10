read_excel_allsheets <- function(filename) {
  require(readxl)

  # Get sheets
  sheets <- readxl::excel_sheets(filename)

  # Read sheets
  data_list <- lapply(sheets, function(sh) {
    readxl::read_excel(filename, sheet = sh)
  })
  names(data_list) <- sheets

  # Remove empty sheets
  out <- data_list[sapply(data_list, function(x) dim(x)[1]) > 0]

  return(out)
}