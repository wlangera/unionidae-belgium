read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  out <- lapply(sheets, function(sh) readxl::read_excel(filename, sheet = sh))
  names(out) <- sheets

  return(out)
}