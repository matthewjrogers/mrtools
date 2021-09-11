#' Create an excel workbook from a list of data frames
#'
#' @param data_input_list A list of data frames
#' @param sheet_names A vector of sheet names
#' @param workbook_name The desired name of the workbook
#'
#' @import openxlsx
#' @importFrom purrr pmap
#' @import dplyr
#' @export
#'
write_workbook <- function(data_input_list, sheet_names, workbook_name){

  # Note: Function takes a list of data frames for its first input, a vector of sheet names for its second, and a character vector for its third

  sheets <- dplyr::mutate(as.data.frame(sheet_names), input_num = dplyr::row_number()) # create an index to allow sheet_names to map to the input list

  header_style <- openxlsx::createStyle(fgFill = "#e5e5e5", # create a style object for header
                                        textDecoration = "Bold", # bold text
                                        border = "Bottom", # bottom border
                                        wrapText = T, # wrap text
                                        halign = "LEFT") # left alignment for header

  workbook <- openxlsx::createWorkbook() # create an empty workbook

  purrr::pmap(sheets,
              function(sheet_names, input_num){ # create worksheets -- matches sheet name to input data by position
                openxlsx::addWorksheet(workbook,
                                       sheetName = as.character({sheet_names}), # as.character ensure the input is of the proper type
                                       gridLines = FALSE) # turn gridlines off

                openxlsx::setColWidths(workbook, # set column widths
                                       sheet = {input_num},
                                       cols = 1:ncol(data_input_list[[{input_num}]]), # sets colwidth for 1:n columns
                                       widths = 14)

                openxlsx::writeData(workbook, # for each sheet name, write data to the sheet
                                    sheet = {input_num}, # sets sheet number by position in list
                                    data_input_list[[{input_num}]], # selects corresponding element of data input list
                                    startCol = 1,
                                    startRow = 1,
                                    headerStyle = header_style)
              })

  # For simplicty, I have the function save the workbook automatically
  # An alternative would be to do away with the workbook_name input, return the workbook, and use %>%  to pipe the output into a saveWorkbook call

  openxlsx::saveWorkbook(workbook, paste0(workbook_name, ".xlsx"), overwrite = TRUE) # writes .xlsx with name specified in function call
  message(paste0("Workbook ",workbook_name, " saved to working directory"))
}


