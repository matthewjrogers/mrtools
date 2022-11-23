#' Create a wordcloud PNG from word frequencies
#'
#' @param frequencies Word frequencies from get_word_counts()
#' @param file_name Output file name
#' @param colors Defaults to DP colors
#' @param shape Default to circle
#' @param width Width of PNG in pixels
#' @param height Height of PNG in pixels
#'
#' @export
#' @import wordcloud2
#' @import htmlwidgets
#' @import webshot
create_wordcloud <- function(frequencies, file_name, colors = c('#292f69', '#1396d3', '#7ac143'), shape = "circle", width = 1992, height = 1744){

  wc <- wordcloud2::wordcloud2(frequencies, color = rep_len(colors, nrow(frequencies)), shape = shape)

  htmlwidgets::saveWidget(wc, file.path(tempdir(), "temp_wc.html"), selfcontained = FALSE)

  webshot::webshot(file.path(tempdir(), "temp_wc.html"), file_name, vwidth = width, vheight = height, delay = 10)

  unlink(file.path(tempdir(), "temp_wc.html"))

  message("Wordcloud created")
}
