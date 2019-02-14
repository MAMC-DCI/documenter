#' Insert each element of a vector into a document as a string of paragraphs.
#'
#' @param denv The environment of the input docx object.
#' @param vec The character vector to be inserted. Each element should
#' correspond to a separate paragraph.
#'
#' @examples
#' \dontrun{
#' insert_paragraphs(obj, vec)
#' }
insert_paragraphs <- function(
  denv,
  vec
){
  # Append each element.
  vec <- unlist(vec, recursive = TRUE)
  valid_elements <- gsub("[[:space:]]|\n|\n\r|\r", "", vec) != ""
  vec <- vec[valid_elements]
  vec <- gsub("\n$|\n\r$", "", vec)
  vec <- vec[vec != ""]


  iteration <- 0
  for(i in vec){
    officer::cursor_end(denv$docx)
    cursor_pos <- denv$docx$doc_obj$get_at_cursor()
    iteration <- iteration + 1
    # Retrieve text.
    string <- i

    # Create xml.
    new_string <- paste0(
      '<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/',
      '2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/',
      '2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.',
      'org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.',
      'microsoft.com/office/word/2010/wordml\">',
      htmltools::htmlEscape(string),
      '</w:p>'
    )

    # Update obj.
    tryCatch(
      {
        new_string <- iconv(new_string, to = "latin1")
        new_string <- xml2::as_xml_document(new_string)
        xml2::xml_add_sibling(
          cursor_pos,
          new_string,
          .where = "after",
          .copy = TRUE
        )
      },
      error = function(e){}
    )
  }


  # Return nothing.
}
