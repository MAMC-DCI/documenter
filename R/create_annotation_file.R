#' Create a template annotation file.
#'
#' @param title The title within the documentation file.
#' @param annotation_file The path to the annotation file to be written.
#'
#' @importFrom utils file_test
#'
#' @export
#'
#' @examples
#' \dontrun{create_annotation_file()}
create_annotation_file <- function(
  title = NULL,
  annotation_file = NULL
){
  # Check title.
  if(is.null(title)){
    title <- gsub(".*/|.*\\\\", "", getwd())
  }else if(
    !(
      (length(class(title)) == 1) &&
      (class(title) == "character") &&
      (length(title) == 1)
    )
  ){
    stop("title is invalid.")
  }

  # Check annotation_file.
  if(is.null(annotation_file)){
    annotation_file <- "annotation_file.yml"
  }
  if(
    !(
      (length(class(annotation_file)) == 1) &&
      (class(annotation_file) == "character") &&
      (length(annotation_file) == 1)
    )
  ){
    stop("annotation_file is invalid.")
  }

  # If annotation_file already exists then stop.
  if((file_test("-f", annotation_file))){
    stop(paste0(annotation_file, " already exists!"))
  }

  # Create a template file.
  yml <- yaml::read_yaml(system.file(
    package = "documenter",
    "extdata", "example", "annotation_file.yml"
  ))

  # Add known data.
  #  File title.
  yml[["title"]] <- title

  # Write the file.
  yaml::write_yaml(yml, annotation_file)
}
