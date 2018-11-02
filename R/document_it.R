#' Generate documentation for the files of a directory.
#'
#' @param input_directory The directory of files to be documented.
#' @param output_file The path to the output file that will be generated.
#' @param annotation_file The path to the annotation file if present.
#' @param title The title of the output document.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' document_it(
#' input_directory = "man",
#' output_file = "documentation",
#' annotation_file = NULL
#' )
document_it <- function(
  input_directory,
  output_file,
  annotation_file = NULL,
  title = NULL
){
  # Check input_directory.
  if(
    !(
      !is.null(input_directory) &&
      (length(class(input_directory)) == 1) &&
      (class(input_directory) == "character") &&
      (length(input_directory) == 1) &&
      (nchar(input_directory) > 0) &&
      (dir.exists(input_directory))
    )
  ){
    stop("input_directory is invalid.")
  }
  input_directory <- fix_path(input_directory)

  # Check output_file.
  if(
    !(
      !is.null(output_file) &&
      (length(class(output_file)) == 1) &&
      (class(output_file) == "character") &&
      (length(output_file) == 1) &&
      (nchar(output_file) > 0)
    )
  ){
    stop("output_file is invalid.")
  }
  output_file <- fix_path(output_file)

  # Correct output_file name.
  if(!grepl(".docx$", output_file)){
    output_file <- paste0(output_file, ".docx", sep = "", collapse = "")
  }

  # Handle title.
  if(!is.null(title)){
    if(!(
      (length(class(title)) == 1) &&
      (class(title) == "character") &&
      (length(title) == 1)
    )){
      stop("title is invalid.")
    }
  }

  # Check annotation_file.
  if(
    !is.null(annotation_file) &&
    !(
      (length(class(annotation_file)) == 1) &&
      (class(annotation_file) == "character") &&
      (length(annotation_file) == 1) &&
      (nchar(annotation_file) > 0) &&
      (file_test("-f", annotation_file))
    )
  ){
    stop("annotation_file is invalid.")
  }

  # Load the annotation_file if it is present.
  if(!is.null(annotation_file)){
    yml <- yaml::read_yaml(annotation_file)
  }else{
    yml <- yaml::read_yaml(system.file(
      package = "documenter",
      "extdata", "example", "annotation_file.yml"
    ))
  }

  # Extract annotations from the yml object.
  if(is.null(title)){
    title <- gsub("", "", input_directory)
    title <- gsub("^.*/|^.*\\\\", "", title)
  }
  annotations <- extract_annotations(yml, title)

  # Load the overview file and format for insertion.
  has_overview <- FALSE
  if(annotations$tags["overview_file"] != " "){
    has_overview <- TRUE
  }
  if(has_overview){
    overview_file <- file.path(
      input_directory, annotations$tags["overview_file"]
    )
    overview <- readLines(overview_file)
  }else{
    overview_file <- ""
    overview <- " "
  }

  # Define style guides.
  title_style <- "heading 1"
  section_title_style <- "table title"

  denv <- new.env()
  # Modify document.xml by replacing tags with their corresponding annotation.
  denv$docx <- officer::read_docx() %>%
    officer::body_add_par(
      .,
      htmltools::htmlEscape(
        gsub("$\n|$\r", "", annotations$tags["title"])
      ),
      style = "graphic title"
    )
  officer::cursor_end(denv$docx)
  insert_paragraphs(denv, annotations$tags["subtitle"])
  insert_paragraphs(denv, annotations$tags["cover_notes"])
  insert_paragraphs(denv, annotations$tags["date"])
  officer::cursor_end(denv$docx)
  insert_paragraphs(denv, annotations$tags["authors"])
  officer::body_add_break(denv$docx, pos = "after")
  officer::cursor_end(denv$docx)
  officer::body_add_par(
    denv$docx,
    "Table of Contents",
    style = section_title_style
  )
  officer::cursor_end(denv$docx)
  officer::body_add_toc(denv$docx, style  = NULL, level = 1)
  officer::cursor_end(denv$docx)
  officer::body_add_break(denv$docx, pos = "after")
  officer::cursor_end(denv$docx)
  officer::body_add_par(
    denv$docx,
    "Overview",
    style = title_style
  )
  officer::cursor_end(denv$docx)
  insert_paragraphs(denv, overview)
  officer::body_add_break(denv$docx, pos = "after")

  # Generate file list and match to annotations.
  files <- list.files(
    input_directory,
    recursive = TRUE, all.files = TRUE, full.names = TRUE
  )
  files <- fix_path(files)
  # Remove the annotation and overview files used in this analysis.
  if(!is.null(annotation_file)){
    files <- files[files != annotation_file]
  }
  if(overview_file != " "){
    files <- files[files != overview_file]
  }
  # Handle the case of no files to process.
  if(length(files) == 0) stop("There are no files!")

  # Create an annotations matrix.
  annotation_df <- matrix(" ", ncol = 3, nrow = length(files), dimnames = list(
    gsub(gsub(" $","",file.path(input_directory, " ")), "", files),
    c("path","description","comments")
  ))
  annotation_df <- as.data.frame(annotation_df, stringsAsFactors = FALSE)
  annotation_df$path <- files
  annotation_df[
    annotations$file_annotations[,"path"],
    "description"
  ] <- annotations$file_annotations[,"description"]
  annotation_df[
    annotations$file_annotations[,"path"],
    "comments"
    ] <- annotations$file_annotations[,"comments"]
  annotation_df <- as.data.frame(annotation_df, stringsAsFactors = FALSE)

  # Add documents to the docx object.
  for(i in seq_along(rownames(annotation_df))){
    # Add a page break.
    officer::body_add_break(denv$docx, pos = "after")

    # Retrieve data.
    description <- unlist(strsplit(annotation_df[i,"description"], "\\\\n"))
    if(is.null(description) || (length(description) == 0)){description <- " "}
    comments <- unlist(strsplit(annotation_df[i,"comments"], "\\\\n"))
    if(is.null(comments) || (length(comments) == 0)){comments <- " "}
    contents <- readLines(annotation_df[i,"path"])
    # Clean the contents.
    contents <- gsub("\n|\r|\n\r", "", contents)

    # Append data.
    # Add the page title.
    section_name <- htmltools::htmlEscape(rownames(annotation_df)[i])
    section_name <- gsub(
      "^[[:space:]]|\n|\r|\n\r|[[:space:]]$", "", section_name
    )
    officer::body_add_par(
      denv$docx,
      section_name,
      style = title_style,
      pos = "on"
    )
    has_metadata <- FALSE
    # Add the description if it is present.
    if(
      (length(description) > 1) ||
      (description[1] != " ")
    ){
      officer::body_add_par(
        denv$docx,
        "--- Description ---",
        style = section_title_style
      )
      insert_paragraphs(denv, description)
      has_metadata <- TRUE
    }
    # Add comments if present.
    if(
      (length(comments) > 1) ||
      (comments[1] != " ")
    ){
      officer::body_add_par(
        denv$docx,
        "--- Comments ---",
        style = section_title_style
      )
      insert_paragraphs(denv, comments)
      has_metadata <- TRUE
    }
    # Add the document contents.
    officer::body_add_par(
      denv$docx,
      "--- Document Contents ---",
      style = section_title_style
    )
    insert_paragraphs(denv, contents)
  }

  # Print the docx object to a file.
  print(denv$docx, target = output_file)

  # Destroy denv.
  rm(denv)

}
