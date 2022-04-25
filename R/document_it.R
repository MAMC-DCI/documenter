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
#' @importFrom utils file_test
#'
#' @examples
#' input <- system.file("extdata", "example", package = "documenter")
#' document_it(
#' input_directory = input,
#' output_file = file.path(tempdir(), "documentation"),
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
      ("character" %in% class(input_directory)) &&
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
      ("character" %in% class(output_file)) &&
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
      ("character" %in% class(title)) &&
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
      ("character" %in% class(annotation_file)) &&
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
    tryCatch(
      {
        overview <- readLines(overview_file)
      },
      error = function(e){
        overview_file <- ""
        overview <- " "
      }
    )
  }else{
    overview_file <- ""
    overview <- " "
  }

  # Define style guides.
  title_style <- "heading 1"
  section_title_style <- "table title"

  denv <- new.env()
  # Modify document.xml by replacing tags with their corresponding annotation.
  denv$docx <- officer::read_docx()
  officer::body_add_par(
    denv$docx,
    htmltools::htmlEscape(
      gsub("$\n|$\r", "", annotations$tags["title"])
    ),
    style = "graphic title"
  )
  officer::cursor_end(denv$docx)
  insert_paragraphs(denv, annotations$tags["subtitle"])
  insert_paragraphs(denv, annotations$tags["cover_notes"])
  insert_paragraphs(denv, annotations$tags["date"])
  insert_paragraphs(denv, annotations$tags["authors"])
  officer::cursor_end(denv$docx)
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
  officer::cursor_end(denv$docx)
  officer::body_add_break(denv$docx, pos = "after")
  officer::cursor_end(denv$docx)

  # Generate file list and match to annotations.
  files <- list.files(
    input_directory,
    recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  files <- fix_path(files)
  # Remove the .git and .Rproj.user folders.
  split_filenames <- strsplit(files, "/|\\\\")
  is_git_file <- lapply(
    split_filenames,
    function(x){any(grepl("^[.]git$", x))}
  )
  is_git_file <- unlist(is_git_file)
  is_rproj_file <- lapply(
    split_filenames,
    function(x){any(grepl("^[.]Rproj.user$", x))}
  )
  is_rproj_file <- unlist(is_rproj_file)

  files <- files[!is_rproj_file & !is_git_file]

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
    contents <- "File not found!"
    try({
      contents <- readLines(annotation_df[i,"path"])
      contents[contents == ""] <- " "
    })
    # tryCatch(
    #   {
    #     contents <- readLines(annotation_df[i,"path"])
    #   },
    #   error = function(e){
    #     contents <- "File not found!"
    #   }
    # )
    # Clean the contents.
    contents <- gsub("\n|\r|\n\r", "", contents)

    # Move to the end of the document.
    officer::cursor_end(denv$docx)

    # Add a page break.
    officer::body_add_break(denv$docx, pos = "after")

    # Retrieve data.
    description <- unlist(strsplit(annotation_df[i,"description"], "\\\\n"))
    if(is.null(description) || (length(description) == 0)){description <- " "}
    comments <- unlist(strsplit(annotation_df[i,"comments"], "\\\\n"))
    if(is.null(comments) || (length(comments) == 0)){comments <- " "}

    # Append data.
    # Add the page title.
    section_name <- htmltools::htmlEscape(rownames(annotation_df)[i])
    section_name <- gsub(
      "^[[:space:]]|\n|\r|\n\r|[[:space:]]$", "", section_name
    )
    officer::cursor_end(denv$docx)
    officer::body_add_par(
      denv$docx,
      section_name,
      style = title_style,
      pos = "on"
    )
    officer::cursor_end(denv$docx)
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
    officer::cursor_end(denv$docx)
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
    officer::cursor_end(denv$docx)
    officer::body_add_par(
      denv$docx,
      "--- Document Contents ---",
      style = section_title_style
    )
    officer::cursor_end(denv$docx)
    insert_paragraphs(denv, contents)

    # Clean up.
    rm(list = c("comments","description", "contents","section_name"))
  }

  # complete <- FALSE
  # while(!complete){
  #   officer::cursor_begin(denv$docx)
  #   tryCatch(
  #     {
  #       officer::cursor_reach(denv$docx, "\n")
  #       officer::body_remove(denv$docx)
  #     },
  #     error = function(e){
  #       complete <- TRUE
  #     },
  #     warning = function(e){
  #       complete <- TRUE
  #     }
  #   )
  # }

  # Print the docx object to a file.
  print(denv$docx, target = output_file)

  # Destroy denv.
  rm(denv)

}
