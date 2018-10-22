#' Generate documentation for the files of a directory.
#'
#' @param input_directory The directory of files to be documented.
#' @param output_file The path to the output file that will be generated.
#' @param annotation_file The path to the annotation file if present.
#'
#' @export
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
  annotation_file = NULL
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

  # Determine the old working directory.
  old_wd <- getwd()

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
  annotations <- extract_annotations(yml)

  # Create a temporary directory to work in. Make sure it is empty then copy
  # the refs folder from extdata to it.
  dirtemp <- input_directory
  dirtemp <- fix_path(dirtemp)

  dir_contents <- list.files(
    dirtemp, recursive = TRUE, full.names = TRUE, include.dirs = TRUE
  )
  dir_contents <- fix_path(dir_contents)
  dir_contents <- dir_contents[dir_contents != dirtemp]
  if(length(dir_contents) > 0){
    unlink(dir_contents)
  }
  copy_success <- file.copy(
    from = system.file(
      package = "documenter",
      "extdata", "refs"
    ),
    to = dirtemp,
    overwrite = TRUE,
    recursive = TRUE
  )
  if(!copy_success){stop("Copying of the template failed!")}

  # Find the files.
  template_dir <- file.path(dirtemp, "refs")
  all_files <- list.files(
    template_dir, recursive = TRUE, full.names = TRUE, include.dirs = TRUE
  )

  # Modify the docProps files app.xml and core.xml, which contain the document
  # company and creator names, respectively.
  app.xml_file <- all_files[grepl("app.xml$", all_files)][1]
  app.xml <- readLines(app.xml_file)
  app.xml <- gsub("COMPANY_NAME", annotations$tags["company"], app.xml)
  writeLines(app.xml, app.xml_file)

  core.xml_file <- all_files[grepl("core.xml$", all_files)][1]
  core.xml <- readLines(core.xml_file)
  core.xml <- gsub("CREATOR_NAME", annotations$tags["creator"], core.xml)
  writeLines(core.xml, core.xml_file)

  # Load the overview file and format for insertion.
  has_overview <- FALSE
  if(annotations$tags["overview_file"] != ""){
    has_overview <- TRUE
  }
  paragraph_start <- paste0(
    '<w:p w:rsidP="00E152FA" w:rsidRDefault="00E152FA" w:rsidR="00E152FA">',
    '<w:ind w:firstLine="720"/></w:pPr><w:r><w:t>'
  )
  paragraph_close <- '</w:t></w:r></w:p>'
  overview_pat <- paste0(
    paragraph_start, "Overview contents go here.", paragraph_close
  )
  if(has_overview){
    overview_file <- file.path(
      input_directory, annotations$tags["overview_file"]
    )
    overview <- readLines(overview_file)
    overview <- paste0(paragraph_start, overview, paragraph_close)
    overview <- paste0(overview, collapse = "")
  }else{
    overview_file <- ""
    overview <- paste0(paragraph_start, "", paragraph_close)
  }

  # Modify document.xml by replacing tags with their corresponding annotation.
  document.xml_file <- all_files[grepl("document.xml$", all_files)][1]
  document.xml <- readLines(document.xml_file)
  document.xml <- gsub("Package Title", annotations$tags["title"], document.xml)
  document.xml <- gsub(
    "Package Subitle", annotations$tags["subtitle"], document.xml
  )
  document.xml <- gsub(
    "To be distributed", annotations$tags["cover_notes"], document.xml
  )
  document.xml <- gsub("The date", annotations$tags["date"], document.xml)
  document.xml <- gsub("Authors", annotations$tags["authors"], document.xml)
  document.xml <- gsub(overview_pat, overview, document.xml)

  # Generate file list and match to annotations.
  files <- list.files(
    input_directory,
    recursive = TRUE, all.files = TRUE, full.names = TRUE
  )
  files <- fix_path(files)
  # Remove files the annotation and overview files used in this analysis.
  if(!is.null(annotation_file)){
    files <- files[files != annotation_file]
  }
  if(overview_file != ""){
    files <- files[files != overview_file]
  }
  # Handle the case of no files to process.
  if(length(files) == 0) stop("There are no files!")

  # Create an annotations matrix.
  annotation_df <- matrix("", ncol = 3, nrow = length(files), dimnames = list(
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

  # Define the PAGEREF IDs for each document.
  overview_ref_num <- 527442460
  overview_ref <- paste0("_Toc", overview_ref_num)
  annotation_df$page_ref <- paste0(
    "_Toc",
    (overview_ref_num+1):(overview_ref_num + nrow(annotation_df))
  )
  # Add the file name.
  annotation_df$filename <- rownames(annotation_df)

  # Format the table of contents.
  # Create the overview chunk.
  overview_toc <- create_overview_toc_chunk(overview_ref)
  # Create the other chunks.
  other_toc <- lapply(
    split(annotation_df, f = 1:nrow(annotation_df)),
    function(df){
      create_toc_chunk(df$filename, df$page_ref)
    }
  )
  other_toc <- unlist(other_toc)
  other_toc <- paste0(other_toc, collapse = "")
  # Combine the toc chunks.
  toc_chunk <- paste0(overview_toc, other_toc, sep = "")

  # Update document.xml to include the TOC.
  document.xml <- gsub("TOC_CONTENTS_GO_HERE", toc_chunk, document.xml)

  # Write document.xml.
  writeLines(document.xml, document.xml_file)

  # Zip the contents of the template_dir directory.
  # Add the docx extension if it is not at the end of output_file.
  if(!grepl(".docx$", output_file)){
    output_file <- paste0(output_file, ".docx")
  }
  # Identify files to zip.
  zip_dir <- file.path(dirtemp, "refs")
  old_dir <- getwd()
  setwd(zip_dir)
  files_to_zip <- list.files()
  zip_terminus <- "zipped.zip"
  zip_file <- file.path(dirtemp, "refs", zip_terminus)
  print(getwd())
  print(zip_terminus)
  print(files_to_zip)
  print(list.files())
  zip(
    zipfile = zip_terminus,
    files = files_to_zip
  )
  setwd(old_dir)
  # system(paste0("open ", zip_file))
  # Sys.sleep(10)

  # Move the zip file.
  file.copy(
    from = zip_file,
    to = output_file
  )

  # Print the directory.
  #print(output_file)
}