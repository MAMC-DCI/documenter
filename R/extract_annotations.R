#' Extract annotations from a yaml annotation file.
#'
#' @param annotation_yml The yaml object read by document_it.
#'
#' @examples
#' extract_annotations(annotation_yml)
extract_annotations <- function(
  annotation_yml
){
  # Duplicate just in case.
  annotation_mat <- annotation_yml

  # Handle file annotations.
  tag_types <- c("path","description","comments")
  file_annotation_mat <- annotation_mat[
    names(annotation_mat) %in% "annotations"
  ]
  if(
    (length(file_annotation_mat) == 0) ||
    (length(file_annotation_mat[["annotations"]]) == 0)
  ){
    file_annotations <- matrix("", ncol = 3, dimnames = list(
      NULL,
      c("path","description","comments")
    ))
    file_annotations <- file_annotations[FALSE,,drop=FALSE]
  }else{
    file_annotations <- matrix(
      "",
      nrow = length(file_annotation_mat[["annotations"]]), ncol = 3,
      dimnames = list(NULL, tag_types)
    )
    for(i in 1:nrow(file_annotations)){
      for(tag_type in tag_types){
        if(
          (tag_type %in% names(file_annotation_mat[["annotations"]][[i]])) &&
          (!is.null(file_annotation_mat[["annotations"]][[i]][[tag_type]]))
        ){
          file_annotations[
            i,tag_type
          ] <- file_annotation_mat[["annotations"]][[i]][[tag_type]]
        }
      }
    }
  }

  # Extract other tags.
  other_tags <- c(
    "creator", "company", "title", "subtitle", "cover_notes", "date",
    "overview_file", "authors"
  )
  tags <- as.character(rep("", length(other_tags)))
  names(tags) <- other_tags
  annotation_mat <- annotation_mat[
    names(annotation_mat) %in% other_tags
  ]
  for(i in other_tags){
    if(i %in% names(annotation_mat)){
      value <- unlist(annotation_mat[[i]], recursive = TRUE)[1]
      if(!is.null(value)){
        tags[i] <- value
      }
    }
  }
  # Handle the date.
  if(tags["date"] == ""){
    tags["date"] <- as.character(Sys.Date())
  }

  # Extract annotations.
  annotations <- list(
    "file_annotations" = file_annotations,
    "tags" = tags
  )

  # Return the annotations.
  return(annotations)
}