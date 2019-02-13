#' Fix a file path.
#'
#' @param path The path needing to be fixed.
fix_path <- function(
  path
){
  paths <- lapply(
    path,
    function(cpath){
      # Split the path on slashes.
      cpath <- strsplit(cpath, "/|\\\\")
      cpath <- unlist(cpath)

      # Collapse the path.
      if(length(cpath) > 1){
        out <- file.path(cpath[1], cpath[2])
      }
      if(length(cpath) >= 3){
        for(i in 3:length(cpath)){
          out <- file.path(out, cpath[i])
        }
      }
      if(length(cpath) > 1){
        cpath <- out
      }

      return(cpath)
    }
  )
  paths <- unlist(paths)

  # Return the corrected path.
  return(paths)
}