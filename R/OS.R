#' @title Kills or ends a windows process
#'
#' @description
#' The task name can be retrieved from the Windows Task list.
#' Useful for killing EXCEL processes that linger-on even after Excel application is quit
#' @param x process to kill
#'
#' @return None
#'
#' @author Amit Agni
#' @family operating system functions
#' @seealso
#' @export
#' @examples
#' killProcess("EXCEL")
killProcess <- function(process_name) {
  Sys.sleep(5)
  x <- system2( 'tasklist' , stdout = TRUE )
  y <- grep(process_name,x,value = T)

  if(length(y) >=1){
    z <- stringr::str_extract_all(y,boundary("word"),simplify = T)[,2]
    lapply(z,function(x) tools::pskill(x, signal = SIGTERM))
  }

  Sys.sleep(5)
}


#' @title Write to clipboard
#'
#' @description
#' Not tested on Mac
#'
#' @param x object to copy
#'
#' @return None
#'
#' @family operating system functions
#' @seealso
#' @export
#' @examples
#' vec <-"test"
#' write2clip(vec)
#'

write2clip <- function(x,row.names=FALSE,col.names=TRUE,...) {
  if(xfun::is_unix() | xfun::is_linux() | xfun::is_macos()) {
    #https://stackoverflow.com/questions/14547069/how-to-write-from-r-to-the-clipboard-on-a-mac
    write.table(x,file=pipe("pbcopy", "w"),sep="\t",row.names=row.names,col.names=col.names,...)
  }else if(xfun::is_windows()) {
    write.table(x,file="clipboard-4056",sep="\t",row.names=row.names,col.names=col.names,...)
  }


}


