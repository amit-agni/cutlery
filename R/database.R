#' @title Runs the query given in a text file on  postgreSQL database
#' @description
#' Connects to postgresSQL database and runs the query given in the specified file.
#' Also, stores the results in a .RDS file in the same location as the sqlFilePath
#' Possible enhancements : Option to not store as RDS
#' @param sqlFilePath Location of the Sql file
#' @param readFromRds Looks for the .Rds file in the same path as sqlFilePath
#' @param bkpExisting Backup the existing RDS file
#' @param user database user name, if not provided will be prompted
#' @param password database passowrd, if not provided will be prompted
#' @param dbname database name
#' @param host database host name
#' @param port database port
#'
#' @return data.table
#'
#' @author Amit Agni
#' @family database functions
#' @seealso \code{\link{runSQLinText}}
#' @export
#' @examples
#' runPostgreSQL_inFile(sqlFileName = here("SQL_get-dates.sql"),user = "",password="",readFromRds = F,dbname = "dw",host = "dwserver", port = 5050)


runSQLinFile <- function(sqlFilePath,readFromRds,bkpExisting=T,user = NA,password= NA,dbname,host, port)
{

  #pacman::p_load(RPostgreSQL,getPass,DBI,svDialogs,data.table)

  fnameRds <- paste(tools::file_path_sans_ext(sqlFilePath),".Rds",sep="")

  if(readFromRds == TRUE){
    if(!file.exists(fnameRds)){
      stop(paste("Error : File does not exists  ",fnameRds,". Run again with readFromRds = FALSE"))
    }else{
      DT <- readRDS(file=fnameRds)
    }
  }else{

    con <- .getConnection(dbname = dbname,host = host, port = port,user = user,password = password)
    qry <- .getSQL(sqlFilePath)
    DT <- DBI::dbGetQuery(con, qry)

  }

  dbDisconnect(con)
  setDT(DT)

  #Backup the existing file
  if(bkpExisting) {
    file.copy(fnameRds,paste0(tools::file_path_sans_ext(fnameRds)
                              ,strftime(Sys.time(), format="_Bkp_%Y%m%d_%H%M%S.Rds")))
  }

  saveRDS(DT,file=fnameRds)
  return(DT)

}


#' @title Runs the query given as sqlText string
#'
#' @description
#' Connects to postgresSQL database and runs the query given in the string.
#' Possible enhancements : Store as RDS
#'
#' @param sqlText Sql query
#' @param user database user name, if not provided will be prompted
#' @param password database passowrd, if not provided will be prompted
#' @param dbname database name
#' @param host database host name
#' @param port database port
#'
#' @author Amit Agni
#' @family database functions
#' @seealso \code{\link{runSQLinFile}}
#' @export
#' @examples
#' runPostgreSQL_asText(sqlText = "select * from table limit 19",user = "",password="",readFromRds = F,dbname = "dw",host = "dwserver", port = 5050)
runSQLinText <- function(sqlText,user = NA,password= NA,dbname,host, port)
{

  pacman::p_load(RPostgreSQL,getPass,DBI,svDialogs,data.table)

  con <- .getConnection(dbname = dbname,host = host, port = port,user = user,password = password)
  DT <- DBI::dbGetQuery(con, sqlText)

  dbDisconnect(con)
  setDT(DT)

  return(DT)

}



.getConnection <- function(dbname,host,port,user,password){
  drv <- DBI::dbDriver("PostgreSQL")

  if(is.na(password) == TRUE) {
    con <- RPostgreSQL::dbConnect(drv, dbname = dbname,host =host, port = port,
                                  user = svDialogs::dlg_input(message = "Enter database username")$res, password = getPass::getPass())
  } else {
    con <- RPostgreSQL::dbConnect(drv, dbname = dbname,host =host, port = port,
                                  user = user, password = password)
  }

  con

}


.getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }

    sql.string <- paste(sql.string, line)
  }

  close(con)
  return(sql.string)
}

