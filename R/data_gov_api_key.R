#Note: The code for data_gov_api_key is lifted from Kyle Walker's tidycensus package (MIT License).

#' Save a data.gov API key
#'
#' It's best practice not to include your API key in scripts. Save it interactively with this function, then it will be available with Sys.getenv("DATAGOV_API_KEY"). All 'api_key' arguments in tidyfec functions default to Sys.getenv("DATAGOV_API_KEY").
#'
#' @param key
#' @param overwrite
#' @param install
#'
#' @return
#' @export
#'
#' @examples
data_gov_api_key <- function (key, overwrite = FALSE, install = FALSE)
{
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    }
    else {
      if (isTRUE(overwrite)) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv = read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("DATAGOV_API_KEY", oldenv),
                         ]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else {
        tv <- readLines(renv)
        if (any(grepl("DATAGOV_API_KEY", tv))) {
          stop("A DATAGOV_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE",
               call. = FALSE)
        }
      }
    }
    keyconcat <- paste0("DATAGOV_API_KEY='", key, "'")
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message("Your API key has been stored in your .Renviron and can be accessed by Sys.getenv(\"DATAGOV_API_KEY\"). \nTo use now, restart R or run `readRenviron(\"~/.Renviron\")`")
    return(key)
  }
  else {
    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(DATAGOV_API_KEY = key)
  }
}
