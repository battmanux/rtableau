## rtableau: Rcpp bindings for the Tableau SDK
##
## Copyright (C) 2016 Mark Hayden
##
## This file is part of rtableau.
##
## rtableau is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## rtableau is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with rtableau.  If not, see <http://www.gnu.org/licenses/>.

#' Write a R data.frame as a Tableau Data Extract (.tde) file using
#' the Tableau SDK.
#'
#' @param df the data.frame to convert
#' @param filename the output of it (must contain .tde)
#' @param append logical. If TRUE, append data to the file.
#'  If FALSE, any existing file of the name is destroyed.
#'
#' @export
#'
#' @examples
#' write_tde(iris, "iris.tde")
#'
write_tde <- function(df, filename, append = FALSE) {
  if (Sys.getenv("RSTUDIO") == "1") {
    write_tde_rstudio(df, filename, append)
  } else {
    write_tde_shell(df, filename, append)
  }
}

write_tde_shell <- function(df, filename, append = FALSE) {
  if (!append & file.exists(filename)) file.remove(filename)
    
    # helper functions for types
    is.POSIXct <- function(x) inherits(x, "POSIXct")
    is.POSIXlt <- function(x) inherits(x, "POSIXlt")
    is.Date <- function(x) inherits(x, "Date")
    
    # Protect character agains non UTF-8 chars
    names(df) <- iconv(names(df), to = "utf8", sub = "byte")
    
    charCols <- sapply(df, is.character)
    df[charCols] <- lapply(df[charCols], function(x)  iconv(x, to = "latin1", sub = "byte") ) 
    
    # Protect levels agains non UTF-8 chars
    factorCols <- sapply(df, is.factor)
    for (l_col in names(factorCols[factorCols]) ) {
      levels(df[[l_col]]) <- iconv(levels(df[[l_col]]), to = "utf8", sub = "byte")
    }
    
    # convert factors to strings
    factorCols <- sapply(df, is.factor)
    df[factorCols] <- lapply(df[factorCols], as.character)
    
    # convert posixlt to posixct
    posixltCols <- sapply(df, is.POSIXlt)
    df[posixltCols] <- lapply(df[posixltCols], as.POSIXct)
    
    # create a list of desired tableau types based on current R types
    colTypes <- rep('CharString', ncol(df))
    colTypes[sapply(df, is.double)] <- 'Double'
    colTypes[sapply(df, is.integer)] <- 'Integer'
    colTypes[sapply(df, is.logical)] <- 'Boolean'
    colTypes[sapply(df, is.Date)] <- 'Date'
    colTypes[sapply(df, is.POSIXct)] <- 'DateTime'
    
    .Call('rtableau_WriteTDE', PACKAGE = 'rtableau', df, colTypes, filename)
    invisible(NULL)
    
}

write_tde_rstudio <- function(df, filename, append=F) {
  l_rds_file <- tempfile(fileext = ".RDS")
  saveRDS(df, l_rds_file)
  system(
    paste0('Rscript -e "l_df <- readRDS(\'',l_rds_file,'\'); rtableau:::write_tde_shell(l_df, \'',filename,'\',',append,')"')
  )
  unlink(l_rds_file)
  invisible(NULL)
}

