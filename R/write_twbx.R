## rtableau: Rcpp bindings for the Tableau SDK
##
## Copyright (C) 2017 Emmanuel BATT
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

l_default_template <- 
  l_default_template <- "<?xml version='1.0' encoding='utf-8' ?>

<!-- build 9300.16.0520.1152                                -->
<workbook source-build='9.3.2 (9300.16.0520.1152)' source-platform='win' version='9.3' xmlns:user='http://www.tableausoftware.com/xml/user'>
<preferences>
<preference name='ui.encoding.shelf.height' value='24' />
<preference name='ui.shelf.height' value='26' />
</preferences>
<datasources>
<datasource caption='Extrait source' inline='true' name='dataengine.03xv6u11qi0dc511azw8205s3tkw' version='9.3'>
<connection class='dataengine' dbname='Data/Report/source.tde' schema='Extract' tablename='Extract'>
<relation name='Extract (Extract.Extract)' table='[Extract].[Extract]' type='table' />
</connection>
<column datatype='integer' name='[Number of Records]' role='measure' type='quantitative' user:auto-column='numrec'>
<calculation class='tableau' formula='1' />
</column>
<layout dim-ordering='alphabetic' dim-percentage='0.464903' measure-ordering='alphabetic' measure-percentage='0.535097' show-structure='true' />
<semantic-values>
<semantic-value key='[Country].[Name]' value='&quot;France&quot;' />
</semantic-values>
<date-options start-of-week='monday' />
</datasource>
</datasources>
<worksheets>
<worksheet name='Feuille 1'>
<table>
<view>
<datasources />
<aggregation value='true' />
</view>
<style />
<panes>
<pane>
<view>
<breakdown value='auto' />
</view>
<mark class='Automatic' />
</pane>
</panes>
<rows />
<cols />
</table>
</worksheet>
</worksheets>
<windows source-height='28'>
<window class='worksheet' maximized='true' name='Feuille 1'>
<cards>
<edge name='left'>
<strip size='160'>
<card type='pages' />
<card type='filters' />
<card type='marks' />
</strip>
</edge>
<edge name='top'>
<strip size='31'>
<card type='columns' />
</strip>
<strip size='31'>
<card type='rows' />
</strip>
</edge>
</cards>
</window>
</windows>
<thumbnails />
</workbook>
"

#' Write or Update a R data.frame into a Tableau Document (.twbx) file using
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
#' write_twbx(iris, "iris.twbx")
#' write_twbx(iris, "iris_template.twbx", "iris_instance.twbx")
#'
write_twbx <- function ( df, template_filepath, output_filepath = template_filepath , append=F ) {
  l_tmp_dir <- tempfile("tmp_tableau_report_")
  
  if ( !file.exists(template_filepath)) {
    try(dir.create(paste0(l_tmp_dir, "/Data/Report"), recursive = T))
    
    # Make sure we delete temporary folder even if function crashed in the middle
    reg.finalizer(environment(), function(e)  suppressMessages(unlink(e$l_tmp_dir, recursive = T, force = T)))
    
    write(x=l_default_template, file = paste0(l_tmp_dir, "/", basename(output_filepath),".twb"))
  } else {
    unzip(template_filepath, exdir = l_tmp_dir )
    unlink(paste0(l_tmp_dir, "/Data/Report"), recursive = T)
    unlink(paste0(l_tmp_dir, "/TwbxExternalCache"), recursive = T)
    dir.create(paste0(l_tmp_dir, "/Data/Report"), recursive = T)
  }
  
  l_data <- as.data.frame(df)
  
  write_tde(l_data, paste0(l_tmp_dir,"/Data/Report/source.tde"), append )
  
  l_output_path <- paste0(normalizePath(dirname(output_filepath)), "/", basename(output_filepath))
  
  if ( file.exists(l_output_path))
    unlink(l_output_path)
  
  l_cur_dir <- setwd(l_tmp_dir)
  if (interactive()) cat ("Creating twbx file\n")
  zip(zipfile = l_output_path, files = ".", extras = "-q")
  
  setwd(l_cur_dir)
  unlink(l_tmp_dir, recursive = T)
  
  if ( template_filepath == output_filepath )
    file.copy(l_output_path, template_filepath)
  
  invisible(NULL)
}