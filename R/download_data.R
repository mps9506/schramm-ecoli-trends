#################################
#### dataRetrieval functions ####
#################################

## find STORET sites in Texas

find_sites <- function(x){
  ## x = au spatial data frame
  au_df <- x %>%
    st_transform(4326)
  
  ## download ATTAINS TMDL info that lists 
  ## all the AUs with bacteria TMDLs
  cat(crayon::blue("downloading TMDL data from TCEQ\n"))
  ir <- read_ir_info()
  
  
  cat(crayon::blue("downloading site info from WQP\n"))
  ## download site info for all
  ## sites with E. coli data
  df <- whatWQPsites(type = "Stream",
                     organization = "TCEQMAIN",
                     characteristicName = "Escherichia coli",
                     startDateLo = "01-01-2012",
                     startDateHi = "12-31-2019") %>%
    st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure")) %>%
    st_set_crs(4326)
  
  cat(crayon::blue("spatially join AUs and WQP sites\n"))
  ## spatial join au to station
  df <- df %>%
    st_join(au_df %>% dplyr::select(AU_ID, IMP_CONTAC),
            join = st_nearest_feature) %>%
    mutate(segment = str_sub(AU_ID, end = -4)) %>%
    ## classify as having TMDL if it is in the ir dataframe
    mutate(tmdl = case_when(
      segment %in% ir$`Segment Number` ~ 1,
      !(segment %in% ir$`Segment Number`) ~ 0
    )) %>%
    dplyr::filter(MonitoringLocationTypeName == "River/Stream")
  
  
  return(df)
}


## download and clean ecoli data from STORET

download_ecoli <- function(x) {
  
  df <- readWQPqw(siteNumbers = x$MonitoringLocationIdentifier,
                  parameterCd = "Escherichia coli",
                  startDate = "2012-01-01",
                  endDate = "2019-12-31")
  
  df
}

clean_ecoli_data <- function(x) {
  x %>%
    dplyr::filter(ResultMeasure.MeasureUnitCode != "hours") %>%
    dplyr::select(OrganizationIdentifier, OrganizationFormalName, ActivityIdentifier,
                  ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName,
                  ResultMeasureValue, `ResultMeasure.MeasureUnitCode`, ResultCommentText,
                  ProviderName) %>%
    mutate(ResultMeasureValue = case_when(
      ResultMeasureValue == 0 ~ 1,
      ResultMeasureValue != 0 ~ ResultMeasureValue
    ))
}

get_ecoli <- function(x) {
  df <- download_ecoli(x)
  df <- clean_ecoli_data(df)
  return(df)
}


###############################
#### TCEQ Assessment Units ####
###############################
download_au <- function(url,
                        rel_path) {
  cat(crayon::blue("downloading Assessment Units\n"))
  # download the files
  tmpfile <- tempfile()
  ras <- download.file(url = url,
                       destfile = tmpfile,
                       mode = "wb")
  
  # unzip
  tmpdir <- tempdir()
  archive::archive_extract(tmpfile, tmpdir)
  
  filepath <- paste0(tmpdir, rel_path)
  
  # reads
  shp <- sf::st_read(filepath)
  
  # deletes temp
  unlink(tmpdir)
  unlink(tmpfile)
  
  return(shp)
}



###################
#### TMDL Data ####
###################

## download TMDL info from TCEQ
read_ir_info <- function() {
  webpage <- read_html("https://www.tceq.texas.gov/waterquality/tmdl/nav/tmdlsegments")
  tbls <- html_nodes(webpage, "table")
  
  table1 <- webpage %>%
    html_nodes("table") %>%
    .[1:2] %>%
    html_table(fill = TRUE) %>%
    purrr::map_dfr(~ tibble::as_tibble(.)) %>%
    dplyr::filter(stringr::str_detect(Parameters, "Bacteria"))
  return(table1)
}


