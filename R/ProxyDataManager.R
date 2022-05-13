# class definition ----
#' ProxyDataManager S3 class
#'
#' @param master_sheet character file path of the Master Sheet
#'
#' @return
#' 
#' @importFrom magrittr %>%
#' @export
#'
#' @examples 
#' library(PTBoxProxydata)
#' mng <- ProxyDataManager()
#' acer_ap <- load_set(mng, 
#'                     'acer_ap_mfa',
#'                     'tibble',
#'                     FALSE,
#'                     TRUE)
ProxyDataManager <- function(master_sheet = get("CONFIG", envir = .PTBoxProxydata)$master$path) {
    # checks
    if (!file.exists(master_sheet)) {
        master_sheet_backup <- get("MASTER_BACKUP_PATH", envir = .PTBoxProxydata)
        warning(paste0('master sheet does not exist at:\n', master_sheet, '\nusing package `PTBoxProxydata` default sheet at\n',
                       master_sheet_backup))
        master_sheet <- master_sheet_backup
    }
    
    # Create and assign ProxyDataManager class
    mng <- structure(list(), class = 'ProxyDataManager')
    
    # Initialize fields of ProxyDataManager class
    mng$master_sheet <- master_sheet
    mng <- load_master_sheet(mng)
    
    return(mng)
}


# master sheet handling ----
load_master_sheet <-  function(x) UseMethod("load_master_sheet")

load_master_sheet.ProxyDataManager <- function(mng) {
    #col_tps_datasets <- get("CONFIG", envir = .PTBoxProxydata)$master$col_tps_datasets
    
    # datasets
    master_datasets <- read_yml_master_sheet(mng$master_sheet)
    
    mng$datasets <- master_datasets
    
    return(mng)
}


# old xlsx version
#load_master_sheet.ProxyDataManager <- function(mng) {
#    col_tps_datasets <- get("CONFIG", envir = .PTBoxProxydata)$master$col_tps_datasets
#    col_tps_coord_vars_1d <- get("CONFIG", envir = .PTBoxProxydata)$master$col_tps_coord_vars_1d
#    
#    # datasets
#    master_datasets <- readxl::read_xlsx(mng$master_sheet,
#                                         sheet = 1, 
#                                         skip = 7,
#                                         col_names = TRUE,
#                                         col_types = col_tps_datasets)
#    
#    # 1d coordinates and variables
#    # only keep mandatory ('0') at the moment
#    # could extend to others to support more cross-checks
#    mandatory_attr_1d <- readxl::read_xlsx(mng$master_sheet,
#                                           sheet = 2, 
#                                           skip = 4,
#                                           col_names = TRUE,
#                                           col_types = col_tps_coord_vars_1d) %>% 
#        dplyr::filter(supported_by == 0) %>% 
#        .$attribute
#    
#    mng$datasets <- master_datasets
#    mng$mandatory_attr_1d <- mandatory_attr_1d
#    
#    return(mng)
#}


read_yml_master_sheet <- function(file) {#./ProxyDataManager_MasterSheet.yml
    master <- yaml::read_yaml(file) %>% 
        data.frame() %>%
        tibble::as_tibble() %>% 
        tidyr::pivot_longer(everything()) %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(dataset_id = stringr::str_split(name,'\\.',2)[[1]][1] %>% stringr::str_replace('X',''), 
                      name = stringr::str_split(name,'\\.',2)[[1]][2]) %>% 
        tidyr::unnest(dataset_id) %>%
        tidyr::pivot_wider() %>% 
        dplyr::mutate(dplyr::across(dataset_id,as.numeric)) %>% 
        dplyr::mutate(dplyr::across(where(is.factor),as.character))
    return(master)
}


#' Export the master sheet to file
#'
#' @param mng `ProxyDataManager` object
#' @param path file path for output file
#' @param format output format one of `yml` (default), `csv`, `xlsx`
#' @param overwrite logical, overwrite destination `path` if already exists? (default `FALSE`)
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' mng <- ProxyDataManager()
#' export_master_sheet(mng, 'path/to/output_file.format_output_file', format = 'csv')
#' }
export_master_sheet <- function(x, ...) UseMethod('export_master_sheet')

#' @export
export_master_sheet.ProxyDataManager <- function(mng, path, format = 'yml', overwrite = FALSE) {
    if (!(format %in% get("CONFIG", envir = .PTBoxProxydata)$master$export_formats)) {
        stop(paste0('format ', format, 'is not supported.\nChoose one of ', get("CONFIG", envir = .PTBoxProxydata)$master$export_formats))
    }
    if (format == 'yml') {
        file.copy(from = get("CONFIG", envir = .PTBoxProxydata)$master$path,
                  to = path,
                  overwrite = overwrite)
    } else if (format == 'csv') {
        if (file.exists(path) & !overwrite) {
            stop('file exists and overwrite = `FALSE`')
        } else {
            readr::write_csv(x = mng$datasets, 
                             path = path)
        }
    } else { # format == 'xlsx'
        if (file.exists(path) & !overwrite) {
            stop('file exists and overwrite = `FALSE`')
        } else {
            writexl::write_xlsx(x = mng$datasets,
                                path = path)
        }
    }
    message(paste0('wrote master sheet to file\n', path))
}


# printing methods ----
#' Printing methods for the `ProxyDataManager`
#'
#' @param mng `ProxyDataManager`
#'
#' @return
#' @export
#'
#' @examples
#' mng <- ProxyDataManager()
#' print(mng)
print.ProxyDataManager <- function(mng) {
    nsets <- length(unique(mng$datasets$dataset_id))
    
    cat('ProxyDataManager:\n')
    cat('\tMaster Sheet:\n')
    cat(paste('\t\t', normalizePath(mng$master_sheet), '\n'))
    cat(paste('\t\t', as.character(nsets), 'datasets are available\n'))
    cat('Call `preview_all(ProxyDataManager)` for more information.')
}


#' Title
#'
#' @param x object of class `ProxyDataManager`
#'
#' @return empty
#' @export
#'
#' @examples
#' mng <- ProxyDataManager()
#' preview_all(mng)
preview_all <- function(x) UseMethod("preview_all")


#' @export
preview_all.ProxyDataManager <- function(mng) {
    print(mng$datasets)
}


# dataset handling ----
# helper functions for loading and writing datasets with 
# `load_set.ProxyDataManager`
manager_set_dataset_index <- function(dataset, dataset_name, dataset_id) {
    return(dataset %>% 
               dplyr::mutate(dataset_name = dataset_name, dataset_id = dataset_id) %>% 
               dplyr::relocate(dataset_id, dataset_name))
}


manager_set_dataset_order <- function(dataset) {
    return(dataset %>% dplyr::relocate(dataset_id,dataset_name,entity_id,entity_name,site_archive,lat,lon,elev,proxy_name,proxy_data))
}


manager_finalise_dataset <- function(data, output_format, zoo_format) {
    print(zoo_format)
    if (output_format == 'tibble') {
        return(as_tibble(data)) 
    } else if (output_format == 'Proxytibble') {
        return(as_Proxytibble(data, zoo_format)) 
    } else {
        stop(paste0('`output_format` ',output_format,' not supported'))
        }
}

#' Load dataset(s) from a ProxyDataManager
#'
#' @param mng ProxyDataManager
#' @param dataset_names character, single or vector of datasets to load. See \emph{Details}
#' @param proxy_names character, single or vector of proxies to load. See \emph{Details}.
#' @param output_format character, one of c('Proxytibble', 'tibble'). 'tibble' corresponds to `tibble::tibble`
#' @param zoo_format character, one of c('zoo', 'zoo_STACY'). format of the proxy data. See \emph{Details} and `vignette('zoo-support-sdm')`.
#' @param only_mandatory_attr logical, defaults `FALSE`, only return the mandatory meta data in the output
#' @param RDS_root character, root directory for pre-formatted .RDS data. See \emph{Details} for more.
#' @param force_file logical, if TRUE forces to load all `dataset_names` from file and to recreate the corresponding RDS caches. 
#' 
#' @section Details:
#' If loading by `dataset_names` only, datasets will be loaded either from file or from cache and returned in format `output_format`. If 
#' `proxy_names` are additionally supplied, the datasets `dataset_names` are filtered for respective proxies and the filtered data will
#' be returned. This is only possible if datasets have previously been cached to RDS. If only `proxy_names` is specified all available 
#' datasets which contain those proxies will be filtered and returned. Again, this works only on datasets which have previously been 
#' cached to RDS.
#' 
#' Datasets are always cached to RDS and meta data entries are written if they are loaded for the first time. This can be triggered when
#' supplying the respective `dataset_names` to \code{\link{load_set}} or to \code{\link{cache_RDS}}.
#' 
#' If directory specified as \code{RDS_root} does not exist, will try to create it simply (i.e. non-recursively). 
#' Throws an error if \code{RDS_root} cannot be created in this way. In this case check the path you submitted and
#' potentially create the necessary directories. This functionality is kept rudimentary by intention to avoid 
#' filling storage spaces unintentionally.
#' 
#' \code{zoo_format} is ignored if \code{output_format == 'tibble'} because proxy data as `zoo::zoo`/`PTBoxProxydata::zoo_STACY` objects is only 
#' supported by `PTBox` inside the `PTBoxProxydata::Proxytibble` conventions.
#'
#' @return dataset(s) in specified format
#' @export
#'
#' @examples
#' \dontrun{
#' # create a ProxyDataManager instance
#' mng <- ProxyDataManager()
#' # load the icecore_data with default options
#' icecore_data <- load_set(mng, 
#'                          dataset_names = 'icecore_data')
#' }
load_set <- function(x, ...) UseMethod("load_set")

#' @export
load_set.ProxyDataManager <- function(mng, 
                                      dataset_names = NULL, 
                                      proxy_names = NULL,
                                      output_format = 'Proxytibble',
                                      zoo_format = 'zoo',
                                      only_mandatory_attr = FALSE,
                                      RDS_root = get("CONFIG", envir = .PTBoxProxydata)$RDS$root_dir,
                                      force_file = FALSE) {
    # checks
    if (any(is.null(dataset_names)) & any(is.null(proxy_names))) stop('need to supply at least one of `dataset_names` and `proxy_names`')
    if (!(output_format %in% tibble_classes)) stop(paste('unknown `output_format`', output_format))
    if (!(zoo_format %in% zoo_classes)) stop(paste('unknown `zoo_format`', zoo_format))
    lapply(dataset_names, function(nm) {
        if (!nm %in% mng$datasets$dataset_short_name) {
            stop(paste('`dataset_names`', nm, 'not contained in Master Sheet'))
            }
        })
    
    # load the data 
    
    ## check which known datasets contain needed proxy_names
    ## and extend/filter dataset_names accordingly.
    ## this will only check datasets that have been read in
    ## at least once before with their respective `manager_load_*`
    ## because it relies on hidden meta entries via 
    ## `manager_save_meta`
    if (!is.null(proxy_names)) {
        datasets_meta <- manager_search_proxy_metadata(proxy_names = proxy_names)
        if (!is.null(dataset_names)) {
            dataset_names <- dataset_names[dataset_names %in% datasets_meta]
            #message(paste0('loading proxies\n', paste(proxy_names, collapse = ', '), '\from datasets\n', paste(dataset_names, collapse = ', ')))
        } else {
            dataset_names <- datasets_meta
            #message(paste0('loading proxies\n', paste(proxy_names, collapse = ', '), '\from all datasets'))
        }
    }
    
    ## check which datasets are available as RDS
    ## suppress the check if re-creation of all
    ## needed RDS files is forced
    if (!force_file) {
        check_RDS <- manager_check_set_RDS(mng$datasets %>% dplyr::filter(dataset_short_name %in% dataset_names), zoo_format = zoo_format)
        nRDS <- dataset_names[!check_RDS]
        yRDS <- dataset_names[check_RDS]
    } else {
        nRDS <- dataset_names
        yRDS <- character()
    }
    
    ## for datasets which are not available as RDS:
    ## load and transform data to a Proxytibble 
    ## cache data as RDS and 
    ## cache proxy metadata for filter loading
    
    ### either with specific helper `manager_load_*` 
    ### (should be part of PTBoxProxydata for reproducibility but works with function in globalenv as well)
    ### or with default `manager_load_default`
    ###
    ### `manager_load_*` routines are expected to return data.frames/tibbles 
    ### according to `Proxytibble` (proxy data as column 'proxy_data' of `zoo::zoo` or `zoo_STACY` objects) format.
    ### See `vignette('how-to-use-sdm')` and existing `manager_load_*` routines for more details.
    if (length(nRDS) > 0) {
        message(paste0('Loading datasets\n', paste(nRDS, collapse = ', '), '\nfrom file and generating RDS'))
    
        mng_load <- lapply(nRDS, function(x) paste0('manager_load_', x))
        mng_load_def <- 'manager_load_default'
        
        datasets_file <- lapply(1:length(nRDS), function(i) {
            name <- nRDS[[i]]
            path <- mng$datasets[mng$datasets$dataset_short_name == name,] %>%
                .$dataset_path
            id <- mng$datasets[mng$datasets$dataset_short_name == name,] %>%
                .$dataset_id
            loader <- mng_load[[i]]
            
            if (exists(loader, mode = 'function')) {
                
                # load with specific `manager_load_*`
                dataset <- do.call(loader, 
                                   args = list(
                                       path, 
                                       name,
                                       id,
                                       zoo_format)
                )
            } else {
                # load with `manager_load_default`
                # will only load data with mandatory attributes
                dataset <- do.call(mng_load_def, 
                                   args = list(
                                       path, 
                                       name,
                                       id,
                                       zoo_format)
                )
            }

            manager_save_RDS(dataset, name, id, zoo_format)
            manager_save_proxy_metadata(dataset, name, id)
            
            
            if (only_mandatory_attr) {
                dataset <- dataset %>% 
                    dplyr::select(tidyselect::all_of(Proxytibble_colnames_all_zoo()))
            }
            
            return(dataset)
        }) %>% 
            dplyr::bind_rows()
        
    }
    
    ## for datasets which are available as RDS:
    ## load data (in `Proxytibble` format) from RDS
    if (length(yRDS) > 0) {
        message(paste0('Loading datasets\n', paste(yRDS, collapse = ', '), '\nfrom RDS'))
        datasets_RDS <- lapply(1:length(yRDS), function(i) {
            name <- yRDS[[i]]
            path <- mng$datasets[mng$datasets$dataset_short_name == name,] %>%
                .$dataset_path
            id <- mng$datasets[mng$datasets$dataset_short_name == name,] %>%
                .$dataset_id
            
            dataset <- manager_load_RDS(name, id, zoo_format)
            
            if (only_mandatory_attr) {
                dataset <- dataset %>% 
                    dplyr::select(Proxytibble_colnames_all_zoo())
            }
            
            return(dataset)
        }) %>% 
            dplyr::bind_rows() %>% 
            dplyr::arrange(dataset_id)
    }
    ## join data from both sources (direct load and RDS)
    if (length(nRDS) == 0 & length(yRDS) > 0) {
        datasets <- datasets_RDS #%>% manager_finalise_dataset(., output_format, zoo_format))
    } else if (length(nRDS) > 0 & length(yRDS) > 0) {
        datasets <- dplyr::bind_rows(datasets_file, datasets_RDS) %>% 
            dplyr::arrange(dataset_id) #%>% manager_finalise_dataset(., output_format, zoo_format))
    } else if (length(nRDS) > 0 & length(yRDS) == 0) {
        datasets <- datasets_file #%>% manager_finalise_dataset(., output_format, zoo_format))
    } else {
        stop('empty dataset')
    }
    
    ## filter for proxy_names if needed
    if (!is.null(proxy_names)) {
        datasets <- apply_proxy(datasets, select_proxies, proxy_names = proxy_names) %>% 
            update_proxy_names()
    }
    
    return(datasets)
}


#' Cache dataset(s) from a ProxyDataManager to RDS
#'
#' @param mng ProxyDataManager
#' @param dataset_names character, single or vector of datasets to load or 'all' to cache all available datasets. 
#' @param output_format character, one of c('Proxytibble', 'tibble'). 'tibble' corresponds to `tibble::tibble`
#' @param zoo_format character, one of c('zoo', 'zoo_STACY'). format of the proxy data. See \emph{Details} and `vignette('zoo-support-sdm')`.
#' @param RDS_root character, root directory for pre-formatted .RDS data. See \emph{Details} for more.
#' @param force_file logical, if TRUE forces to load all `dataset_names` from file and to recreate the corresponding RDS caches. 
#' 
#' @section Details:
#' Datasets are always cached to RDS and meta data entries are written if they are loaded for the first time. This can be triggered when
#' supplying the respective `dataset_names` to \code{\link{load_set}} or to \code{\link{cache_RDS}}.
#' 
#' If directory specified as \code{RDS_root} does not exist, will try to create it simply (i.e. non-recursively). 
#' Throws an error if \code{RDS_root} cannot be created in this way. In this case check the path you submitted and
#' potentially create the necessary directories. This functionality is kept rudimentary by intention to avoid 
#' filling storage spaces unintentionally.
#' 
#' \code{zoo_format} is ignored if \code{output_format == 'tibble'} because proxy data as `zoo::zoo`/`PTBoxProxydata::zoo_STACY` objects is only 
#' supported by `PTBox` inside the `PTBoxProxydata::Proxytibble` conventions.
#'
#' @return dataset(s) in specified format
#' @export
cache_RDS <- function(x, ...) UseMethod("cache_RDS")

#' @export
cache_RDS.ProxyDataManager <- function(mng, 
                                       dataset_names, 
                                       output_format = 'Proxytibble',
                                       zoo_format = 'zoo',
                                       RDS_root = get("CONFIG", envir = .PTBoxProxydata)$RDS$root_dir,
                                       force_file = FALSE) {
    # checks
    if (!(output_format %in% tibble_classes)) stop(paste('unknown `output_format`', output_format))
    if (!(zoo_format %in% zoo_classes)) stop(paste('unknown `zoo_format`', zoo_format))
    if (all(dataset_names != 'all')) {
        lapply(dataset_names, function(nm) {
            if (!nm %in% mng$datasets$dataset_short_name) {
                stop(paste('`dataset_names`', nm, 'not contained in Master Sheet'))
            }
        })
    } else {
        dataset_names <- mng$datasets$dataset_short_name
    }
    
    # load the data 
    
    ## check which datasets are available as RDS
    ## suppress the check if re-creation of all
    ## needed RDS files is forced
    if (!force_file) {
        check_RDS <- manager_check_set_RDS(mng$datasets %>% dplyr::filter(dataset_short_name %in% dataset_names), zoo_format = zoo_format)
        nRDS <- dataset_names[!check_RDS]
        yRDS <- dataset_names[check_RDS]
    } else {
        nRDS <- dataset_names
        yRDS <- character()
    }
    
    ## for datasets which are not available as RDS:
    ## load and transform data to a Proxytibble 
    ## cache data as RDS and 
    ## cache proxy metadata for filter loading
    
    ### either with specific helper `manager_load_*` 
    ### (should be part of PTBoxProxydata for reproducibility but works with function in globalenv as well)
    ### or with default `manager_load_default`
    ###
    ### `manager_load_*` routines are expected to return data.frames/tibbles 
    ### according to `Proxytibble` (proxy data as column 'proxy_data' of `zoo::zoo` or `zoo_STACY` objects) format.
    ### See `vignette('how-to-use-sdm')` and existing `manager_load_*` routines for more details.
    if (length(nRDS) > 0) {
        message(paste0('Loading datasets\n', paste(nRDS, collapse = ', '), '\nfrom file and generating RDS'))
        
        mng_load <- lapply(nRDS, function(x) paste0('manager_load_', x))
        mng_load_def <- 'manager_load_default'
        
        invisible(lapply(1:length(nRDS), function(i) {
            name <- nRDS[[i]]
            path <- mng$datasets[mng$datasets$dataset_short_name == name,] %>%
                .$dataset_path
            id <- mng$datasets[mng$datasets$dataset_short_name == name,] %>%
                .$dataset_id
            loader <- mng_load[[i]]
            
            if (exists(loader, mode = 'function')) {
                # load with specific `manager_load_*`
                dataset <- do.call(loader, 
                                   args = list(
                                       path, 
                                       name,
                                       id,
                                       zoo_format,
                                       FALSE) #only_mandatory_attr
                )
            } else {
                # load with `manager_load_default`
                # will only load data with mandatory attributes
                dataset <- do.call(loader, 
                                   args = list(
                                       path, 
                                       name,
                                       id,
                                       zoo_format)
                )
            }
            
            manager_save_RDS(dataset, name, id, zoo_format)
            manager_save_proxy_metadata(dataset, name, id)
        }))
    }
}