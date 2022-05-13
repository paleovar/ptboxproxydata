# routines to proxy metadata
# inside the `ProxyDataManager`
##
#manager_check_set_proxy_metadata <- function(datasets, dataset_name, dataset_id, 
#                                             meta_root_dir = get("CONFIG", envir = .PTBoxProxydata)$proxy_meta$root_dir,
#                                             meta_file = get("CONFIG", envir = .PTBoxProxydata)$proxy_meta$file) {
#    exists <- datasets %>% 
#        dplyr::select(dataset_short_name, dataset_id) %>% 
#        dplyr::mutate(exists = purrr::map2(.$dataset_short_name, .$dataset_id, function(x,y) manager_check_RDS(x,y,meta_root_dir = meta_root_dir))) %>% 
#        .$exists %>% 
#        unlist()
#    return(exists)
#}


manager_save_proxy_metadata <- function(dataset, dataset_name, dataset_id, 
                                        meta_root_dir = get("CONFIG", envir = .PTBoxProxydata)$proxy_meta$root_dir,
                                        meta_file = get("CONFIG", envir = .PTBoxProxydata)$proxy_meta$file) {
    if (!dir.exists(meta_root_dir)) dir.create(meta_root_dir, recursive = FALSE)
    
    path <- file.path(meta_root_dir,
                      meta_file)

        proxy_name <- dataset %>% 
        dplyr::select(proxy_name) %>% 
        tidyr::unnest(proxy_name) %>% 
        dplyr::distinct(proxy_name)
    
    dataset_id_this <- dataset_id # to prevent confusion in the filtering
    if (!file.exists(path)) {
        meta <- tibble::tibble(dataset_id = dataset_id_this, dataset_name = dataset_name, proxy_name = proxy_name$proxy_name) 
    } else {
        meta <- manager_read_proxy_metadata(path) %>% 
            dplyr::filter(dataset_id != dataset_id_this) %>% # this is to override the respective dataset meta data with newest data
            tibble::add_row(dataset_id = dataset_id_this, dataset_name = dataset_name, proxy_name = proxy_name$proxy_name) %>% 
            dplyr::arrange(dataset_id)
    }
    
    readr::write_csv(meta, 
                     path = path,
                     append = FALSE)
}


manager_read_proxy_metadata <- function(path) {
    col_tps <- readr::cols(
        readr::col_double(),    #dataset_id
        readr::col_character(), #dataset_name
        readr::col_character()  #proxy_name
    )
    meta <- readr::read_csv(file = path,
                            col_types = col_tps)
    return(meta)
}


manager_search_proxy_metadata <- function(proxy_names, dataset_name = NULL, dataset_id = NULL,
                                          meta_root_dir = get("CONFIG", envir = .PTBoxProxydata)$proxy_meta$root_dir,
                                          meta_file = get("CONFIG", envir = .PTBoxProxydata)$proxy_meta$file) {
    path <- file.path(meta_root_dir,
                      meta_file)
    
    if (!file.exists(path)) {
        stop('no meta data file available. Load datasets to RDS first using `PTBoxProxydata::load_set` or `PTBoxProxydata::cache_RDS`')
    }
    
    filtered_names <- manager_read_proxy_metadata(path) %>% 
        dplyr::filter(proxy_name %in% proxy_names) %>% 
        dplyr::select(dataset_name) %>% 
        dplyr::distinct() 
    
    if (!is.null(dataset_name)) {
        filtered_names <- filtered_names %>% 
            dplyr::filter(dataset_name %in% dataset_name)
        if (length(filtered_names) == 0) {
            stop(paste0('no proxies\n', proxy_names, '\nin datassets.\n', dataset_name, '\nCheck proxy name and/or existance of RDS cache for datasets.'))
        }
    }
    
    if (length(filtered_names) == 0) {
        stop(paste0('no proxies\n', proxy_names, '\nin cached database.\nCheck proxy name and/or existance of RDS cache for datasets.'))
    } else {
        return(filtered_names$dataset_name)
    }
}