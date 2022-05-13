# routines to handle RDS caches
# inside the `ProxyDataManager`

manager_check_RDS <- function(dataset_name, dataset_id, zoo_format, RDS_root_dir = get("CONFIG", envir = .PTBoxProxydata)$RDS$root_dir) {
    file <- file.path(RDS_root_dir,
                      paste0(dataset_id, '_', dataset_name, '_', zoo_format, '.rds'))
    return(file.exists(file))
}


manager_check_set_RDS <- function(datasets, zoo_format, RDS_root_dir = get("CONFIG", envir = .PTBoxProxydata)$RDS$root_dir) {
    exists <- datasets %>% 
        dplyr::select(dataset_short_name, dataset_id) %>% 
        dplyr::mutate(exists = purrr::map2(.$dataset_short_name, .$dataset_id, function(x,y) manager_check_RDS(x,y,zoo_format = zoo_format,RDS_root_dir = RDS_root_dir))) %>% 
        .$exists %>% 
        unlist()
    return(exists)
}


manager_save_RDS <- function(dataset, dataset_name, dataset_id, zoo_format, RDS_root_dir = get("CONFIG", envir = .PTBoxProxydata)$RDS$root_dir) {
    if (!dir.exists(RDS_root_dir)) dir.create(RDS_root_dir, recursive = FALSE)
    file <- file.path(RDS_root_dir,
                      paste0(dataset_id, '_', dataset_name, '_', zoo_format, '.rds'))
    saveRDS(dataset,
            file = file)
    Sys.chmod(paths = file,
              mode = "664", use_umask = FALSE)
}


manager_read_RDS <- function(dataset_name, dataset_id, zoo_format, RDS_root_dir = get("CONFIG", envir = .PTBoxProxydata)$RDS$root_dir) {
    file <- file.path(RDS_root_dir,
                      paste0(dataset_id, '_', dataset_name, '_', zoo_format, '.rds'))
    if (!file.exists(file)) {
        stop('file\n', file, '\n does not exist')
    } else {
        return(readRDS(file = file))
    }
}


manager_load_RDS <- function(dataset_name, dataset_id, zoo_format, RDS_root_dir = get("CONFIG", envir = .PTBoxProxydata)$RDS$root_dir) {
    dataset <- manager_read_RDS(dataset_name = dataset_name, dataset_id = dataset_id, zoo_format = zoo_format, RDS_root_dir = RDS_root_dir)
    if (!any(class(dataset) == 'Proxytibble')) {
        stop(paste0('object loaded from .rds file\n', 
                    file.path(RDS_root_dir,paste0(dataset_id, '_', dataset_name, '_', zoo_format, '.rds')),
                    '\n is not of class Proxytibble'))
    } else {
        return(dataset)
    }
}
