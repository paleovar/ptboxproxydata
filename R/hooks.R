#' @keywords internal
"_PACKAGE"

PTBoxProxydata_set_config_path <- function(config_path) {
    assign("CONFIG_PATH",
           config_path,
           envir = .PTBoxProxydata)
}


PTBoxProxydata_set_master_backup_path <- function(path) {
    assign("MASTER_BACKUP_PATH",
           path,
           envir = .PTBoxProxydata)
}


PTBoxProxydata_load_config <- function(config) {
    config <- config::get(file = get("CONFIG_PATH", envir = .PTBoxProxydata), 
                          config = config, 
                          use_parent = FALSE)
    
    
    if (config$master$path == "DEFAULT") {
        if ( !dir.exists("~/.ptbox") ) { 
            dir.create("~/.ptbox")
        }
        config$master$path <- path.expand("~/.ptbox/ProxyDataManager_MasterSheet.yml")
        file.copy(paste0(find.package("PTBoxProxydata"), "/extdata/ProxyDataManager_MasterSheet.yml"), config$master$path)

        
    }
     
    if (config$RDS$root_dir == "DEFAULT") {
        if ( !dir.exists("~/.ptbox/PTBoxProxydata_RDS") ) { 
            dir.create("~/.ptbox/PTBoxProxydata_RDS", recursive = T)
        }
        config$RDS$root_dir <- path.expand("~/.ptbox/PTBoxProxydata_RDS")
    }
    
    if (config$proxy_meta$root_dir == "DEFAULT") {
        if ( !dir.exists("~/.ptbox/proxy_metadata") ) { 
            dir.create("~/.ptbox/proxy_metadata", recursive = T)
        }
        config$proxy_meta$root_dir <- path.expand("~/.ptbox/proxy_metadata")
        # file.create(paste0(config$proxy_meta$root_dir, "/StacyDataManager_proxy_metadata.csv")) # Created in manager_save_proxy_metadata()
    }
        
        
    assign("CONFIG", config, envir = .PTBoxProxydata)
        
        
}


.onAttach <- function(libname, pkgname) {
    message <- paste0("Welcome to the PTBox Proxy Data Manger!\nUsing package configuration from\n`",
                      normalizePath(get("CONFIG_PATH", envir = .PTBoxProxydata)),
                      "`.")
    packageStartupMessage(message)
}


.onLoad <- function(libname, pkgname) {
    .PTBoxProxydata <<- new.env(parent = emptyenv())
    
    PTBoxProxydata_set_config_path(system.file('extdata/PTBoxProxydata-config.yml', package = 'PTBoxProxydata'))
    PTBoxProxydata_set_master_backup_path(system.file('extdata/ProxyDataManager_MasterSheet.yml', package = 'PTBoxProxydata'))
    
    # if specific config for system exists, use it
    # this is helpful for pre-configured paths in `PTBoxProxydata-config.yml`
    # on the STACY machines for example
    nodename <- Sys.info()[['nodename']]
    
    # this will load 'default' config if "nodename" config
    # does not exist (using behaviour of config::get)
    config <- PTBoxProxydata_load_config(nodename)
    
    # make config accessible for development
    # comment for production
    #PTBoxProxydata_CONFIG <<- get("CONFIG", envir = .PTBoxProxydata)
}


#' Reload the configuration for the PTBoxProxydata package
#'
#' @param config_path path of the changed new or old `.yml` file containing the configuration. See \emph{Warning}.
#' @param config configuration to load from the `.yml` file provided in \code{config_path}. See \emph{Details}.
#'
#' @return no return
#' @export
#' 
#' @section Warning:
#' Be careful when changing the default configuration of \emph{PTBoxProxydata}. This may turn your package instance to be unable to read the Master Spreadsheet temporarily.
#' If you made changes to the default configuration file, \code{config_path} doesn't have to be supplied. However, it is recommended to test changes with a 
#' custom configuration file first before changing the configuration at it's native location. See \code{vignette('how-to-use-sdm')} as well.
#' 
#' @section Details:
#' If \code{config} is not provided or \code{config = 'default'}, will load the configuration tagged with `default` in the config file. 
#' If custom config is provided but not complete, will always expand config with `default` configuration from config file. Be careful when changing the `default`
#' configuration for your package instance (See \emph{Warning} and \code{vignette('PTBoxProxydata_howto')}).
#' 
#' @section See also:
#' \code{\link{PTBoxProxydata_print_config}, \link{PTBoxProxydata_print_config_path}}
#'
#' @examples
#' \dontrun{
#' # print current config path
#' PTBoxProxydata_print_config_path()
#' # set a config file
#' new_config_file <- 'my/new/config/path/PTBoxProxydata-config.yml'
#' PTBoxProxydata_reload_config(config_path = new_config_file,
#'                              config = 'my_machine')
#' }
PTBoxProxydata_reload_config <- function(config_path = get("CONFIG_PATH", .PTBoxProxydata), config = Sys.info()[['nodename']]) {
    old_config_path <- normalizePath(get("CONFIG_PATH", envir = .PTBoxProxydata))
    old_config <- attr(PTBoxProxydata_get_config(), "config")
    
    PTBoxProxydata_set_config_path(config_path)
    
    PTBoxProxydata_load_config(config)
    
    message <- paste0("PTBoxProxydata: Reloaded package configuration\n`", config, "`\nfrom\n`",
                      normalizePath(get("CONFIG_PATH", envir = .PTBoxProxydata)),
                      "`.\nPrevious configuration was\n`", old_config,"`\nfrom\n`",
                      old_config_path, "`.")
    message(message)
}


#' Print/get configuration of the PTBoxProxydata package and the corresponding configuration path
#'
#' @export
#' @examples 
#' PTBoxProxydata_print_config_path()
#' 
#' # these are equivalent
#' cfg <- PTBoxProxydata_get_config()
#' print(cfg)
#' PTBoxProxydata_print_config()
PTBoxProxydata_print_config <- function() {
    print(get("CONFIG", envir = .PTBoxProxydata))
}


#' @rdname PTBoxProxydata_print_config
#' @export
PTBoxProxydata_get_config <- function() {
    return(get("CONFIG", envir = .PTBoxProxydata))
}


#' @rdname PTBoxProxydata_print_config
#' @export
PTBoxProxydata_print_config_path <- function() {
    message(normalizePath(get("CONFIG_PATH", envir = .PTBoxProxydata)))
}
