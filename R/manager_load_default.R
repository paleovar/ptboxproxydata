#' Load a preformatted dataset for the ProxyDataManager
#'
#' @param file character, file containing the dataset
#' @param man_attr character, mandatory attributes for the ProxyDataManager
#'
#' @return
#' @export
manager_load_default <- function(file, dataset_name, dataset_id, man_attr) {
    # define the column types that should be used for the data
    # refer to the ProxyDataManager_MasterSheet for formatting conventions
    # of the mandatory attributes
    col_tps <- readr::cols(
        readr::col_double(),    #entity_id
        readr::col_character(), #entity_name
        readr::col_double(),    #lat
        readr::col_double(),    #long
        readr::col_integer(),   #sample_id
        readr::col_double(),    #age
        readr::col_character(), #proxy_name
        readr::col_character(), #proxy_unit
        readr::col_double()     #proxy_value
    )
    
    # read in the data
    # expect ProxyDataManager conventions
    # throw error otherwise
    attr_trial <- readr::read_csv(file = file,
                                  col_names = TRUE,
                                  n_max = 1) %>% 
        colnames()
    if (any(attr_trial != man_attr)) {
        stop(paste('attributes of file', file, 
             'do not match mandatory attributes expected by ProxyDataManager'))
    }
    dat <- readr::read_csv(file = file,
                           col_names = TRUE,
                           col_types = col_tps)
    
    # add global index and name of dataset
    # and create a Proxytibble from it
    dat <- dat %>% 
        as_Proxytibble() %>% 
        manager_set_dataset_index(dataset = ., dataset_name = dataset_name, dataset_id = dataset_id) %>%  
        manager_set_dataset_order(dataset = .)
    
    return(dat)
}


#' Load the test data set with EPICA DML and DC ice cores for the ProxyDataManager
#'
#' @param file character, file containing the dataset
#' @param dataset_name character, (short) name of the dataset (passed on to helpers)
#' @param dataset_id numeric, id of the dataset (passed on to helpers)
#' @param zoo_format character, format of zoo to be used for storing proxy data (passed on to helpers)
#'
#' @return
#' @export
#' 
#' @references 
#' The water isotope data from the EPICA ice cores on Dronning Maud Land (DML) and at Dome C (DC) are taken from
#' B. Stenni et al.: The deuterium excess records of EPICA Dome C and Dronning Maud Land ice cores (East Antarctica), Quat. Sci. Rev., 29 (1-2), 146-159, https://doi.org/10.1016/j.quascirev.2009.10.009, 2010
manager_load_icecore_testset <- function(file, dataset_name, dataset_id, zoo_format) {
    #load(file = system.file("R","sysdata.rda",package = "PTBoxProxydata")) # loads the icecore_testset as Proxytibble
    
    if (zoo_format == 'zoo') {
        dat <- icecore_testset_zoo
    } else {
        dat <- icecore_testset_Proxyzoo
    }
    out <- dat %>% 
        manager_set_dataset_index(dataset = ., dataset_name = dataset_name, dataset_id = dataset_id) %>% 
        manager_set_dataset_order(dataset = .)
    #rm(icecore_testset_zoo,icecore_testset_Proxyzoo,monticchio_testset_zoo,monticchio_testset_Proxyzoo)
    
    return(out)
}


#' Load the test data set with arboreal pollen data from Lake Monticchio for the ProxyDataManager
#'
#' @param file character, file containing the dataset
#' @param dataset_name character, (short) name of the dataset (passed on to helpers)
#' @param dataset_id numeric, id of the dataset (passed on to helpers)
#' @param zoo_format character, format of zoo to be used for storing proxy data (passed on to helpers)
#'
#' @return
#' @export
#' 
#' @references 
#' The pollen data from the Monticchio archive was first published by
#' Wulf, S., Kraml, M., Brauer, A., et al.: Tephrochronology of the 100 ka lacustrine sediment record of Lago Grande di Monticchio (southern Italy), Quatern. Int., 122, 7–30, https://doi.org/10.1016/j.quaint.2004.01.028, 2004.
#' The univariate arboreal pollen data is decribed in
#' Adam, M., Weitzel, N., and Rehfeld, K.: Identifying Global-Scale Patterns of Vegetation Change During the Last Deglaciation From Paleoclimate Networks, Paleoceanogr. Paleoclimatol., 36, 12, https://doi.org/10.1029/2021PA004265, 2021.
#' It builds on the harmonized ACER database by 
#' Sánchez Goñi, M. F., Desprat, S., Daniau, A.-L., et al.: The ACER pollen and charcoal database: a global resource to document vegetation and fire response to abrupt climate changes during the last glacial period, Earth Syst. Sci. Data, 9, 679–695, https://doi.org/10.5194/essd-9-679-2017, 2017. 
manager_load_monticchio_testset <- function(file, dataset_name, dataset_id, zoo_format) {
    #load(file = system.file("R","sysdata.rda",package = "PTBoxProxydata")) # loads the icecore_testset as Proxytibble
    
    if (zoo_format == 'zoo') {
        dat <- monticchio_testset_zoo
    } else {
        dat <- monticchio_testset_Proxyzoo
    }
    out <- dat %>% 
        manager_set_dataset_index(dataset = ., dataset_name = dataset_name, dataset_id = dataset_id) %>% 
        manager_set_dataset_order(dataset = .)
    #rm(icecore_testset_zoo,icecore_testset_Proxyzoo,monticchio_testset_zoo,monticchio_testset_Proxyzoo)
    
    return(out)
}
