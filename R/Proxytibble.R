Proxytibble_class <- c('Proxytibble', 'tbl', 'tbl_df', 'data.frame')
tibble_class <- c('tbl_df', 'tbl', 'data.frame')
# supported output classes
tibble_classes <- c('Proxytibble') #, 'tibble') #,'list','mixed')
zoo_classes <- c('Proxyzoo', 'zoo')

Proxytibble_colnames_all_plane <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_all_plane %>% 
    unlist()
Proxytibble_colnames_all_zoo <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_all_zoo %>% 
    unlist()
Proxytibble_colnames_ent <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_ent
Proxytibble_colnames_proxy_names <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_proxy_names
Proxytibble_colnames_core <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_core
Proxytibble_colnames_age <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_age
Proxytibble_colnames_depth <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_depth
Proxytibble_colnames_proxy_plane <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_proxy_plane

#' @export
Proxytibble_colnames_proxy_data <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxytibble$mandatory_colnames_proxy_data

set_Proxytibble_class <- function(x, class = NULL) {
    class(x) <- c(setdiff(class, Proxytibble_class), Proxytibble_class)
    return(x)
}

# Proxytibble S3 class
#' Proxytibble S3 class
#'
#' @param ... passed to \code{tibble::tibble}
#'
#' @return object of classes c('Proxytibble', 'tbl', 'tbl_df', 'data.frame')
#' 
#' @importFrom rlang `:=`
#' 
#' @export
Proxytibble <- function(...) {
    tb <- as_Proxytibble(...)
    return(tb)
}


has_format_Proxytibble <- function(x) {
    rt <- FALSE
    if (all(class(x) %in% Proxytibble_class)) {
        if (nrow(x) > 0) {
            if (all(colnames(x) %in% Proxytibble_colnames_all_zoo())
                & all(Proxytibble_colnames_all_zoo() %in% colnames(x))) {
                if (class(x[[Proxytibble_colnames_proxy_data()]][[1]]) %in% zoo_classes) {
                    rt <- TRUE
                }
            }
        } else {
            rt <- TRUE
        }
    }
    return(rt)
}


#' Class check for `PTBoxProxydata::Proxytibble`
#' 
#' @param x an object of class 'Proxytibble'
#'
#' @export
is.Proxytibble <- function(x) {
    has_format_Proxytibble(x) & inherits(x, "Proxytibble")
}


#' Conversion of `data.frame`s and `list` of `matrix` objects into `Proxytibble`.
#' 
#' @param x either proxydata (`list` of `matrix` objects) or of class `data.frame`/`tibble::tibble` holding an entire set of paleo data, including agedata and metadata. See \emph{Details} for the expected format.
#' @param agedata agedata, if provided, it has to be of same class as `x`. Ignored if `x` is of `class` `data.frame/tibble::tibble`. See \emph{Details} for the expected format.
#' @param agesumm summary of the agedata, if provided, it has to be of same class as `x`. Ignored if `x` is of `class` `data.frame/tibble::tibble`. See \emph{Details} for the expected format.
#' @param metadata meta data corresponding to each entity. If `x` is a `list` of proxy data, `metadata` has to be of class `data.frame/tibble::tibble`. Ignored if `x` is of class `data.frame/tibble::tibble`.
#' @param zoo_format format of the object containing the proxy data, can be 'zoo' or 'Proxyzoo'. See `vignette('zoo-support-sdm')` for details.
#' 
#' @section Details:
#' For input format `data.frame`/`tibble::tibble` the `data.frame`/`tibble::tibble` is expected to hold all metadata as plain rows and corresponding proxy data, age data, and age summary as nested `data.frame`/`tibble::tibble`. See \emph{Examples}.
#' The column names holding proxy, age, and age summary data can be specified in the package conbfiguration. Per default, they are `proxydata`, `agedata`, `agesummdata`.
#' Mandatory metadata entries are (in default configuration of `PTBoxProxydata`): INSERT
#' If agesummary is provided, these columns are mandatory: mean_age, q25, q50, q75, giving the mean and the 25th, 50th, and 75th quantile of the age model ensemble. If not provided, they are computed based on `agedata`. Additinal `agesumm` statistics, like error ranges, can be provided.
#' 
#' For input format `matrix`, `x` is expected to be a list of `matrix` objects which hold the proxy data as columns and correspond to the `metadata`. Analogous formats are expected for `agedata` and `agesumm` if provided. The foremat of `metadata` is expected to be a named list, with list names corresponding to the metadata names (as above) and vector entries holding the metadata that corresponds to the data passed as matrix lists. See \emph{Examples}.
#' 
#' @return 
#' @export
as_Proxytibble <- function(x, ...) UseMethod("as_Proxytibble")

#' @export
as_Proxytibble.data.frame <- function(x, zoo_format = 'zoo') {
    if (Proxytibble_class[1] %in% class(x)) {
        return(x)
    } else if (has_format_Proxytibble(x)) {
        tb <- set_Proxytibble_class(x)
    } else {
        tb <- as_Proxytibble.list(x = lapply(x[[Proxytibble_colnames_proxy_data()]], as.matrix),
                                  metadata = x %>% dplyr::select(-tidyselect::any_of(c(Proxytibble_colnames_proxy_data(), Proxyzoo_agedata(), Proxyzoo_agesumm(), Proxyzoo_depth()))), # important: cannot use tidyselect::all_of(Proxytibble_colnames_ent() cause that would crash non-mandatory metadata
                                  agedata = { if (is.null(x[[Proxyzoo_agedata()]])) NULL else lapply(x[[Proxyzoo_agedata()]], as.matrix) },
                                  agesumm = { if (is.null(x[[Proxyzoo_agesumm()]])) NULL else lapply(x[[Proxyzoo_agesumm()]], as.matrix) },
                                  depth = { if (is.null(x[[Proxyzoo_depth()]])) NULL else lapply(x[[Proxyzoo_depth()]], as.matrix) },
                                  zoo_format = zoo_format)
    }
    return(tb)
}

#' @export
as_Proxytibble.list <- function(x, metadata, agedata = NULL, agesumm = NULL, depth = NULL, zoo_format = 'zoo') {
    # class checks (use only first object here)
    if (!is.null(agedata) & !any(class(agedata[[1]]) %in% c('matrix', 'data.frame', 'tibble', "numeric"))) stop("`agedata` has to be of class `matrix` for `class(x) == 'matrix'")
    if (!is.null(agesumm) & !any(class(agesumm[[1]]) %in% c('matrix', 'data.frame', 'tibble', "numeric"))) stop("`agesumm` has to be of class `matrix` for `class(x) == 'matrix'")
    if (!is.null(depth) & !any(class(depth[[1]]) %in% c('matrix', 'data.frame', 'tibble', "numeric"))) stop("`depth` has to be of class `matrix` for `class(x) == 'matrix'")
    
    # data format checks
    if (!all(Proxytibble_colnames_ent() %in% colnames(metadata))) stop(paste0('Not all mandatory entity metadata provided to `as_Proxytibble()`.\nMandatory entity metadata are:',
                                                                            paste(Proxytibble_colnames_ent(), collapse = ', ')))
    if (!is.null(agesumm) & !is.null(agedata)) if (all.equal(rep(length(x), 3), c(dim(metadata)[1], length(agedata), length(agesumm))) != T) stop('lengths of proxydata, metadata, agedata, agesumm do not match')
    if (is.null(agesumm) & !is.null(agedata)) if (all.equal(rep(length(x), 2), c(dim(metadata)[1], length(agedata))) != T) stop('lengths of proxydata, metadata, agedata do not match')
    if (!is.null(agesumm) & !is.null(agedata)) if (length(x) != dim(metadata)[1]) stop('lengths of proxydata and metadata do not match')

    
    # build up of Proxytibble
    ## metadata
    tb <- metadata %>% 
        tibble::as_tibble()
    ## proxy names
    if (! any(names(metadata) == "proxy_name")) {
        tb <- tb %>% dplyr::bind_cols(tibble::tibble(!!Proxytibble_colnames_proxy_names() := lapply(x, colnames)))
    }
    ## proxy unit 
    ### has to provide as list of character vectors if multivariate
    ### but to keep type consistency for univariate case, nest proxy unit in this case
    if (class(tb$proxy_unit) == 'character' & length(tb$proxy_unit)) { # ensure this is the univariate case
        tb$proxy_unit <- as.list(tb$proxy_unit)
    } else if (class(tb$proxy_unit) != 'list') {
        stop('need to provide `proxy_unit` as character for univariate and as list of character vectors for multivariate data')
    }
    
    ## paleo data (`zoo::zoo` or `PTBoxProxydata::Proxyzoo` objects)
    if (zoo_format == 'zoo') {
        tb <- tb %>% 
            dplyr::bind_cols(tibble::tibble(!!Proxytibble_colnames_proxy_data() := lapply(1:length(x), 
                                                                                          function(i) { # prevent zoo::zoo from recycling data without warning message
                                                                                              if (!is.null(agedata[[i]][,1]) & length(as.matrix(agedata[[i]][,1])) != length(as.matrix(x[[i]])[,1])) {
                                                                                                  stop("lengths of proxydata and agedata don't match when creating zoo::zoo")
                                                                                              }
                                                                                              zoo::zoo(x[[i]],
                                                                                                       order.by = as.matrix(agedata[[i]])[,1])
                                                                                          }))) %>%
            set_Proxytibble_class(.)
    } else if (zoo_format == 'Proxyzoo') {
        tb <- tb %>% 
            dplyr::bind_cols(tibble::tibble(!!Proxytibble_colnames_proxy_data() := lapply(1:length(x), # don't need a length check here, because Proxyzoo is doing this itself
                                                                                          function(i) Proxyzoo(proxydata = {if(is.null(x[[i]])) NULL else as.matrix(x[[i]])}, 
                                                                                                               agedata = {if(is.null(agedata[[i]])) NULL else as.matrix(agedata[[i]])},
                                                                                                               agesumm = {if(is.null(agesumm[[i]])) NULL else as.matrix(agesumm[[i]])},
                                                                                                               depth = {if(is.null(depth[[i]])) NULL else as.matrix(depth[[i]])} )))) %>% 
            set_Proxytibble_class(.)
    } else {
        stop('unsupported `zoo_format`')
    }
    return(tb)
}

#' Apply a function to all proxy data in a Proxytibble
#'
#' @param x `PTBoxProxydata::Proxytibble`
#' @param fun function
#' @param col column containing proxy data (in `zoo::zoo` or `PTBoxProxydata::Proxyzoo` format) to apply `fun` to
#' @param ... arguments passed on to `fun`
#'
#' @return
#' @export
apply_proxy <- function(x, fun, ...) UseMethod("apply_proxy")

#' @export
apply_proxy.Proxytibble <- function(x, fun, col = Proxytibble_colnames_proxy_data(), ...) {
    x[[col]] <- lapply(x[[col]], fun, ...)
    return(x)
}


#' Update the `proxy_name` and `proxy_unit` column of a `Proxytibble` to (re-)match the `proxy_data` column
#'
#' @param x `PTBoxProxydata::Proxytibble`
#' @param fun function
#' @param dcol data column containing proxy data (in `zoo::zoo` or `PTBoxProxydata::Proxyzoo` format) to apply `fun` to
#' @param ncol data column containing proxy name(s)
#' @param ucol data column containing proxy unit(s)
#'
#' @return
#' @export 
update_proxy_names <- function(x, ...) UseMethod("update_proxy_names")

#' @export
update_proxy_names.Proxytibble <- function(x, dcol = Proxytibble_colnames_proxy_data(), ncol = 'proxy_name', ucol = 'proxy_unit') {
    lapply(
        1:nrow(x),
        function(i) {
            loc_mask <- (x[[ncol]][[i]] %in% colnames(x[[dcol]][[i]]))
            x[[ncol]][[i]] <<- x[[ncol]][[i]][loc_mask]
            x[[ucol]][[i]] <<- x[[ucol]][[i]][loc_mask]
        }
    )
    return(x)
}
