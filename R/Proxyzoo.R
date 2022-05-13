# class definition
Proxyzoo_class <- c('Proxyzoo', 'tbl', 'tbl_df', 'data.frame')

#' Dynamic column names for the Proxyzoo
#' 
#' Column names as specified in `PTBoxProxydata-config.yml`
#' and read from config in `hooks.R` on attachment/loading of the package.
#' 
#' @details 
#' \code{Proxyzoo_proxydata()} # default `proxydata`
#' 
#' \code{Proxyzoo_agedata()} # default `agedata`
#' 
#' \code{Proxyzoo_agesumm()} # default `agesumm`
#' 
#' \code{Proxyzoo_depth()} # default `depth`
#' 
#' @export
Proxyzoo_proxydata <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxyzoo$proxydata_name
#' @rdname Proxyzoo_proxydata
#' @export
Proxyzoo_agedata <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxyzoo$agedata_name
#' @rdname Proxyzoo_proxydata
#' @export
Proxyzoo_agesumm <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxyzoo$agesummary_name
#' @rdname Proxyzoo_proxydata
#' @export
Proxyzoo_depth <- function() get("CONFIG", envir = .PTBoxProxydata)$Proxyzoo$depth_name

Proxyzoo_agesumm_stats <- c('mean_age', 'q25', 'q50', 'q75')


#' Proxyzoo S3 class for universal handling of proxy data, including age model ensembles
#' 
#' @param ... passed to \link{as_Proxyzoo.zoo}
#'
#' @details 
#' See `vignette('zoo-support-PTBoxProxydata')` for details.
#'
#' @return object of class `Proxyzoo`. Empty if no data supplied via `...`.
#' @export
Proxyzoo <- function(proxydata = NULL, agedata = NULL, agesumm = NULL, depth = NULL) {
    # do checks on data
    if (!is.null(proxydata) & all(class(proxydata) != "matrix")) stop("class(proxydata) is not `matrix`")
    if (!is.null(agedata) & all(class(agedata) != "matrix")) stop("class(agedata) is not `matrix`")
    if (!is.null(agesumm) & all(class(agesumm) != "matrix")) stop("class(agesumm) is not `matrix`")
    if (!is.null(depth) & all(class(depth) != "matrix")) stop("class(depth) is not `matrix`")
    
    # Create and assign Proxyzoo class
    z <- tibble::tibble()
    
    # Initialize fields of Proxyzoo class
    if (is.null(proxydata) & is.null(agedata) & is.null(depth)) { # no data passed
        z <- tibble::tibble(!!Proxyzoo_proxydata() := matrix(data = numeric()), 
                            !!Proxyzoo_agedata() := matrix(data = numeric()), 
                            !!Proxyzoo_agesumm() := matrix(data = numeric()),
                            !!Proxyzoo_depth() := matrix(data = numeric())) %>% 
            set_Proxyzoo_class()%>% 
            comp_agesumm()
    } else if (!is.null(proxydata) & is.null(agedata) & is.null(depth)) {
        if (!is.null(agesumm)) warning("no agedata provided, using default agesumm instead of given one")
        z <- tibble::tibble(!!Proxyzoo_proxydata() := proxydata, 
                            !!Proxyzoo_agedata() := matrix(data = 1:(dim(proxydata)[1])), 
                            !!Proxyzoo_agesumm() := matrix(data = rep(NA_real_, dim(proxydata)[1])),
                            !!Proxyzoo_depth() := matrix(data = rep(NA_real_, dim(proxydata)[1]))) %>% 
            set_Proxyzoo_class() %>% 
            comp_agesumm()
    } else if (is.null(proxydata) & !is.null(agedata)) {
        stop("must provide proxydata with non-NULL agedata to `Proxyzoo()`")
    } else if (is.null(proxydata) & !is.null(depth)) {
        stop("must provide proxydata with non-NULL depth to `Proxyzoo()`")
    } else if (is.null(agesumm) & is.null(depth)) {
        if (dim(proxydata)[1] != dim(agedata)[1]) stop('dim(proxydata)[1] has to match dim(agedata)[1]')
        z <- tibble::tibble(!!Proxyzoo_proxydata() := proxydata, 
                            !!Proxyzoo_agedata() := agedata, 
                            !!Proxyzoo_agesumm() := matrix(data = rep(NA_real_, dim(proxydata)[1])),
                            !!Proxyzoo_depth() := matrix(data = rep(NA_real_, dim(proxydata)[1]))) %>% 
            set_Proxyzoo_class() %>% 
            comp_agesumm()
    } else if (is.null(agesumm) & !is.null(depth)) {
        if (!all.equal(dim(proxydata)[1], dim(agedata)[1], dim(depth)[1], rep(dim(proxydata)[1], 3))) {
            stop('dim(proxydata)[1], dim(agedata)[1], dim(depth)[1] have to match')
        }
        z <- tibble::tibble(!!Proxyzoo_proxydata() := proxydata, 
                            !!Proxyzoo_agedata() := agedata, 
                            !!Proxyzoo_agesumm() := matrix(data = rep(NA_real_, dim(proxydata)[1])),
                            !!Proxyzoo_depth() := depth) %>% 
            set_Proxyzoo_class()%>% 
            comp_agesumm()
    } else if (!is.null(agesumm) & is.null(depth)) {
        if (!all.equal(dim(proxydata)[1], dim(agedata)[1], dim(agesumm)[1], rep(dim(proxydata)[1], 3))) {
            stop('dim(proxydata)[1], dim(agedata)[1], dim(agesumm)[1] have to match')
        }
        z <- tibble::tibble(!!Proxyzoo_proxydata() := proxydata, 
                            !!Proxyzoo_agedata() := agedata, 
                            !!Proxyzoo_agesumm() := agesumm,
                            !!Proxyzoo_depth() := matrix(data = rep(NA_real_, dim(proxydata)[1]))) %>% 
            set_Proxyzoo_class()%>% 
            comp_agesumm()
    } else {
        if (!all.equal(dim(proxydata)[1], dim(agedata)[1], dim(agesumm)[1], dim(depth)[1], rep(dim(proxydata)[1], 3))) {
            stop('dim(proxydata)[1], dim(agedata)[1], dim(agesumm)[1], dim(depth)[1] have to match')
        }
        z <- tibble::tibble(!!Proxyzoo_proxydata() := proxydata, 
                            !!Proxyzoo_agedata() := agedata, 
                            !!Proxyzoo_agesumm() := agesumm,
                            !!Proxyzoo_depth() := depth) %>% 
            set_Proxyzoo_class()%>% 
            comp_agesumm()
    }
    return(z)
}


set_Proxyzoo_class <- function(x, class = NULL) {
    class(x) <- c(setdiff(class, Proxyzoo_class), Proxyzoo_class)
    return(x)
}


comp_agesumm <- function(x) UseMethod('comp_agesumm')

comp_agesumm.Proxyzoo <- function(x) {
    if (dim(agedata.Proxyzoo(x))[2] > 1) {
        a <- matrix(
            c(agedata.Proxyzoo(x) %>% 
                  apply(., 1, mean, na.rm = TRUE),
              agedata.Proxyzoo(x) %>% 
                  apply(., 1, quantile, probs = 0.25, na.rm = TRUE),
              agedata.Proxyzoo(x) %>% 
                  apply(., 1, quantile, probs = 0.5, na.rm = TRUE),
              agedata.Proxyzoo(x) %>% 
                  apply(., 1, quantile, probs = 0.75, na.rm = TRUE)
            ),
            ncol = 4
        )
    } else {
        a <- matrix(
            c(agedata.Proxyzoo(x)[,1],
              rep(NA_real_,length(agedata.Proxyzoo(x)[,1])),
              rep(NA_real_,length(agedata.Proxyzoo(x)[,1])),
              rep(NA_real_,length(agedata.Proxyzoo(x)[,1]))
            ),
            ncol = 4
        )
    }
    colnames(a) <- Proxyzoo_agesumm_stats
    agesumm(x) <- a
    return(x)
}


#' Extract and assign agedata of Proxyzoo
#'
#' @param x 
#'
#' @return
#' @export
agedata <- function(x) UseMethod('agedata')

#' @export
agedata.Proxyzoo <- function(x) {
    return(x[[Proxyzoo_agedata()]])
}


#' Extract and assign proxydata of Proxyzoo
#'
#' @param x 
#'
#' @return
#' @export
proxydata <- function(x) UseMethod('proxydata')

#' @export
proxydata.Proxyzoo <- function(x) {
    return(x[[Proxyzoo_proxydata()]])
}

#' Extract and assign age summary of Proxyzoo
#'
#' @param x 
#'
#' @return
#' @export
agesumm <- function(x) UseMethod('agesumm')

#' @export
agesumm.Proxyzoo <- function(x) {
    return(x[[Proxyzoo_agesumm()]])
}

#' Extract and assign depth of Proxyzoo
#'
#' @param x 
#'
#' @return
#' @export
depth <- function(x) UseMethod('depth')

#' @export
depth.Proxyzoo <- function(x) {
    return(x[[Proxyzoo_depth()]])
}


#' @rdname agedata
#' @export
"agedata<-" <- function(x, value) UseMethod('agedata<-')

#' @export
"agedata<-.Proxyzoo" <- function(x, value) {
    x[[Proxyzoo_agedata()]] <- value
    return(x)
}


#' @rdname proxydata
#' @export
"proxydata<-" <- function(x, value) UseMethod('proxydata<-')

#' @export
"proxydata<-.Proxyzoo" <- function(x, value) {
    x[[Proxyzoo_proxydata()]] <- value
    return(x)
}


#' @rdname agesumm
#' @export
"agesumm<-" <- function(x, value) UseMethod('agesumm<-')

#' @export
"agesumm<-.Proxyzoo" <- function(x, value) {
    x[[Proxyzoo_agesumm()]] <- value
    return(x)
}


#' @rdname depth
#' @export
"depth<-" <- function(x, value) UseMethod('depth<-')

#' @export
"depth<-.Proxyzoo" <- function(x, value) {
    x[[Proxyzoo_depth()]] <- value
    return(x)
}


#' Convert one (two) zoo::zoo objects to one PTBoxProxydata::Proxyzoo
#'
#' @param x `zoo::zoo` object
#' @param ... passed to `as_Proxyzoo.zoo`
#' 
#' @return 
#' @export
as_Proxyzoo <- function(x, ...) UseMethod('as_Proxyzoo')


#' Convert one (two) zoo::zoo objects to one PTBoxProxydata::Proxyzoo
#'
#' @param proxyzoo `zoo::zoo`, contains proxy data (see *Details* for expected formatting)
#' @param agezoo `zoo::zoo`, optional contains age/age model data (see *Details* for expected formatting)
#' @param indexcol 
#' 
#' @details 
#' Expects proxy data `proxyzoo` either with a column named `depth` or `age` which can be used as an index 
#' (unless names are specified otherwise in the configuration of the `PTBoxProxydata` package), or tries to use the first column
#' of the `proxyzoo` `zoo::zoo` object. If `zoo::zoo` object `agezoo` is supplied additionally, expects 
#' matching `depth`, `age` or first indexing column.
#' 
#' If both `depth` and `age` are contained in `proxyzoo`, `depth` will be used 
#' as *unambiguous* index, and `age` will be included in `Proxyzoo$agedata`. 
#' 
#' If only `age` is provided as index,
#' this *can* (does not have to) lead to *ambigious* indexing of the data in case that a hiatus is present in the data.
#' 
#' If none of `depth` or `age` are provided, returned `Proxyzoo$agedata` will only consist of an `$index`/`index()` column
#' linking to `Proxyzoo$agedata` (see \link{Proxyzoo} for shape of `PTBoxProxydata::Proxyzoo`).
#' 
#'
#' @return object of `PTBoxProxydata::Proxyzoo` (see *Details* for return formatting, \link{Proxyzoo} for shape of `PTBoxProxydata::Proxyzoo`)
#' @export
as_Proxyzoo.zoo <- function(proxyzoo, agezoo = NULL, indexcol = 'index') {
    # initial checks
    if (!(indexcol %in% zoo_STACY_indexcols())) stop(paste0('indexcol has to be one of ', paste(zoo_STACY_indexcols(), collapse = ',')))
    if (any(class(proxyzoo) == 'zoo_STACY')) warning('class of proxyzoo is already `zoo_STACY`. Returning `proxyzoo` untouched.')
    if (all(class(proxyzoo) != 'zoo')) stop(paste0('class of proxyzoo has to be `zoo`, not ', class(proxyzoo)))
    if (!is.null(agezoo)) if (class(agezoo) != 'zoo') stop(paste0('class of agezoo has to be `zoo`, not ', class(agezoo)))
    
    z <- zoo_STACY()
    
    if (!is.null(agezoo)) {
        if (any(zoo::index(proxyzoo) != zoo::index(agezoo))) stop('index() of proxyzoo and agezoo does not match')
        proxydata(z) <- proxyzoo
        agedata(z) <- agezoo
    } else {
        proxydata(z) <- proxyzoo
        agedata(z) <- zoo::zoo(order.by = zoo::index(proxyzoo))
    }
    indexcol(z) <- indexcol
    
    return(z)
}


#' Apply a function to all proxy data contained in a zoo::zoo (univariate or multivariate)
#' while maintaining the dimension/length of the zoo
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
zoo_applyfix <- function(x, ...) UseMethod('zoo_applyfix')


#' Apply a function to multivariate zoo::zoo
#' @param x 
#' @param fun Function operating on a zoo
#' @param ... 
#'
#' @return
#' @export
zoo_applyfix.zoo <- function(x, fun, ...) {
    if (is.null(dim(x))) {
        x <- fun(x, ...) # cover vector case
    } else {
        sapply(1:dim(x)[2],function(i) {
            x[,i] <<- fun(x[,i], ...)
        })   
    }
    # x <- apply(x, 2, fun) # appears to be a little slower than solution above
    return(x)
}


#' Apply a function to data contained in a PTBoxProxydata::Proxyzoo (proxydata, agedata or both)
#'
#' @param x `Proxyzoo` object
#' @param fun function to apply on the data (has to expect and return a `zoo::zoo` object. For other input/output type see `Proxyzoo_apply` (tb implemeted))
#' @param case one of c('nmean', 'meanm', 'n1', '1m', 'oneone', 'nm'), see \emph{Details}
#' @param colsel integer, only effective for cases `m` or `n`
#' @param seed integer, only effective for case `oneone`, `NULL` turns off randomization (see \emph{Return} for `case = oneone`)
#' @param recycle logical, only effective for case `oneone`
#' @param ... named arguments that are passed to fun
#' 
#' @details 
#' Cases for combining proxydata (`n` `proxy_name`s) and agedata (`m` age models):
#' \itemize{
#' \item{`nmean`: }{apply fun on a all `proxy_name`s columns combined with `mean()` across all `m` age models (dim 2 of `agedata()`)}
#' \item{`meanm`: }{apply fun on a all age models columns combined with `mean()` across all `proxy_name` columns (dim 2 of `proxydata()`)}
#' \item{`n`: }{as `nmean` but for combination with `colsel` number of the age models (if available) }
#' \item{`m`: }{as `meanm` but for combination with `colsel` number of the proxy data column (if available)}
#' \item{`oneone`: }{apply function on one-to-one combinations of `proxy_name` columns from `proxydata()` and age models from `agedata()`. `seed` can be specified to guarantee reproducibility of combination. `recycle = priom` recycles age models as long as all `proxy_name` columns have been matched, `recycle = prion` recycles `proxy_name` columns as long as all age models have been matched, `unique` stops combining as soon as either all `proxy_name` columns or age models have been sampled without replacement.}
#' \item{`nm`: }{apply function across all possible combinations of `proxy_name` columns and age model in `agedata()`}
#' }
#' 
#' See \emph{Value} for returns.
#'
#' @return
#' Return cases:
#' \itemize{
#' \item{`nmean`: }{`Proxyzoo` with `proxydata()` as input and univariate `agedata()` of mean age of age models}
#' \item{`meanm`: }{`Proxyzoo` with with multivariate (!!) `proxydata()` as result of combination of mean of `proxy_name` colunms and age models as in input}
#' \item{`n`: }{as `nmean` but with `agedata()` as specified by `colsel`}
#' \item{`m`: }{`Proxyzoo` with multivariate (!!) `proxydata()`, recycling of one and the same `proxy_name` column for all age models}
#' \item{`oneone`: }{depending on `recycle`, either mxm or nxn `Proxyzoo` with column order corresponding to how `proxy_name` and age models have been combined in the processing. This is why it is not recommended to use a sequence of `prion` or `priom`. Instead initial calls to `priom` or `prion` are normally follwed only by further `unique` calls with `seed = NULL` (no randomization)}
#' \item{`nm`: }{to be determined, maybe list of `Proxyzoo`s?}
#' }
#' 
#' See \emph{Details} for possible combinations.
#' 
#' @section Note: 
#' Outputs do not necessarily maintain the same dimensions as inputs (a) because of the different `case`s and (b) because some zoo methods (e.g. interpolation) result in changed time axes that are propagated into the output `Proxyzoo`.
#' 
#' @export
zoo_applyfix.Proxyzoo <- function(x, fun,
                               case = 'nmean', colsel = 1, seed = 0, recycle = 'unique', ...) {
    cases <- c('nmean', 'meanm', 'n', 'm', 'oneone', 'nm')
    recycles <- c('prion', 'priom', 'unique')
    if (!(case %in% cases)) stop(paste0('unknown `case`: ', case))
    if (!(recycle %in% recycles)) stop(paste0('unknown `recycle`: ', recycle))
    
    if (case == 'nmean') {
        # create intermediate zoo
        izoo <- zoo::zoo(proxydata(x),
                         order.by = agesumm(x)[,"mean_age"]) %>% 
            zoo_apply.zoo(x = ., fun = fun, ... = ...)
        # important: don't use agesumm(x)[,"mean"] here, because lenght of output could be different
        # (e.g. temporal interpolation)
        ret <- Proxyzoo(proxydata = zoo::coredata(izoo), 
                        agedata = as.matrix(zoo::index(izoo)))
    } else if(case == 'meanm') {
        mn_proxy <- apply(proxydata(x), 1, mean, na.rm = TRUE)
        izoos <- lapply(1:dim(agedata(x))[2], function(xx) {
            izoo <- zoo::zoo(mn_proxy,
                             order.by = agedata(x)[,xx]) %>% 
                zoo_apply.zoo(x = ., fun = fun, ... = ...) 
        })
        ret <- Proxyzoo(proxydata = do.call("cbind", lapply(izoos, function(xx) zoo::coredata(xx))), 
                        agedata = do.call("cbind", lapply(izoos, function(xx) zoo::index(xx))))
    } else if (case == 'n') {
        izoo <- zoo::zoo(proxydata(x),
                         order.by = agedata(x)[,colsel]) %>% 
            zoo_apply.zoo(x = ., fun = fun, ... = ...)
        ret <- Proxyzoo(proxydata = zoo::coredata(izoo), 
                        agedata = as.matrix(zoo::index(izoo)))
    } else if (case == 'm') {
        sel_proxy <- proxydata(x)[,colsel]
        izoos <- lapply(1:dim(agedata(x))[2], function(xx) {
            izoo <- zoo::zoo(sel_proxy,
                             order.by = agedata(x)[,xx]) %>% 
                zoo_apply.zoo(x = ., fun = fun, ... = ...) 
        })
        ret <- Proxyzoo(proxydata = do.call("cbind", lapply(izoos, function(xx) zoo::coredata(xx))), 
                        agedata = do.call("cbind", lapply(izoos, function(xx) zoo::index(xx))))
    } else {
        stop('cases other than `nmean` have not yet been implemented')
    }
    
    #if (!(field %in% c('proxydata', 'agedata', 'both'))) stop('`field` needs to be either `proxydata`, `agedata` or `both`')
    #
    #if (field %in% c('proxydata', 'both')) {
    #    x <- zoo_STACY_apply_proxydata(x, fun, ...)
    #}
    #if (field %in% c('agedata', 'both')) {
    #    x <- zoo_STACY_apply_agedata(x, fun, ...)
    #}
    
    return(ret)
}

#' Apply a function to all proxy data contained in a zoo::zoo (univariate or multivariate)
#' while allowing for changing the dimension of the underlying zoo (this is a less strict and
#' more generalized version of zoo_applyfix)
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
zoo_apply <- function(x, ...) UseMethod('zoo_apply')


#' Apply a function to multivariate zoo::zoo
#' @param x 
#' @param fun Function operating on a univariate zoo
#' @param ... 
#'
#' @return
#' @export
zoo_apply.zoo <- function(x, fun, out_index = NULL, ...) {
    if (is.null(dim(x))) {
        x <- zoo::zoo(matrix(zoo::coredata(x)), order.by = zoo::index(x)) # cover vector case
    }
    xout <- sapply(1:dim(x)[2],function(i) {
        nzoo <- fun(x[,i], ...)
        if (!is.null(dim(nzoo))) stop('`fun` returned object that is not univariate')
        if ("zoo" %in% class(nzoo)) {
            if(is.null(out_index)) {
                out_index <<- zoo::index(nzoo) 
            } else if("zoo" %in% class(nzoo) & !all(out_index == zoo::index(nzoo))) {
                stop('different indices returned by fun for subsets of multivariate zoo')
            }
        } else if(is.null(out_index)) {
            if (length(nzoo) == length(x[,i])) {
                out_index <<- zoo::index(x[,i])    
            } else {
                out_index <<- 1:length(nzoo)
            }
        } 
        return(nzoo)
    })
    # Handle special case of only 1 out index (which leads not xout not being a matrix at the end of the sapply)
    if (length(out_index) == 1) {
        xout <- matrix(xout,nrow = 1)
    }
    colnames(xout) <- colnames(x)
    xout <- zoo::zoo(xout, order.by = out_index)
    
    return(xout)
}

#zoo_STACY_apply_proxydata <- function(x, fun, ...) {
#    proxydata(x) <- zoo_apply.zoo(proxydata(x), fun, ...)
#    #proxydata(x) <- apply(X = proxydata(x), 
#    #                      MARGIN = 2,
#    #                      FUN = fun, 
#    #                      ... = ...)
#    return(x)
#}


#zoo_STACY_apply_agedata <- function(x, fun, ...) {
#    agedata(x) <- zoo_apply.zoo(agedata(x), fun, ...)
#    #agedata(x) <- apply(X = agedata(x), 
#    #                    MARGIN = 2,
#    #                    FUN = fun, 
#    #                    ... = ...)
#    return(x)
#}


#' Type conversion of Proxyzoo
#'
#' @param x 
#'
#' @return
#' @export
as_zoo <- function(x, ...) UseMethod('as_zoo')


#' @export
as_zoo.Proxyzoo <- function(x, field = 'proxydata') {
    if (!(field %in% c('proxydata', 'agedata'))) stop('`field` needs to be either `proxydata`, `agedata`')
    if (field == 'proxydata') return(proxydata(x)) else return(agedata(x))
}


#' @export
as_zoo.data.frame <- function(x, field = 'proxydata') {
    if (!(field %in% c('proxydata', 'agedata'))) stop('`field` needs to be either `proxydata`, `agedata`')
    if (field == 'proxydata') return(proxydata(x)) else return(agedata(x))
}


#' @export
as_zoo.zoo <- function(x, ...) {
    return(zoo::as.zoo(x, ...))
}

#' Conversion to zoo::zoo respecting PTBox conventions 
#'
#' @param proxyframe 
#' @param ... ignored
#'
#' @return
#' @export
as_zoo.data.frame <- function(proxyframe, ...) {
    # initial checks
    if (any(class(proxyframe) == 'zoo')) warning('class of proxyframe is already `zoo`. Returning `proxyframe` untouched.')
    if (!any(class(proxyframe) == 'data.frame')) stop(paste0('one class of proxyframe has to be `data.frame`, not ', class(proxyframe)))
    
    z <- as_zoo_data.frame_(proxyframe)
    return(z)
}


as_zoo_data.frame_ <- function(proxyframe) {
    z <- NULL
    
    if (Proxytibble_colnames_depth() %in% colnames(proxyframe)) {
        z <- zoo::zoo(x = dplyr::select(proxyframe, -!!dplyr::sym(Proxytibble_colnames_depth())),
                      order.by = proxyframe[[Proxytibble_colnames_depth()]])
    } else if (Proxytibble_colnames_age() %in% colnames(proxyframe)) {
        z <- zoo::zoo(x = dplyr::select(proxyframe, -!!dplyr::sym(Proxytibble_colnames_age())),
                      order.by = proxyframe[[Proxytibble_colnames_age()]])
    } else {
        z <- zoo::zoo(x = dplyr::select(proxyframe, -1),
                      order.by = proxyframe[,1])
    }
    
    return(z)
}


#' @rdname as_zoo
#'
#' @param x 
#'
#' @return
#' @export
as_data.frame <- function(x, ...) UseMethod('as_data.frame')


#' @export
#' @method as_data.frame zoo_STACY
as_data.frame.zoo_STACY <- function(x, field = 'proxydata') {
    if (!(field %in% c('proxydata', 'agedata'))) stop('`field` needs to be either `proxydata`, `agedata`')
    if (field == 'proxydata') return(as.data.frame(proxydata(x))) else return(as.data.frame(agedata(x)))
}

#' @export
#' @method as_data.frame zoo
as_data.frame.zoo <- function(x, ...) {
    return(as.data.frame(x, ...))
}


#' Select proxies by name
#'
#' @param x `zoo::zoo` or `PTBoxProxydata::zoo_STACY`
#' @param proxy_names character vector of proxy names
#'
#' @return
#' @export
select_proxies <- function(x, ...) UseMethod('select_proxies')


#' @export
select_proxies.zoo <- function(x, proxy_names) {
    proxy_names <- proxy_names[proxy_names %in% colnames(x)]
    return(x[, proxy_names, drop = F])
}

#' @export
select_proxies.Proxyzoo <- function(x, proxy_names) {
    stop('select_proxies not yet implemented for Proxyzoo')
}

##' export
#select_proxies.zoo_STACY <- function(x, proxy_names) {
#    proxydata(x) <- select_proxies.zoo(proxydata(x), proxy_names)
#    return(x)
#}

# as_zoo_STACY.tibble
#as_zoo_STACY.tibble <- function(tb) {
#    # expects tibble with columns 'age', 'proxy_name', 'proxy_unit', 'proxy_value' 
#    # and optionally 'age_min', 'age_max', 'proxy_min', 'proxy_max'
#    
#    # check provided columns
#    
#    # order columns
#    tb <- tb %>%
#        dplyr::relocate({ Proxytibble_colnames_age() }) # %>%  #, proxy_name, proxy_unit, proxy_value) %>% 
#        #tidyr::pivot_wider(names_from = proxy_name, values_from = proxy_value)
#       # dplyr::arrange({ Proxytibble_colnames_age() })# order by age
#    
#    # create zoo object (age and data in one object for now)
#    # would need to extend with sample id and two zoo objects 
#    # to include age uncertainties
#    tb <- tb[order(tb[[Proxytibble_colnames_age()]]),]
#    tbz <- zoo::as.zoo(dplyr::select(tb, -{ Proxytibble_colnames_age() }))
#    #temp <- tb[[Proxytibble_colnames_age()]][1:(length(tb[[Proxytibble_colnames_age()]])-1)]-tb[[Proxytibble_colnames_age()]][2:(length(tb[[Proxytibble_colnames_age()]]))]
#    #print(temp)
#    zoo::index(tbz) <- tb[[Proxytibble_colnames_age()]]
#    #tbz <- zoo::as.zoo(tb)
#    #print(tbz)
#    return(tbz)
#}
