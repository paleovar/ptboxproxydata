% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProxyDataManager.R
\name{ProxyDataManager}
\alias{ProxyDataManager}
\title{ProxyDataManager S3 class}
\usage{
ProxyDataManager(
  master_sheet = get("CONFIG", envir = .PTBoxProxydata)$master$path
)
}
\arguments{
\item{master_sheet}{character file path of the Master Sheet}
}
\value{

}
\description{
ProxyDataManager S3 class
}
\examples{
library(PTBoxProxydata)
mng <- ProxyDataManager()
acer_ap <- load_set(mng, 
                    'acer_ap_mfa',
                    'tibble',
                    FALSE,
                    TRUE)
}
