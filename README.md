# PTBoxProxydata - The data manager for paleo data time series 

This package provides the ProxyDataManager class and imposes conventions on format and processing of datasets used within the [PaleoToolBox (PTBox)](https://palmodapp.cloud.dkrz.de/) for a more consistent, efficient, and reusable paleodata workflow.

## Introduction

The package can be installed using `devtools` in [R](https://www.r-project.org/):

`require(devtools)` 

`devtools::install_git("https://github.com/paleovar/PTBoxProxydata", build_vignettes = TRUE)`

`library(PTBoxProxydata)` 

If you'd like to contribute to the package, you might want to clone it (type `git clone https://github.com/paleovar/PTBoxProxydata.git` in the terminal) and open the `.Rproj` in `RStudio`. Please don't hesitate to report issues, bugs, etc. to the authors.

## Introduction
After the installation run
```R
vignette('PTBoxProxydata_howto')
```
for the introductory tutorial and check out the help pages with
```R
?PTBoxProxydata
```

You can browse the vignette without installing the package as well. Therefore, simply download `doc/PTBoxProxydata-howto.html` from this repo and open the file in your browser.

## References

The water isotope data from the EPICA ice cores on Dronning Maud Land (DML) and at Dome C (DC) are taken from
B. Stenni et al.: The deuterium excess records of EPICA Dome C and Dronning Maud Land ice cores (East Antarctica), Quat. Sci. Rev., 29 (1-2), 146-159, https://doi.org/10.1016/j.quascirev.2009.10.009, 2010.

The pollen data from the Monticchio archive was first published by
Wulf, S., Kraml, M., Brauer, A., et al.: Tephrochronology of the 100 ka lacustrine sediment record of Lago Grande di Monticchio (southern Italy), Quatern. Int., 122, 7–30, https://doi.org/10.1016/j.quaint.2004.01.028, 2004.
The univariate arboreal pollen data is decribed in
Adam, M., Weitzel, N., and Rehfeld, K.: Identifying Global-Scale Patterns of Vegetation Change During the Last Deglaciation From Paleoclimate Networks, Paleoceanogr. Paleoclimatol., 36, 12, https://doi.org/10.1029/2021PA004265, 2021.
It builds on the harmonized ACER database by 
Sánchez Goñi, M. F., Desprat, S., Daniau, A.-L., et al.: The ACER pollen and charcoal database: a global resource to document vegetation and fire response to abrupt climate changes during the last glacial period, Earth Syst. Sci. Data, 9, 679–695, https://doi.org/10.5194/essd-9-679-2017, 2017.

## Responsible for this repository:
Developers: *[Moritz Adam](https://github.com/MoritzAdam), [Beatrice Ellerhoff](https://github.com/bellerhoff), [Jean-Philippe Baudouin](https://github.com/jeanphilippebaudouin) and [Nils Weitzel](https://github.com/nilsweitzel)*

Contributors to the [PTBox](https://palmodapp.cloud.dkrz.de/) framework (alphabetically): *Moritz Adam, Jean-Philippe Baudouin, Oliver Bothe, Manuel Chevalier, Beatrice Ellerhoff, Patrizia Schoch, Kira Rehfeld, Nils Weitzel*

## Acknowledgements

We thank the [R Core team](https://www.R-project.org/) and the developers of all packages that `PTBoxProxydata` buids on. Please see `citation()` for details on the R Core Team and `citation("packagename")` for details on the developers of individual packages.

The development of this package has been supported by the [PalMod](https://www.palmod.de/) project (subproject no. 01LP1926C). We thank all contributors and members of Palmod, the [Earth's climate and environmental dynamics](https://www.iup.uni-heidelberg.de/en/research/paleoclimate-dynamics) and the [SPACY](https://uni-tuebingen.de/climatology/) group for discussion at different stages of this package. 

*The Authors, April 2022*
