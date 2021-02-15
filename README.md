s4r
========

Installation
------------

## For use

In an R session:

```r
remotes::install_github("fdavidcl/s4r")
```

## For development

In a shell:

```
$ git clone https://github.com/fdavidcl/s4r.git
$ cd s4r
$ R
```

In the R session:

```r
remotes::install_github("fdavidcl/dcme")
remotes::install_bioc("graph")
remotes::install_bioc("Rgraphviz")
devtools::install_dev_deps()
```

## Dependencies

- Tensorflow (up to 2.2; 2.4 is not supported)
