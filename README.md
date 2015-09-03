<!-- README.md is generated from README.Rmd. Please edit that file -->
ocrquality
----------

An R package for measuring OCR quality

**Author:** [Lincoln Mullen](http://lincolnmullen.com)<br> **License:** [MIT](http://opensource.org/licenses/MIT)<br> **Status:** In development

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ocrquality)](http://cran.r-project.org/package=ocrquality) [![CRAN\_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ocrquality)](http://cran.r-project.org/package=ocrquality)

### Description

Measuring OCR rigorously is probably more effort than it is worth, if it can even be done properly. But sometimes you have a corpus, perhaps one for which you have done the OCR yourself, and need to check the reliability of the OCR to make sure that the texts are about the same quality. That's what this package is for. It provides a few quick-and-dirty methods of estimating the quality of OCR. These estimates do not rely on any ground truth, so they are not an absolute measure of the quality of the texts. But they do provide a relative measure within the corpus, so that you can detect texts which are significantly worse than others.