
<!-- README.md is generated from README.Rmd. Please edit that file -->
The malaria elimination prediction market
=========================================

*Aligning incentives, reducing uncertainty, and increasing transparency through a marketplace of ideas approach*

Introduction
------------

**The problem**: When it comes to malaria elimination, policy-makers are faced with a difficult decision: should they divert resources from other important health areas (HIV/AIDS, Tuberculosis, etc.) to malaria elimination, since the "return on investment" would be so great in the *long term*, or should they spread resources around so as to minimalize morbidity and mortality in the *short term*?

**The solution**: To answer this question, it is necessary to *quantify* the probability of and timeframe to achieving elimination, since the "expected value" of any investment requires some notion of likelihood and discount. for the purposes of quantifying likelihood of and time to country-specific malaria elimination, we propose a prediction market, a platform in which users "buy and sell ‘shares’ in a future event at a price that reflects their collective wisdom about the chance of the event happening" (Mann 2016). The concept has been applied to sports and politics, and have even started to make a foirée into science (Dreber et al. 2015, Almenberg, Kittlitz, and Pfeiffer (2009)).

Methods
-------

We design, create, and deploy a fully functioning malaria elimination prediction market prototype. We publish open-source code (so others can copy and improve on our approach) and recruit a cohort of initial volunteers to test the platform.

Discussion
----------

This is a work-in-progress (currently in the software development and testing phase).

Details
=======

### Funding

This research is for Joe Brew's PhD. Full details at [www.economicsofmalaria.com](http://economicsofmalaria.com) He is generously funded by the Erasmus Mundus Joint Doctorate Fellowship, Specific Grant Agreement 2016-1346. His program of study is the [Transdisciplinary Global Health programme](http://www.transglobalhealth.org/).

This repository
===============

This code repository serves as the "research compendium" for the "Malaria elimination prediction market", a project which aims to build, test, and roll out a prediction market in which participants wager on the local elimination and global eradication of malaria. The purpose of this repository is "to integrate the computations and code used in data analyses, methodological descriptions, simulations, etc. with the documents that describe and rely on them" ([Gentleman and Lang, 2004](http://biostats.bepress.com/bioconductor/paper2/)).

This project is formatted as an R package. It can be downloaded and installed by running the following from the R console:

``` r
devtools::install_github('joebrew/malariaprediction')
```

References
==========

Almenberg, Johan, Ken Kittlitz, and Thomas Pfeiffer. 2009. “An Experiment on Prediction Markets in Science.” Edited by Joel M.Editor Schnur. *PLoS ONE* 4 (12). Public Library of Science (PLoS): e8500. doi:[10.1371/journal.pone.0008500](https://doi.org/10.1371/journal.pone.0008500).

Dreber, Anna, Thomas Pfeiffer, Johan Almenberg, Siri Isaksson, Brad Wilson, Yiling Chen, Brian A. Nosek, and Magnus Johannesson. 2015. “Using Prediction Markets to Estimate the Reproducibility of Scientific Research.” *Proceedings of the National Academy of Sciences* 112 (50). Proceedings of the National Academy of Sciences: 15343–7. doi:[10.1073/pnas.1516179112](https://doi.org/10.1073/pnas.1516179112).

Mann, Adam. 2016. “The Power of Prediction Markets.” *Nature* 538 (7625). Springer Nature: 308–10. doi:[10.1038/538308a](https://doi.org/10.1038/538308a).
