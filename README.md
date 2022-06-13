
# Indirect effect of the COVID-19 pandemic on hospital mortality in hip fracture patients

#### Project Status: [In pgrogess]

## Project Description

Hip fractures are a leading cause of disability and death among older people. The COVID-19 pandemic led to rapid restructuring of orthopaedic care pathways in the NHS in England, in order to manage pressures on bed availability. This analysis aimed to investigate the indirect effect of the pandemic on hospital mortality among older hip fracture patients, admitted from cares homes or from the community. 

## Data source

We are using pseudonymised data on care home characteristics from the Care Quality Commission (similar to the publicly available [care directory on the CQC website](https://www.cqc.org.uk/files/cqc-care-directory-filters-1-april-2020)), linked to longituginal information on care home residents (Master Patient Index, MPI) and to hospital records from [Secondary Uses Service](https://digital.nhs.uk/services/secondary-uses-service-sus), a national administrative database of all inpatient admissions, A&E attendances and outpatient appointments funded by the NHS in England. Access to this data has been granted as the analysis is carried out under instruction from NHS England.

Using address information from monthly MPI extracts, care home residents can be
identified by assigning Unique Property Reference Numbers (UPRN) to patient addresses
and to addresses of care homes registered with the Care Quality Commission (CQC) and
comparing them.All processing of address information, and subsequent linkage of patient
information, was carried out by the National Commissioning Data Repository (NCDR)
and the analysis of the linked dataset used ‘pseudonymised’ information in a secure
environment hosted by the Health Foundation. 

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data will be accessed in The Health Foundation's Secure Data Environment; a secure data analysis facility (accredited with the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual will be used.

## How does it work?

As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on similar datasets.  

### Requirements

These scripts were written under R version version 4.0.3.
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/) (1.3.0)
* [**tidylog**](https://cran.r-project.org/web/packages/tidylog/index.html) (0.2.0)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)
* [**data.table**](https://cran.r-project.org/web/packages/data.table/index.html) (1.12.2)
* [**rlang**](https://cran.r-project.org/web/packages/rlang/index.html) (0.4.0)
* [**readxl**](https://cran.r-project.org/web/packages/readxl/index.html) 
* [**ISOweek**](https://cran.r-project.org/web/packages/ISOweek/index.html)
* [**tableone**](https://cran.r-project.org/web/packages/tableone/index.html)
* [**zoo**](https://cran.r-project.org/web/packages/zoo/index.html) 
* [**survival**](https://cran.r-project.org/web/packages/survival/index.html)
* [**survminer**](https://cran.r-project.org/web/packages/survminer/index.html)
* [**gridExtra**](https://cran.r-project.org/web/packages/gridExtra/index.html)
* [**Publish**](https://cran.r-project.org/web/packages/Publish/index.html)
* [**ggdag**](https://cran.r-project.org/web/packages/ggdag/index.html)
* [**RColorBrewer**](https://cran.r-project.org/web/packages/RColorBrewer/index.html)

### Getting started
* 01_clean_CQC.R - cleans pseudonimysed care home characteristics
* 02_clean_MPI.R - cleaning pseudonimysed master patient index for care home residents and their long-term conditions
* 03_admissions.R - process and filter hospital admissions
* 04_hipfracture_cohort.R - define study cohorts, descriptive analysis of admitted patients
* 05_hipfracture_outcomes_crude.R - calculate crude hospital mortality
* 06_models.R - competing risk survival modelling
* 07_subgroup.R - sensitivity analysis
* DAG.Rmd - directed acyclic graphs showing known causes of confounding when comparing hospital mortality risk across study populations from  before and during the pandemic
* functions.R - utility functions

## Useful references
* Improvement Analytics Unit briefing. [Emergency admissions to hospital from care homes: how often and what for?](http://www.scie-socialcareonline.org.uk/emergency-admissions-to-hospital-from-care-homes-how-often-and-what-for/r/a110f00000THg3xAAD) 2019.
* Grimm F, Hodgson K, Brine R, Deeny SR. [Hospital admissions from care homes in England during the COVID-19 pandemic: a retrospective, cross-sectional analysis using linked administrative data.](https://ijpds.org/article/view/1663) Interntational J Popul Data Sci. 2021;5(4). doi:10.23889/ijpds.v5i4.1663

## Authors
* **Fiona Grimm** - on [Twitter](https://twitter.com/fiona_grimm) or [GitHub](https://github.com/fiona-grimm)
* **Richard Brine** - on [GitHub](https://github.com/richardbrine)

## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/COVID19_carehome_hipfrac_mortality/blob/master/LICENSE).

## Acknowledgments

