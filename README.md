# Hospital mortality in hip fracture patients from care homes during the COVID-19 pandemic

#### Project Status: In progress

## Project Description

Hip fracture is a major cause of disability and death among frail older people.

This analysis examines the indirect effect of the pandemic on hospital mortality among hip fracture patients from care homes and the community, using a competing risk survival analysis approach. 

## Data sources

We are using pseudonymised data on care home characteristics from the Care Quality Commission (similar to the publicly available [care directory on the CQC website](https://www.cqc.org.uk/files/cqc-care-directory-filters-1-april-2020)), linked to longituginal information on care home residents (Master Patient Index, MPI) and to hospital records from [Secondary Uses Service](https://digital.nhs.uk/services/secondary-uses-service-sus), a national administrative database of NHS-funded hospital activity.
Access to this data has been granted as the analysis is carried out under instruction from NHS England.

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

These scripts were written under R version version 3.6.3.

The following R packages (available on CRAN) are needed: 
* [**tidyverse**](https://www.tidyverse.org/) 
* [**tidylog**](https://cran.r-project.org/web/packages/tidylog/index.html) 
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)
* [**data.table**](https://cran.r-project.org/web/packages/data.table/index.html) (1.12.2)
* [**broom**](https://cran.r-project.org/web/packages/broom/index.html) 
* [**RColorBrewer**](https://cran.r-project.org/web/packages/RColorBrewer/index.html)
* [**gridExtra**](https://cran.r-project.org/web/packages/gridExtra/index.html
* [**glmnet**](https://cran.r-project.org/web/packages/glmnet/index.html)
* [**survival**](https://cran.r-project.org/web/packages/survival/index.html)
* [**survminer**](https://cran.r-project.org/web/packages/survminer/index.html)
* [**Publish**](https://cran.r-project.org/web/packages/Publish/index.html)
* [**ISOweek**](https://cran.r-project.org/web/packages/ISOweek/index.html)
* [**zoo**](https://cran.r-project.org/web/packages/zoo/index.html)

### Analysis code

* [functions.R](src/functions.R) - functions used across sprints
* [01_clean_CQC.R](src/01_clean_CQC.R) - cleans pseudonimysed care home characteristics
* [02_clean_MMPI.R](src/02_clean_MMPI.R) - cleaning pseudonimysed master patient index
* [03_admissions.R](src/03_admissions.R) - cleans hospital admissions data, engineers features
* [04_hipfracture_cohort.R](src/04_hipfracture_cohort.R) - create study cohort, summarise baseline characteristics 
* [04b_SMD_plots.R](src/04b_SMD_plots.R) - create figures showing SMDs created in script 04
* [05_hipfracture_outcomes_crude.R](src/05_hipfracture_outcomes_crude.R) -  calculate crude mortality rates
* [06_outcomes_models.R](src/06_outcomes_models.R) - statistical modelling, create summary tables

## Lookup table

* [SMD lookup](https://github.com/HFAnalyticsLab/COVID19_carehome_hipfrac_mortality/blob/main/lookup_tables/smd_lookup.xlsx) - lookup table for axis labels used in script 04b

## References

Oliveira dos Santos, F., Conti, S. and Wolters, A. (2021) “A Novel Method for Identifying Care Home Residents in England: A Validation Study”, International Journal of Population Data Science, 5(4). [oi: 10.23889/ijpds.v5i4.1666](https://ijpds.org/article/view/1666).

## Authors

* **Fiona Grimm** - on [Twitter](https://twitter.com/fiona_grimm) or [GitHub](https://github.com/fiona-grimm)
* **Richard Brine** - on [GitHub](https://github.com/richardbrine)

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/COVID19_carehome_hipfrac_mortality/blob/master/LICENSE).
