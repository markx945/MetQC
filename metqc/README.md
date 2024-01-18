
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetQC

<!-- badges: start -->

<!-- badges: end -->

The goal of MetQC is to provide Quartet-based QCtools for Metabolomics.

## Installation
You can install this package from github

``` r
# install.packages("devtools")
devtools::install_github("markx945/MetQC/metqc")
```

## Example

``` r
library(MetQC)
## Get performance report for metabolomics data
### The result document was outputed to "output" directory under the working directory
GetPerformance(dt.path=sample_data,metadata.path=sample_metadata)

## Count SNR and plot related PCA plot
CountSNR(dt=sample_data,metadata=sample_metadata)
#> [1] 4.511966

## Count RC and plot related scatter plot
CountRC(dt=sample_data,metadata=sample_metadata)
#> [1] 0.7637139

## Count Recall 
CountRecall(dt=sample_data,metadata=sample_metadata)
#> [1] 0.1649485

#### generate report
### get example data and reprot template path
example_sample <- system.file("extdata","sample_data.csv",package = "MetQC")
example_metadata <- system.file("extdata","sample_metadata.csv",package = "MetQC")
doc_file <- system.file("extdata","Quartet_temp.docx",package = "MetQC")

### get metabolomics metrics data and generate report
met_result = GetPerformance(dt.path = example_sample,metadata.path = example_metadata)
GenerateReport(Met_result = met_result,doc_file_path = doc_file,output_path = './')


### The output directory can also be assigned.
# GetPerformance(output.path = [Your Path],dt=sample_data,metadata=sample_metadata)
# CountSNR(output.path = [Your Path],dt=sample_data,metadata=sample_metadata)
# CountRC(output.path = [Your Path],dt=sample_data,metadata=sample_metadata)
# CountRecall(output.path = [Your Path],dt=sample_data,metadata=sample_metadata)

```


