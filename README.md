# figured
simple, miscellaneous functions I've developed for monitoring &amp; evaluation.

## Installation
### Prerequisites
#### RTools
This is required to compile many packages. RTools can be downloaded from [cran here](https://cran.r-project.org/bin/windows/Rtools/).

#### Java
The ODK functions in this package are little more than an R interface for the command line functions for ODK Briefcase. As such, it will require that you have Java installed. As of this writing, [Get ODK says they verify with Java 11](https://docs.getodk.org/briefcase-install/), and ecommend installing [OpenJDK 11 LTS](https://adoptopenjdk.net/) from AdoptOpenJDK.

#### Devtools
If you haven't already installed devtools, do so with the following  
  
`install.packages("devtools")`

### installation code
Next, load devtools and install the package from the repository  
```
library(devtools)  
install_github("jwilliamrozelle/figured")  
```

## Main features
Main functions of the package include some data manipulation functions and ODK Briefcase in R. This package will allow you to pull and export normal and encrypted forms.
