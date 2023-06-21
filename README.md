This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor). 

This problem set retraces the findings of the article "Defaults and Donations: Evidence from a Field Experiment" by Steffen Altmann, Armin Falk, Paul Heidhues, Rajshri Jayaraman and Marrit Teirlinck. The authors explore an approach for online fundraising websites to help them increase their donation revenues using defaults. The article can be found here: https://direct.mit.edu/rest/article-abstract/101/5/808/58537/Defaults-and-Donations-Evidence-from-a-Field.

## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
install.packages("RTutor",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))

if (!require(devtools))
  install.packages("devtools")

devtools::install_github("StefanieBuda/RTutorDefaultsAndDonations")
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(RTutorDefaultsAndDonations)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/RTutorDefaultsAndDonations")
# Adapt your user name
run.ps(user.name="Jon Doe", package="RTutorDefaultsAndDonations",
       auto.save.code=TRUE, clear.user=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
