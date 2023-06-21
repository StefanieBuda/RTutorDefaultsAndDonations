# data preparation

library(haven)
library(dplyr)
library(labelled)

# Load original data
data <- read_dta("defaults_donations.dta")

# remove top 0.2% percent and use labels for treatments
data <- data %>%
  filter(q99_8 != 1)%>%
  mutate(treat = as.character(paste0("(", unlabelled(treat), ")")))

# remove unused columns
data <- data %>%
  select(-c(receiver_type, tax_deductible, q99_8, donation_sum, codonation_sum,
            D0, D10, D20, D50, C5, C10, C15, T05, T105, T205, T505, T010, T1010, T2010, T5010, T015, T1015, T2015, T5015, 
            don_followdefault, codon_followdefault, both_follow))

# save preprocessed data set
write_dta(data, file.path(getwd(), "DefaultsAndDonations.dta"))
