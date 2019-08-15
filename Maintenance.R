# Maintenance.R

# This file has code for performing maintenance on the package.


#-----------------------------#
#### New Year Descriptions ####
#-----------------------------#

# Adjust the info in YearDescriptions.xlsx, then run this code
DescriptionLookup = openxlsx::read.xlsx(xlsxFile = "YearDescriptions.xlsx")
usethis::use_data(DescriptionLookup, overwrite = T)

