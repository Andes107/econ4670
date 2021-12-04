rm(list=ls())
total_aid=read.csv("unmerged_unraw_csv/OECD_total_aid_1995_2019.csv",
                        fileEncoding="UTF-8-BOM")
#a special character appended to the first value of the first column
#https://stackoverflow.com/a/15399003
loan=read.csv("unmerged_data/OECD_net_loan_1995_2019.csv",
                   fileEncoding="UTF-8-BOM")
unique(total_aid$countries)
iso=read.csv("unmerged_unraw_csv/all.csv")
#intersect will not return duplicates
NROW(unique(total_aid$countries))
NROW(intersect(iso$name,total_aid$countries))
diff=setdiff(total_aid$countries, iso$name)
#end with either total or regional
#https://stackoverflow.com/a/49089598
diff[!grepl("(Total|regional)$", diff)]
