rm(list=ls())
#table 1: summary table
full=read.csv("building_block/full.csv")
install.packages("stargazer"); library(stargazer)
#use summary.stat to perfectly capture summary table in the paper
#https://cran.r-project.org/web/packages/stargazer/stargazer.pdf
#excel table transpose
#https://www.excel-easy.com/examples/transpose.html
stargazer(na.omit(subset(full,select=c(total_aid_gdp,grant_gdp,
          loan_gdp,tax_to_gdp,real_gdp_growth,real_gdp_capita,
          trade_open,industry_value_gdp,natural_rent_gdp))),
          type="html",out="tables/table1.html",
          summary.stat = c("mean", "median", "max", "min", "sd", "n"))
#table 2
install.packages("AER"); library(AER)
full$year.fac=as.factor(full$year)
full$subregion.fac=as.factor(full$subregion)
#omit na in lm
#https://stats.stackexchange.com/a/11028
stargazer(lm(tax_to_gdp~total_aid_gdp+real_gdp_capita,
             data=full,na.action = na.omit),
          ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita|dist.aid+col.aid+religion.aid+real_gdp_capita,
                data=full,
                na.action = na.omit),
          lm(tax_to_gdp~total_aid_gdp+real_gdp_capita+year.fac,
             data=full,na.action = na.omit),
          ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita+year.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+year.fac,
                data=full,
                na.action = na.omit),
          type="html",out="tables/table2_IV.html",
          omit.stat=c("adj.rsq","f","ser"))
stargazer(lm(total_aid_gdp~dist.aid+col.aid+religion.aid, 
             data=
na.omit(subset(full,select=c(tax_to_gdp,total_aid_gdp,real_gdp_capita,dist.aid,col.aid,religion.aid)))),
          type="html", out="tables/table2_OLS.html",
          omit.stat=c("adj.rsq","f","ser"))
#cragg-donald f-statistic documentation
#https://cran.r-project.org/web/packages/cragg/vignettes/introduction.html
install.packages("cragg"); library(cragg)
table2=na.omit(subset(full,select=c(tax_to_gdp,total_aid_gdp,real_gdp_capita,dist.aid,col.aid,religion.aid)))
colnames(table2)[1]="Y"
cragg_donald(X=~real_gdp_capita, # Control Variables
             D=~total_aid_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = table2)
#summary that produces over-identification test p-value
#https://stats.stackexchange.com/a/134825
summary(ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita|dist.aid+col.aid+religion.aid+real_gdp_capita,
                        data=full,
                        na.action = na.omit),diagnostics = TRUE)
summary(ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita+year.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+year.fac,
              data=full,
              na.action = na.omit),diagnostics = TRUE)
#table 3
rm(table2)
colnames(full)
col1=subset(full,select=c(
  tax_to_gdp, total_aid_gdp, real_gdp_capita, trade_open, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col3=subset(full,select=c(
  tax_to_gdp, total_aid_gdp, real_gdp_capita, industry_value_gdp, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col4=subset(full,select=c(
  tax_to_gdp, total_aid_gdp, real_gdp_capita, natural_rent_gdp, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col5=subset(full,select=c(
  tax_to_gdp, total_aid_gdp, real_gdp_capita, real_gdp_growth, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col7=subset(full,select=c(
  tax_to_gdp, total_aid_gdp, real_gdp_capita, trade_open,  
  industry_value_gdp, natural_rent_gdp, real_gdp_growth, 
  dist.aid, col.aid, religion.aid, year.fac, subregion.fac
))
iv1=ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita+trade_open+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            trade_open+year.fac+subregion.fac,
      data=col1,
      na.action = na.omit)
iv3=ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita+industry_value_gdp+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            industry_value_gdp+year.fac+subregion.fac,
          data=col3,
          na.action = na.omit)
iv4=ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita+natural_rent_gdp+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            natural_rent_gdp+year.fac+subregion.fac,
          data=col4,
          na.action = na.omit)
iv5=ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita+real_gdp_growth+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            real_gdp_growth+year.fac+subregion.fac,
          data=col5,
          na.action = na.omit)
iv7=ivreg(tax_to_gdp~total_aid_gdp+real_gdp_capita+trade_open+
           industry_value_gdp+natural_rent_gdp+real_gdp_growth+year.fac+
           subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
           trade_open+industry_value_gdp+natural_rent_gdp+real_gdp_growth+
           year.fac+subregion.fac, 
           data=col7,
           na.action = na.omit)
stargazer(iv1, iv3, iv4, iv5, iv7,
          type="html",out="tables/table3_IV.html",
          omit.stat=c("adj.rsq","f","ser"))
stargazer(lm(total_aid_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col1)),
          lm(total_aid_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col3)),
          lm(total_aid_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col4)),
          lm(total_aid_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col5)),
          lm(total_aid_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col7)),
          type="html",out="tables/table3_OLS.html",
          omit.stat=c("adj.rsq","f","ser"))
colnames(col1)[1]="Y"; colnames(col3)[1]="Y"; colnames(col4)[1]="Y"
colnames(col5)[1]="Y"; colnames(col7)[1]="Y"
cragg_donald(X=~real_gdp_capita+trade_open, # Control Variables
             D=~total_aid_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col1))
summary(iv1,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+industry_value_gdp, # Control Variables
             D=~total_aid_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col3))
summary(iv3,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+natural_rent_gdp, # Control Variables
             D=~total_aid_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col4))
summary(iv4,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+real_gdp_growth, # Control Variables
             D=~total_aid_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col5))
summary(iv5,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+trade_open+industry_value_gdp+
               natural_rent_gdp+real_gdp_growth, # Control Variables
             D=~total_aid_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col7))
summary(iv7,diagnostics = TRUE)
#table 4
colnames(full)
col0=subset(full,select=c(
  tax_to_gdp, grant_gdp, real_gdp_capita, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col1=subset(full,select=c(
  tax_to_gdp, grant_gdp, real_gdp_capita, trade_open, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col3=subset(full,select=c(
  tax_to_gdp, grant_gdp, real_gdp_capita, industry_value_gdp, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col4=subset(full,select=c(
  tax_to_gdp, grant_gdp, real_gdp_capita, natural_rent_gdp, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col5=subset(full,select=c(
  tax_to_gdp, grant_gdp, real_gdp_capita, real_gdp_growth, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col7=subset(full,select=c(
  tax_to_gdp, grant_gdp, real_gdp_capita, trade_open,  
  industry_value_gdp, natural_rent_gdp, real_gdp_growth, 
  dist.aid, col.aid, religion.aid, year.fac, subregion.fac
))
iv0=ivreg(tax_to_gdp~grant_gdp+real_gdp_capita+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            year.fac+subregion.fac,
          data=col0,
          na.action = na.omit)
iv1=ivreg(tax_to_gdp~grant_gdp+real_gdp_capita+trade_open+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            trade_open+year.fac+subregion.fac,
          data=col1,
          na.action = na.omit)
iv3=ivreg(tax_to_gdp~grant_gdp+real_gdp_capita+industry_value_gdp+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            industry_value_gdp+year.fac+subregion.fac,
          data=col3,
          na.action = na.omit)
iv4=ivreg(tax_to_gdp~grant_gdp+real_gdp_capita+natural_rent_gdp+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            natural_rent_gdp+year.fac+subregion.fac,
          data=col4,
          na.action = na.omit)
iv5=ivreg(tax_to_gdp~grant_gdp+real_gdp_capita+real_gdp_growth+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            real_gdp_growth+year.fac+subregion.fac,
          data=col5,
          na.action = na.omit)
iv7=ivreg(tax_to_gdp~grant_gdp+real_gdp_capita+trade_open+
            industry_value_gdp+natural_rent_gdp+real_gdp_growth+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            trade_open+industry_value_gdp+natural_rent_gdp+real_gdp_growth+
            year.fac+subregion.fac, 
          data=col7,
          na.action = na.omit)
stargazer(lm(tax_to_gdp~grant_gdp+real_gdp_capita+year.fac+subregion.fac,
             data=na.omit(col0)), 
          iv0, iv1, iv3, iv4, iv5, iv7,
          type="html",out="tables/table4_IV.html",
          omit.stat=c("adj.rsq","f","ser"))
stargazer(lm(grant_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col0)),
          lm(grant_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col1)),
          lm(grant_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col3)),
          lm(grant_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col4)),
          lm(grant_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col5)),
          lm(grant_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col7)),
          type="html",out="tables/table4_OLS.html",
          omit.stat=c("adj.rsq","f","ser"))
cragg_donald(X=~real_gdp_capita, # Control Variables
             D=~grant_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col0))
summary(iv0,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+trade_open, # Control Variables
             D=~grant_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col1))
summary(iv1,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+industry_value_gdp, # Control Variables
             D=~grant_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col3))
summary(iv3,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+natural_rent_gdp, # Control Variables
             D=~grant_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col4))
summary(iv4,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+real_gdp_growth, # Control Variables
             D=~grant_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col5))
summary(iv5,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+trade_open+industry_value_gdp+
               natural_rent_gdp+real_gdp_growth, # Control Variables
             D=~grant_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col7))
summary(iv7,diagnostics = TRUE)
#table 5
colnames(full)
col0=subset(full,select=c(
  tax_to_gdp, loan_gdp, real_gdp_capita, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col1=subset(full,select=c(
  tax_to_gdp, loan_gdp, real_gdp_capita, trade_open, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col3=subset(full,select=c(
  tax_to_gdp, loan_gdp, real_gdp_capita, industry_value_gdp, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col4=subset(full,select=c(
  tax_to_gdp, loan_gdp, real_gdp_capita, natural_rent_gdp, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col5=subset(full,select=c(
  tax_to_gdp, loan_gdp, real_gdp_capita, real_gdp_growth, dist.aid, col.aid,
  religion.aid, year.fac, subregion.fac
))
colnames(full)
col7=subset(full,select=c(
  tax_to_gdp, loan_gdp, real_gdp_capita, trade_open,  
  industry_value_gdp, natural_rent_gdp, real_gdp_growth, 
  dist.aid, col.aid, religion.aid, year.fac, subregion.fac
))
iv0=ivreg(tax_to_gdp~loan_gdp+real_gdp_capita+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            year.fac+subregion.fac,
          data=col0,
          na.action = na.omit)
iv1=ivreg(tax_to_gdp~loan_gdp+real_gdp_capita+trade_open+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            trade_open+year.fac+subregion.fac,
          data=col1,
          na.action = na.omit)
iv3=ivreg(tax_to_gdp~loan_gdp+real_gdp_capita+industry_value_gdp+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            industry_value_gdp+year.fac+subregion.fac,
          data=col3,
          na.action = na.omit)
iv4=ivreg(tax_to_gdp~loan_gdp+real_gdp_capita+natural_rent_gdp+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            natural_rent_gdp+year.fac+subregion.fac,
          data=col4,
          na.action = na.omit)
iv5=ivreg(tax_to_gdp~loan_gdp+real_gdp_capita+real_gdp_growth+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            real_gdp_growth+year.fac+subregion.fac,
          data=col5,
          na.action = na.omit)
iv7=ivreg(tax_to_gdp~loan_gdp+real_gdp_capita+trade_open+
            industry_value_gdp+natural_rent_gdp+real_gdp_growth+year.fac+
            subregion.fac|dist.aid+col.aid+religion.aid+real_gdp_capita+
            trade_open+industry_value_gdp+natural_rent_gdp+real_gdp_growth+
            year.fac+subregion.fac, 
          data=col7,
          na.action = na.omit)
stargazer(lm(tax_to_gdp~loan_gdp+real_gdp_capita+year.fac+subregion.fac,
             data=na.omit(col0)), 
          iv0, iv1, iv3, iv4, iv5, iv7,
          type="html",out="tables/table5_IV.html",
          omit.stat=c("adj.rsq","f","ser"))
stargazer(lm(loan_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col0)),
          lm(loan_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col1)),
          lm(loan_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col3)),
          lm(loan_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col4)),
          lm(loan_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col5)),
          lm(loan_gdp~dist.aid+col.aid+religion.aid,
             data=na.omit(col7)),
          type="html",out="tables/table5_OLS.html",
          omit.stat=c("adj.rsq","f","ser"))
cragg_donald(X=~real_gdp_capita, # Control Variables
             D=~loan_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col0))
summary(iv0,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+trade_open, # Control Variables
             D=~loan_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col1))
summary(iv1,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+industry_value_gdp, # Control Variables
             D=~loan_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col3))
summary(iv3,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+natural_rent_gdp, # Control Variables
             D=~loan_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col4))
summary(iv4,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+real_gdp_growth, # Control Variables
             D=~loan_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col5))
summary(iv5,diagnostics = TRUE)
cragg_donald(X=~real_gdp_capita+trade_open+industry_value_gdp+
               natural_rent_gdp+real_gdp_growth, # Control Variables
             D=~loan_gdp, # Treatments
             Z=~dist.aid+col.aid+religion.aid,# Instruments
             data = na.omit(col7))
summary(iv7,diagnostics = TRUE)
