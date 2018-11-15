# AGEPRO
The ApplyBB_CR.R can do 2 things:
1) use a pre-existing, functional AgePro .INP file, but iteratively runs AgePro to automate the application of a status (i.e., biomass-based; B/Bmsy) based harvest control rule.  AgePro is then reapplied with the catch in year y+1 (year y is the "bridge" year) used in all years (i.e., a constant catch, at least in the short-term).  The result is tables with short-term projection results.  

2) optionally use a pre-existing, functional AgePro .INP file to run a long-term MSY agepro and return Fmsy, Bmsy, MSY, recruitment at MSY, and then combine these results with the results of #1 above and write a .txt file that contains Latex ready code that is fully compatible with the GARM auto-update report R-script.

The Analyze_AgePro.R and ProjectionTables.Rmd files contain functions that apply the control rule, calculate median and CIs for age pros runs, and create the tables.  These files are "sourced" in the ApplyBB_CR.R file.
