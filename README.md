# AGEPRO

*Jonathan J. Deroba November 15, 2018*

The ApplyBB_CR.R can do 2 things:
1) use a pre-existing, functional AgePro .INP file, but iteratively runs AgePro to automate the application of a status (i.e., biomass-based; B/Bmsy) based harvest control rule.  AgePro is then reapplied with the catch in year y+1 (year y is the "bridge" year) used in all years (i.e., a constant catch, at least in the short-term).  The result is tables with short-term projection results.  

2) optionally use a pre-existing, functional AgePro .INP file to run a long-term MSY agepro and return Fmsy, Bmsy, MSY, recruitment at MSY, and then combine these results with the results of #1 above and write a .txt file that contains Latex ready code that is fully compatible with the GARM auto-update report R-script.

*Instructions*

Open ApplyBB_CR.R

direct is the location of the AgePro input files and where all results will be saved; enter this directory.  Within this directory, paste Analyze_AgePro.R, ProjectionTables.Rmd, and AGEPRO40.exe.  This directory should also contain the functional short-term, and if desired, long-term MSY .INP files.  The Analyze_AgePro.R and ProjectionTables.Rmd files contain functions that apply the control rule, calculate median and CIs for age pros runs, and create the tables.  These files are "sourced" in the ApplyBB_CR.R file.

proj.fname is the name of the short-term .INP; enter this name.  The code assumes that the first year of this .INP is a "bridge" year with the appropriately specified catch.

FracBmsyThreshLo is the fraction of Bmsy at which no fishing is permitted (i.e., F=0).  F increases linearly between FracBmsyThreshLo to a maximum F at FracBmsyThreshHi.  FracFtarg is the maximum desired F.  For constant F in all years, FracBmsyThreshLo=FracBmsyThreshHi=0.0.

Bmsy and Fmsy are the current MSY reference points. decimals is the desired number of decimals for reporting results related to F and probabilities.

domsy is either TRUE or FALSE.  A FALSE assignment will only do short-term projections and not produce the .txt file for use with Latex.  If FALSE is specified, then the remaining inputs below domsy can be ignored.  A TRUE assignment will do a long-term MSY AgePro and produce the .txt for Latex.  msy.name is the name of the long-term .INP used to determine MSY (or proxy) reference points; enter this name.

CIwant are the desired CI bounds; enter the values.

fmsyold, bmsyold, msyold, and recrold are all the obsolete MSY reference points; enter their values.

nyr.avg is the number of years to be averaged from the end of the long-term age-pro run to determine the MSY reference points.

Touch nothing else and hit source...

With domsy<-FALSE, the code will produce 2 .docx files containing short-term projection results; one with F varying annually and a another with constant catch.  With domsy<-TRUE, the code will also produce the .txt file compatible with the GARM auto-update report R-script/Latex.
