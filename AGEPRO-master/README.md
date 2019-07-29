# AGEPRO

*Jonathan J. Deroba - package added July 26, 2019*

The only useful function is AgeProRun.  The user must provide a directory (direct) that contains a functional AgePro.INP file (proj.fname).  **The code assumes that the AgePro.INP file (proj.fname) points to the directory containing the correct ASAP.BSN file and that the first year of the projection is a "bridge year" with the correct catch already specified.**  The code will create two .docx tables.  The first table "...annual.docx" is the outcome of annually applying a user specifed harvest control rule and reports: catch, F, SSB, P(overfishing), P(overfished), OFL, and SSB/SSBmsy.  The second table "...concatch.docx" takes the catch value from year 2 of "...annual.docx" and applies this catch for the remainder of the projection period (a constant catch).  Optionally (if domsy=TRUE), the user can provide a functional AgePro.INP file setup for a longterm projection at Fmsy (msy.name) in the same directory, along with some other conditional inputs to the function, and the code will produce LaTex code in a .txt file comparing old and new MSY reference points (with CI for new ref points), and short-term (four year) projection results with CI.  This .txt file should be readily compatible for input to the groundfish summary reports.

*Example code*

devtools::install_github("jonathanderoba/AGEPRO/AgePro")

library(AgePro)

AgeProRun(direct = "C:\\NFT\\AGEPROV40\\GBYT_test",proj.fname="Fref025",FracBmsyThreshHi = 0.0,FracBmsyThreshLo = 0.0,
          FracFtarg = 1,Bmsy=26800,Fmsy = 0.25,decimals=3,domsy = TRUE,msy.name="FREF025_LONG",CIwantLow = 0.05,CIwantHi = 0.95,
          fmsyold = 0.25,Bmsyold = 1000,msyold = 225,recrold = 55000,nyr.avg = 10)


