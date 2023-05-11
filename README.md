# MSBiometrics Package


# Project Overview
  This project builds are R-package containing biometric authetication functions from Dr. Michael Schuckers'
  Computational Methods for Biometrics Authentication, Springer (2010). This package is for an updated storage of these functions,
  so they could be used easily with old projects, newer research, and shared with others.
  
  Each Folder/File's purpose in the package will be described below.
  
 # .Rproj.user
  - This is created by R and used to build the package. 
    
 # R
  - fmr.R
      - This file contains all of the code for the FMR functions. any edits to functions should be made here and then edited
      in the function documentation Rmd file as well.
  - fnmr.R
      - This file contains all of the code for the FNMR functions.

# data
   - Each data set has a .rda file that is stored in this file. 
   - To read these data files into the folder, type use_this::use_this(NAMEOFDATA) in the R console

# man
   - The man folder contains the Rmd documentation files for all of the functions, and all of the data files.
   - Each function/data set must be documented in this folder.
   - To create a documentation of new function/data, create new file -> new documentation file and select either function or data
   - Each documentation file must have a title, description, and argument section, and the rest is option but value, and examples
   are useful and used in this package.
   
# .Rbuildignore
  - Made by R in building
# .Rhistory
  - Keeps history
  
# DESCRIPTION
  - This gives the overall package information, and is where authors, description, licensing, etc. can be edited.

# MSBiometrics.Rproj
  - The current Rproj space, and can be updated as anything else is changed.

# NAMESPACE
  - Used to read in any outside packages that the functions use. 
  - If any library/package other than base R is used within a function, it can be read in here.
