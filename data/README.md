# README

Participant consent did not provide for the publication of their data, and hence neither the original nor cleaned data have been made available. However, we do not wish to bar access to the data unnecessarily and we will judge requests to access the data on a case-by-case basis. Examples of potential use cases include independent assessments of our analyses, and secondary data analyses. Please contact Prof Romy Parker ([romy.parker@uct.ac.za](mailto:romy.parker@uct.ac.za)), Dr Antonia Wadley ([antonia.wadley@wits.ac.za](mailto:antonia.wadley@wits.ac.za)), or open an [_issue_](https://github.com/kamermanpr/HIP-supplement/issues) on this repo.

If your request for access is successful and you wish to reproduce the analyses, we recommend that you save the _demographics.xlsx_ and _amalgamated\_data.xlsx_ files we will supply you with into the _original\_data_ directory and then run the _build.R_ script located in the root directory of this repo. Doing so, will extract and clean the data from the two Excel files into five R data objects (_demographics.rds_, _bpi.rds_, _bdi.rds_, _eq5d.rds_, and _se6.rds_) located in the _data_ directory. These new data files will then be used to render the _RMarkdown_ scripts in the _scripts_ directory, overwriting the _\*.md_ files in the _outputs_ directory.  