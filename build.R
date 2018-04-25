############################################################
#                                                          #
#          Builds the outputs using the files in           #
#           the './scripts/' directory (requires           #
#                 the original data files                  #
#             or the cleaned data files to be              #
#            present in the './original-data/'             #
#         or './data/' directories, respectively).         #
#                                                          #
############################################################

# Extract data from original Excel data files
#############################################
## Files must be located in directory: './original-data/'
## Cleaned R data objects will be saved to the './data/' directory

### Create the './data/' directory if it is not present
if(!dir.exists('./data')) {
    dir.create('./data')
}

### Run the data extraction and cleaning script
source(file = 'scripts/extract-data.R',
       local = TRUE)

# Render Rmarkdown files
########################
## The scripts are located in the './scripts/' directory
## Outputs will be saved to the './outputs/' directory

### Create a list of script paths
paths <- list('descriptive-data',
              'data-completeness',
              'dropout-predictors')

### Apply the rmarkdown render function on items of the list
lapply(X = paths,
       function(x) {
           rmarkdown::render(input = paste0('scripts/', x, '.Rmd'),
                             output_dir = 'outputs/')
       })

### Remove any unwanted outputs (*.html files)
lapply(X = paths,
       function(x) {
           file.remove(paste0('outputs/', x, '.html'))
       })
