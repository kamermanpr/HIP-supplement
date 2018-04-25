############################################################
#                                                          #
#  Builds the outputs using the files in the './scripts/'  #
#             directory (Requires the original             #
#                data or the cleaned data)                 #
#                                                          #
############################################################

# Extract data from original excel files
# (files must be located in directory: './original-data/')

if(!dir.exists('./data')) {
    dir.create('./data')
}

source(file = 'scripts/extract-data.R',
       local = TRUE)

# Render Rmarkdown files


rmarkdown::render(input = 'scripts/descriptive-data.Rmd',
                  output_dir = 'outputs/')
file.remove('./outputs/descriptive-data.html')
