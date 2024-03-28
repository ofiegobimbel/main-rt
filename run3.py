from rpy2.robjects.packages import importr, SignatureTranslatedAnonymousPackage

utils = importr('utils')
base = importr('base')

utils.install_packages('tidyverse', repos = "https://cloud.r-project.org")
utils.install_packages('haven', repos = "https://cloud.r-project.org")
utils.install_packages('readr', repos = "https://cloud.r-project.org")

string = """
    testvar <- 1

    write_sas <- function(file, col_names = TRUE, write_to) {
        data <- readr::read_csv(file, col_names = col_names)
        haven::write_sas(data, path = write_to)
        print(paste("Data is written to ", write_to))
    }

    
    calculateIRT <- function(file, delim = ",", col_names = TRUE, unimodel, itemtype, method, write_to) {
        install.packages('mirt', repos = "https://cloud.r-project.org")

        PM <- readr::read_delim(file, delim = delim, col_names = col_names)

        print(PM)
        
        
    }
""" 

rwrap = SignatureTranslatedAnonymousPackage(string, "rwrap")

# rwrap.write_sas(file = "https://robjhyndman.com/data/ausretail.csv",
#                 col_names = False,
#                 write_to = "~/filename.sas7bdat")

rwrap.calculateIRT(file = "D:/Normalisasi Data Rstudio/new-irt/data-sampel.csv",
                delim = ";",
                unimodel = "F1 = 1-20",
                itemtype = "3PL",
                method = "MHRM",
                write_to = "~/filename.sas7bdat")

print(rwrap.testvar)