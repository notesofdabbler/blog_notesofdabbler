#' ---
#' title: Keeping up with Tidyverse Functions using Tidy Tuesday Screencasts
#' author: Notes of Dabbler
#' date: "`r Sys.Date()`"
#' ---
#' 
#' David Robinson has done several [screencasts](https://www.youtube.com/channel/UCeiiqmVK07qhY-wvg3IZiZQ) where he analyzes a  Tidy Tuesday dataset live. 
#' I have listened to a few of them and 
#' found them very interesting and instructive. As I don't use R on a daily basis, I 
#' have not kept up with what the latest is in Tidyverse. So when I listened to his screencasts,
#' I learnt functions that I was not aware of. Since I sometimes forget which function I learnt, I wanted to extract
#' all the functions used in the screencasts so that it is easier for me to refer to the ones that I am not aware of but should learn.  
#' 
#' The approach I took is:
#' 
#' * Get all the Rmd analysis files from the screencast github repo.
#' * Extract the list of libraries and functions used in each .Rmd file
#' * Plot frequencies of function use and review functions that I am not aware of
#' 
#' 
#+ setup, echo = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#'
#+ warning = FALSE
# Load libraries
library(tidyverse)
library(rvest)
library(DT)

#' First we get all the Rmd file from Dave Robinson's screencast github repo

# get list of files that have screencast analysis
githubrepo = read_html("https://github.com/dgrtwo/data-screencasts")
rmdfilelist = githubrepo %>% html_nodes(".content") %>% html_text() %>% str_trim()
rmdfilelist

# remove first 3 files since they are not analysis files
rmdfilelist = rmdfilelist[4:length(rmdfilelist)]

# get the link to raw file for each Rmd file
rmdfileListlink = paste0("https://raw.githubusercontent.com/dgrtwo/data-screencasts/master/", rmdfilelist)

#' Read each Rmd file and extract
#' 
#'  * List of libraries used
#'  * List of functions used
#' 
#' The approach to extract functions is based on the blog post([Top 100 most used R functions on GitHub](https://towardsdatascience.com/top-100-most-used-r-functions-on-github-9caf2b81b314))
#' The approach looks for words that precede the left parenthesis. It is not perfect but is good enough for this analysis. 
#'
liblistL = list()
fnlistL = list()

for(i in 1:length(rmdfileListlink)) {
  
  #print(i)
  
  # Read the Rmd file
  rmdfile = read_file(rmdfileListlink[i])
  
  # get list of libraries used in the file
  liblist = str_extract_all(rmdfile, "library\\(.*\\)") %>% unlist() %>%
    gsub("(library|\\(|\\))", "", .)
  liblistL[[i]] = tibble(liblist)
  
  # pattern to look for function
  fnpattern = "([a-zA-Z][a-zA-Z0-9_.]{0,43}[(])|([a-zA-Z][a-zA-Z0-9_.]{0,43}[ ][(])"
  fnlist = map(str_extract_all(rmdfile, fnpattern), ~ gsub("\\(", "", .x))
  fnlistL[[i]] = tibble(fnlist = fnlist[[1]])
  
}

# dataframe with list of libraries
liblistdf = bind_rows(liblistL)
libcountdf = liblistdf %>% count(liblist, sort = TRUE)

#' The plot below shows the number of analysis in which each package was
#' used.
#+ libcntplt, fig.path = "figure/"
ggplot(libcountdf) + geom_col(aes(x = fct_reorder(liblist, n), y = n), fill = "blue") + 
  labs(x = "", y = "# analysis that used the library") + theme_bw() + coord_flip()

#' The top library as tidyverse is to be expected. It is interesting that lubridate
#' is second. I can see that broom is used quite a bit since after exploratory analysis in 
#' the screencast, David explores some models. There are several that I was not aware of
#' but I will probably look up the following: widyr, fuzzyjoin, glue, janitor, patchwork and the 
#' context in which they were used in the screencast.

# dataframe with list of functions
fnlistdf = bind_rows(fnlistL)
fncountdf = fnlistdf %>% count(fnlist, sort = TRUE)

#' The table below lists the functions extracted from Rmd files.
datatable(fncountdf)

#' The current logic extracts things such as *aes*, *c*, *scale_x_continuous* which
#' are not of interest to us here. We will combine the above data with list of functions
#' in tidyverse packages to clean up the list of functions of interest.

# get all functions in tidyverse packages
# https://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package/8696442#8696442
#
pkglist = tidyverse_packages()
pkglist[which(pkglist == "readxl\n(>=")] = "readxl"
pkgfunsL = list()
for(i in 1:length(pkglist)) {
  #print(i)
  pkgfuns = ls(getNamespace(pkglist[i]), all.names=TRUE)
  pkgfunsL[[i]] = tibble(pkg = pkglist[i], pkgfuns = pkgfuns)
}

pkgfunsdf = bind_rows(pkgfunsL)

# keep extracted functions that have an associated tidyverse package
fncountdf2 = inner_join(fncountdf, pkgfunsdf %>% rename(fnlist = pkgfuns), by = "fnlist")

fncountpltdf = fncountdf2 %>%
       mutate(fnlist2 = fct_reorder(fnlist, n))

# Count number of functions used in each package
pkgfncount = fncountdf2 %>% count(pkg, sort = TRUE)

#' The plot below shows the number of functions used in each package
#+ pkgcntplt, fig.path = "figure/"
ggplot(pkgfncount) + geom_col(aes(x = fct_reorder(pkg, nn), y = nn), fill = "blue") + 
  labs(x = "", y = "# functions") + theme_bw() + coord_flip()

 
#' As expected, most used functions are from *ggplot2*, *dplyr*, *tidyr* since there
#' is lot of exploratory analysis and visualization of data in the screencasts. 
#' 
#' We next plot the list of functions used from each package.

getplot = function(df) {
  p = ggplot() + geom_col(data = df, aes(x = fnlist2, y = n), fill = "blue") 
  p = p + facet_wrap(~pkg, scales = "free") + labs(x = "", y = "")
  p = p + coord_flip() + theme_bw()
  p 
}

#+ fncountplt, fig.path = "figure/", fig.width = 10, fig.height = 10
getplot(fncountpltdf %>% 
          filter(pkg %in% c("ggplot2", "dplyr"))
)

getplot(fncountpltdf %>% 
          filter(pkg %in% c("tidyr", "lubridate", "stringr"))
)

getplot(fncountpltdf %>% 
          filter(!(pkg %in% c("ggplot2", "dplyr", "tidyr", "lubridate", "stringr")))
)


#' Based on the above figures, I am listing below some functions that I was not aware of and should learn
#' 
#' * *count* function in *dplyr* as a easier way to
#' count for each group or sum a variable for each group. 
#' * *geom_col* function in *ggplot2* for bar graphs
#' * I became aware of *forcats* package for working with factors. *fct_reorder* and *fct_lump* from the package were used frequently.
#' * *tidyr* functions - *nest/unnest*, *crossing*, *separate_rows*
#' * I realized that I know only a few functions in *stringr* and should learn more about several functions that were used in the screencast.
#'
#'
#' ## Session Info

sessionInfo()


