# packages that we want to use
libs <- c("tidyverse", "ranger")

# require and install automatically
requireLibs <- function(libs){
  for(lib in libs){
    if(require(lib, character.only = TRUE)){
      print(paste(lib, "is loaded correctly"))
    }
    else{
      print(paste("trying to install", lib))
      install.packages(lib)
      if(require(lib, character.only = TRUE)){
        print(paste(lib, "installed and loaded"))
      }
      else{
        stop(paste("could not install", lib))
      }
    }
  }
}

# RUN
requireLibs(libs)
