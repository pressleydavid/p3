# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc
    
# install plumber, GGally
RUN R -e "install.packages(c('GGally', 'leaflet', 'plumber', 'tidymodels', 'yardstick', 'tidyverse','ranger'))"

# copy API.R from the current directory into the container
COPY API.R API.R
COPY diabetes_model.rds diabetes_model.rds
COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv

# open port to traffic
EXPOSE 8000

# when the container starts, start the API.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=8000)"]
