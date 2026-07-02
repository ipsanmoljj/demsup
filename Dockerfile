FROM rocker/r-ver:4.4.1

# System dependencies for R packages (data.table, xml2, curl, etc.)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages used by plumber.R and sourced R files
RUN for i in 1 2 3; do \
      install2.r --error --skipinstalled \
        plumber jsonlite data.table xgboost glmnet forecast strucchange zoo lubridate caret ranger \
      && break || sleep 10; \
    done && rm -rf /tmp/downloaded_packages

WORKDIR /app
COPY . .

# Railway provides PORT; plumber must bind to 0.0.0.0 on that port
EXPOSE ${PORT:-8080}
CMD Rscript -e "plumber::pr_run(plumber::pr('plumber.R'), host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '8080')))"
