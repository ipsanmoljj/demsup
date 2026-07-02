FROM rocker/shiny:latest

RUN for i in 1 2 3; do \
      install2.r --error --skipinstalled \
        data.table ggplot2 zoo lubridate scales patchwork DT \
      && break || sleep 10; \
    done && rm -rf /tmp/downloaded_packages

WORKDIR /app

# Build context must be repo root — set "Root Directory" to / in Railway
COPY dashboard/app.R .
COPY output/regime_labels_CL.csv  output/regime_labels_CL.csv
COPY output/regime_labels_LCO.csv output/regime_labels_LCO.csv
COPY output/signal_CL.csv         output/signal_CL.csv
COPY output/signal_LCO.csv        output/signal_LCO.csv
COPY output/trades_CL.csv         output/trades_CL.csv
COPY output/trades_LCO.csv        output/trades_LCO.csv

EXPOSE 3838
CMD Rscript -e "shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', 3838)))"
