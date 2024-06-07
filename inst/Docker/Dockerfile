FROM wernitznig/shiny

LABEL maintainer="Andreas Wernitznig"
LABEL email="andreas@wernitznig.com"

ENV dbhost=google
ENV dbport=5432
ENV dbname=bioinfo_hg38
ENV dbuser=reader
ENV dbpass=reader

# update to old shiny lib
COPY inst/Docker/update_shiny.R /install/update_shiny.R
RUN Rscript /install/update_shiny.R

# copy shiny app
COPY shinyApp ./app 

# set access and port
COPY inst/Docker/Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('./app')"]
