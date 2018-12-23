FROM rocker/r-base

RUN apt-get update \
	&& apt-get install -y \
		git \
		libcurl4-openssl-dev \
		libxml2-dev \
		libmariadbclient-dev \
		libssl-dev
		
RUN git clone https://github.com/fire-ants/mlb-analytics-ant.git

WORKDIR /mlb-analytics-ant

RUN Rscript -e "install.packages('RJSONIO')" \
	&& Rscript -e "install.packages('RCurl')" \
	&& Rscript -e "install.packages('dplyr')" \
	&& Rscript -e "install.packages('ggplot2')" \
	&& Rscript -e "install.packages('akima')" \
	&& Rscript -e "install.packages('DBI')" \
	&& Rscript -e "install.packages('RMySQL')" \
	&& Rscript -e "install.packages('xml2')" \
	&& Rscript -e "install.packages('aws.signature')" \
	&& Rscript -e "install.packages('curl')" \
	&& Rscript -e "install.packages('httr')" \
	&& Rscript -e "install.packages('aws.s3', repos = c('cloudyr' = 'http://cloudyr.github.io/drat'))"

CMD ["Rscript", "00-mlb-visualization.R"]