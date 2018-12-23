FROM rocker/r-base

RUN apt-get update \
	&& apt-get install -y \
		git \
		libcurl4-openssl-dev \
		libxml2-dev \
		libmariadbclient-dev
		
#		python \
#		python-pip

# RUN pip install awscli

RUN git clone https://github.com/fire-ants/mlb-analytics-ant.git

WORKDIR /mlb-analytics-ant

RUN Rscript -e "install.packages('RJSONIO')"
RUN Rscript -e "install.packages('RCurl')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('akima')"
RUN Rscript -e "install.packages('DBI')"
RUN Rscript -e "install.packages('RMySQL')"
RUN Rscript -e 'install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))'

CMD ["Rscript", "00-mlb-visualization.R"]