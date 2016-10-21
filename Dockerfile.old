FROM rocker/r-base

RUN apt-get update \
	&& apt-get install -y \
		libxml2-dev \
		libcurl4-gnutls-dev \
		python \
		python-pip	

COPY /mlb-analytics-engine.R .

RUN pip install awscli

CMD ["Rscript", "mlb-analytics-engine.R"]