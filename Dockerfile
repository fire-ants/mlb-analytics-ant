FROM rocker/r-base

RUN apt-get update \
	&& apt-get install -y \
		libxml2-dev \
		libcurl4-gnutls-dev \
		python \
		python-pip	

COPY /02-GenerateImagesTmp.R .

RUN pip install awscli

CMD ["Rscript", "02-GenerateImagesTmp.R"]