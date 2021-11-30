FROM ubuntu:18.04
# minimal docker file to get sp and sf running on ubunty 16.04 image,
# using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable

MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de

RUN apt-get update && apt-get install -y software-properties-common
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/" >> /etc/apt/sources.list

RUN apt-key  adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

RUN apt-get update
RUN apt-get upgrade -y

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y \
	gdb \
	git \
	libcairo2-dev \
	libcurl4-openssl-dev \
	libexpat1-dev \
	libpq-dev \
	libsqlite3-dev \
	libudunits2-dev \
	make \
	pandoc \
	qpdf \
	r-base-dev \
  	sqlite3 \
	subversion \
	valgrind \
	vim \
	tk-dev \
	wget

RUN apt-get install -y \
	libv8-3.14-dev \
	libjq-dev \
	libprotobuf-dev \
	libxml2-dev \
	libprotobuf-dev \
	protobuf-compiler \
	unixodbc-dev \
	libssh2-1-dev \
	libgit2-dev \
	libnetcdf-dev \
	locales \
	libssl-dev \
	libtiff-dev

RUN locale-gen en_US.UTF-8

ENV PROJ_VERSION=7.1.0
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y \
	cmake \
	libtiff5-dev

#RUN git clone --depth 1 https://github.com/OSGeo/PROJ.git
RUN wget http://download.osgeo.org/proj/proj-7.1.1.tar.gz
RUN tar zxvf proj-7.1.1.tar.gz
RUN cd proj-7.1.1 \
  && ls -l \
  && mkdir build \
  && cd build \
  && cmake .. \
  && make \
  && make install \
  && cd ../.. \
  && ldconfig

# install proj-data:
#RUN cd /usr/local/share/proj \
#  && wget http://download.osgeo.org/proj/proj-data-1.1RC1.zip \
#  && unzip -o proj-data*zip \
#  && rm proj-data*zip \
#  && cd -

# GEOS:
ENV GEOS_VERSION 3.8.1

RUN wget http://download.osgeo.org/geos/geos-${GEOS_VERSION}.tar.bz2 \
  && bzip2 -d geos-*bz2 \
  && tar xf geos*tar \
  && cd geos* \
  && ./configure \
  && make \
  && make install \
  && cd .. \
  && ldconfig

# GDAL:

# https://download.osgeo.org/gdal/3.1.0/gdal-3.1.0rc2.tar.gz
ENV GDAL_VERSION 3.1.3
ENV GDAL_VERSION_NAME 3.1.3rc1

RUN wget http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION_NAME}.tar.gz \
  && tar -xf gdal-${GDAL_VERSION_NAME}.tar.gz \
  && cd gdal* \
  && ./configure \
  && make \
  && make install \
  && cd .. \
  && ldconfig

#RUN git clone --depth 1 https://github.com/OSGeo/gdal.git
#RUN cd gdal/gdal \
#  && ls -l \
#  && ./configure \
#  && make \
#  && make install \
#  && cd .. \
#  && ldconfig

#RUN Rscript -e 'install.packages(c("sp", "rgeos", "rgdal", "RPostgreSQL", "RSQLite", "testthat", "knitr", "tidyr", "geosphere", "maptools", "maps", "microbenchmark", "raster", "dplyr", "tibble", "units", "DBI",  "covr", "protolite", "tmap", "mapview", "odbc", "pool", "rmarkdown", "RPostgres","spatstat", "stars"))'
RUN Rscript -e 'install.packages(c("sf"))'

CMD ["/bin/bash"]

