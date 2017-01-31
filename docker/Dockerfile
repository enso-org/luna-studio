FROM alpine
MAINTAINER kamil.figiela@luna-lang.org

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

COPY env/config /etc/nodelab/config

RUN    apk update \
    && apk add zeromq s6 nginx gmp mercurial g++ \
    && ln -s /userdata/projects /etc/nodelab/projects \
    && adduser -u 82 -D -S -G www-data www-data

COPY docker/s6/combined /etc/s6/combined
COPY docker/s6/common  /etc/s6/common

COPY docker/nginx/nginx.conf /etc/nginx/nginx.conf
COPY docker/nginx/combined/nginx-default /etc/nginx/sites-enabled/default

CMD /bin/s6-svscan /etc/s6/combined

ENV HOME /root

EXPOSE 80 8088

COPY nodelab/www /usr/share/nodelab/gui

COPY dist/bin /usr/local/bin/nodelab

COPY userdata /userdata

COPY userdata/why_fp_matters.txt /lorem.txt
COPY userdata/cupcake_ipsum.txt  /ipsum.txt

COPY userdata /data

VOLUME ["/userdata"]

