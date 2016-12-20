FROM mormahr/latex-full

RUN apt-get update
RUN apt-get install wget -y
RUN wget -qO- https://get.haskellstack.org/ | sh

RUN echo 'export PATH=/root/.local/bin:$PATH\n' >> /root/.bashrc

RUN stack setup 8.0.1
RUN stack install aeson HaTeX

ADD artifacts/pdf-slave /usr/bin/pdf-slave
ADD artifacts/haskintex /usr/bin/haskintex
