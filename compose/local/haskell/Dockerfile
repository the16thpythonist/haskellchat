FROM haskell:8.6.5


RUN apt-get update
RUN apt-get install -y python3 python3-dev curl libpq-dev
RUN curl https://bootstrap.pypa.io/get-pip.py -o gp.py
RUN python3 gp.py
RUN pip3 install psycopg2-binary

COPY . /app

WORKDIR /app

# Copying the script which will be used to start the server
COPY ./compose/local/haskell/start.sh /start
RUN sed -i 's/\r//' /start
RUN chmod +x /start

COPY ./compose/local/haskell/entrypoint.sh /entrypoint
RUN sed -i 's/\r//' /entrypoint
RUN chmod +x /entrypoint

# Building the stuff
RUN stack install --system-ghc yesod-bin
RUN stack build --system-ghc

ENTRYPOINT ["/entrypoint"]