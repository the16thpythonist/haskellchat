FROM haskellchat_local_haskell

# Copying the script which will be used to start the server
COPY ./compose/local/haskell/start.sh /start
RUN sed -i 's/\r//' /start
RUN chmod +x /start

COPY ./compose/local/haskell/entrypoint.sh /entrypoint
RUN sed -i 's/\r//' /entrypoint
RUN chmod +x /entrypoint

ENTRYPOINT ["/entrypoint"]