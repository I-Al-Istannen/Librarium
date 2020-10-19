# vim: ft=dockerfile

# Base image is haskell
FROM haskell:8
LABEL "librarium"="true"

# We use nginx as a reverse proxy
# We use sudo to drop privileges
RUN apt update && apt install nginx -y && apt install sudo -y && apt install libc6 -y

# Preconfigured nginx configs for deployment
COPY nginx-site /etc/nginx/sites-available/nginx-site
COPY nginx.conf /etc/nginx/nginx.conf

ARG USER_ID

RUN ln -s /etc/nginx/sites-available/nginx-site /etc/nginx/sites-enabled && \
    rm /etc/nginx/sites-enabled/default && \
    # The user id is injected at build time
    useradd --uid $USER_ID librarium

# Expose the data dir
VOLUME ["/home/librarium/data"]

# Frontend serving port (from nginx config)
EXPOSE 80
# API serving port
EXPOSE 81

COPY start.sh /home/librarium/start.sh

# Include backend files
COPY backend-exe /home/librarium/backend
COPY dist /home/librarium/frontend

# We should be able to place temp data in there
# Not *strictly* needed though
RUN chown -R librarium:librarium /home/librarium

# Execute the server, needs the path to the configuration file and users passed as an argument
ENTRYPOINT ["/home/librarium/start.sh"]
