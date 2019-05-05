FROM ubuntu:18.04

# Update and install packages
RUN apt-get update -y && apt-get upgrade -y
#RUN apt-get install -y ...

# Create a new user
RUN groupadd users || true
RUN useradd -m -G users user

# Run
USER user
CMD ["whoami"]
