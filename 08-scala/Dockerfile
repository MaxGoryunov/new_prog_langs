# Use the official OpenJDK image as a base
FROM sbtscala/scala-sbt:graalvm-community-22.0.1_1.10.6_3.6.2


# Set the Scala version you want to install
# ENV SCALA_VERSION=2.13.10

# RUN apt-get update && \
#     apt-get install -y scala

# Set environment variables for Scala
# ENV SCALA_HOME=/usr/local/scala
# ENV PATH=$SCALA_HOME/bin:$PATH

# Create a directory for your Scala application
WORKDIR /app

# Copy your Scala application files into the container (adjust as needed)
COPY . .

# RUN scalac converter.scala

# Command to run your Scala program (adjust as needed)
CMD ["bash"]
