# Build stage: JDK 21 + sbt (sbt is fetched from its GitHub release)
FROM eclipse-temurin:21-jdk AS build

ARG SBT_VERSION=1.10.7
RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates && \
    curl -fsSL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" | tar xz -C /opt && \
    ln -s /opt/sbt/bin/sbt /usr/local/bin/sbt && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /galileo

# Resolve dependencies first so they are cached between source changes
COPY build.sbt ./
COPY project/build.properties project/assembly.sbt project/
RUN sbt -batch update

COPY src src
# Build the 'fat' jar
RUN sbt -batch assembly && cp target/scala-2.13/Galileo-assembly-*.jar /galileo.jar

# Run stage: JRE only
FROM eclipse-temurin:21-jre
COPY --from=build /galileo.jar /opt/galileo/galileo.jar
ENTRYPOINT [ "java", "-jar", "/opt/galileo/galileo.jar" ]
