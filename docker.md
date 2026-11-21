Docker Instructions

Build:
>docker build -t cascala/galileo:0.2.1 -t cascala/galileo:latest .

Run:
>docker run -it --rm cascala/galileo

Publish (after `docker login`), for Intel and Apple Silicon at once:
>docker buildx build --platform linux/amd64,linux/arm64 -t cascala/galileo:0.2.1 -t cascala/galileo:latest --push .
