build:
	docker build -t chat .
run: build
	docker run -it --net=host chat
redis:
	docker rm redis; docker run --name redis -d --net=host redis redis-server
redis-cli:
	docker run -it --rm --net=host redis redis-cli
