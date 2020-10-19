all: frontend backend

USER_ID ?= 1003

frontend:
	echo "#####################"
	echo "# Building Frontend #"
	echo "#####################"
	make -C frontend

backend:
	echo "#####################"
	echo "# Building Backend  #"
	echo "#####################"
	make -C backend

clean:
	make -C frontend clean
	make -C backend clean

docker: backend frontend
	mkdir -p .docker
	cp backend/build/backend-exe .docker
	cp -r frontend/dist .docker
	cp -r deploy/* .docker
	(cd .docker && sudo docker build -t librarium:latest --build-arg USER_ID=$(USER_ID) .)

docker-ci:
	mkdir -p .docker
	cp backend-exe .docker
	cp -r dist .docker
	cp -r deploy/* .docker
	(cd .docker && sudo docker build -t librarium:latest --build-arg USER_ID=$(USER_ID) .)

.PHONY: clean frontend backend
