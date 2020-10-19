all: frontend backend

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

.PHONY: clean frontend backend
