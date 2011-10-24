EBIN_DIR = ebin

all: werld_server

werld_server:
	erl -make

clean:
	rm -rf ${EBIN_DIR}/*.beam
