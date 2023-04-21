title = erlang-fp

all: tex pdf

tex: main.org
	pandoc --standalone --slide-level=2 --to beamer -o $(title).tex main.org

pdf: main.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-V 'mainfont:FiraSans-Regular.ttf' \
		-V 'mainfontoptions:Path=../common/fonts/' \
		-o $(title).pdf main.org

watch: main.org
	ls *.org | entr make all

.PHONY: all
