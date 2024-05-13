title = erlang-fp
VERSION ?= all

all: bfpg_tex bfpg_pdf

bfpg_tex: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer -H preamble.tex --listings -o bfpg.tex $<

bfpg_pdf: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-H preamble.tex \
		-V 'mainfont:FiraSans-Regular.ttf' \
		-V 'mainfontoptions:Path=../common/fonts/' \
		-o bfpg.pdf $<

watch: bfpg.org
	find . -type f \( -iname "*.org" -o -iname "*.ex" -o -iname "*.tex" \) | entr make $(VERSION)


.PHONY: all
