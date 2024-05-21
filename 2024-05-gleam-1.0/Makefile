title = erlang-fp
VERSION ?= all

all: bfpg_tex bfpg_pdf alembic_pdf

bfpg_tex: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer -H preamble.tex --listings -o bfpg.tex $<

bfpg_pdf: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-H preamble.tex \
		-V 'mainfont:Atkinson-Hyperlegible-Regular-102.ttf' \
		-V 'mainfontoptions:Path=../common/fonts/' \
		-V 'monofont:FiraCode-Regular.ttf' \
		-V 'monofontoptions:Path=../common/fonts/' \
		--syntax-definition=gleam.xml \
		-o bfpg.pdf $<

alembic_pdf: alembic.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-H preamble.tex \
		-V 'mainfont:Atkinson-Hyperlegible-Regular-102.ttf' \
		-V 'mainfontoptions:Path=../common/fonts/' \
		-V 'monofont:FiraCode-Regular.ttf' \
		-V 'monofontoptions:Path=../common/fonts/' \
		--syntax-definition=gleam.xml \
		-o alembic.pdf $<

watch: bfpg.org alembic.org
	find . -type f \( -iname "*.org" -o -iname "*.ex" -o -iname "*.tex" \) | entr make $(VERSION)


.PHONY: all
