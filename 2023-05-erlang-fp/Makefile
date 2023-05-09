title = erlang-fp
VERSION ?= all

all: elixir_tex bfpg_tex elixir_pdf bfpg_pdf

elixir_tex: elixir.org
	pandoc --standalone --slide-level=2 --to beamer -o elixir.tex $<

bfpg_tex: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer -o bfpg.tex $<

elixir_pdf: elixir.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-V 'mainfont:FiraSans-Regular.ttf' \
		-V 'mainfontoptions:Path=../common/fonts/' \
		-o elixir_types_now.pdf $<

bfpg_pdf: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-V 'mainfont:FiraSans-Regular.ttf' \
		-V 'mainfontoptions:Path=../common/fonts/' \
		-o bfpg.pdf $<

watch: elixir.org bfpg.org
	ls *.org | entr make $(VERSION)

.PHONY: all
