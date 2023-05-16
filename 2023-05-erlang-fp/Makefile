title = erlang-fp
VERSION ?= all

all: elixir_tex bfpg_tex elixir_pdf bfpg_pdf

elixir_tex: elixir.org
	pandoc --standalone --slide-level=2 --to beamer -H preamble.tex --listings -o elixir.tex $<

bfpg_tex: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer -H preamble.tex --listings -o bfpg.tex $<

elxir_pdf: elixir.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-H preamble.tex \
		-V 'mainfont:FiraSans-Regular.ttf' \
		-V 'mainfontoptions:Path=../common/fonts/' \
		-o elixir_types_now.pdf $<

bfpg_pdf: bfpg.org
	pandoc --standalone --slide-level=2 --to beamer \
		--pdf-engine=xelatex \
		-H preamble.tex \
		-o bfpg.pdf $<

watch: elixir.org bfpg.org
	find . -type f \( -iname "*.org" -o -iname "*.ex" -o -iname "*.tex" \) -not -path "./dialyzer_example/deps/*" -not -path "./gradient_example/deps/*" -not -path "./witchcraft_example/deps/*" | entr make $(VERSION)


.PHONY: all
