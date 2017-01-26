all: README.html

%.html: %.md
	pandoc --mathjax -s $< -o $@

%.pdf: %.md
	pandoc $< -o $@
