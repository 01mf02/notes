all: README.html

%.html: %.md
	pandoc --mathjax -s $< -o $@
