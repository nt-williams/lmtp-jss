pdf: lmtp/lmtp.pdf

all: pdf

lmtp/lmtp.pdf: lmtp/lmtp.tex lmtp/lmtp.bib
	cd lmtp; pdflatex lmtp
	cd lmtp; bibtex lmtp
	cd lmtp; pdflatex lmtp
	cd lmtp; pdflatex lmtp
	cd lmtp; open lmtp.pdf