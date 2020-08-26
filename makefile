pdf: lmtp/lmtp.pdf

all: pdf

lmtp/lmtp.pdf: lmtp/lmtp.tex lmtp/lmtp-paper.bib
	cd lmtp; pdflatex lmtp
	cd lmtp; open lmtp.pdf