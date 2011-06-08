all :
	ghc --make -Wall Nucleotide.hs

clean :
	rm -f Nucleotide tNucleotide *.hi *.o report.html

test :
	ghc --make -Wall tNucleotide.hs
	./tNucleotide

prove :
# cpan App::Prove
	prove --exec make test


provehtml :
# cpan App::Prove::Plugin::HTML
	prove -P HTML=outfile:report.html --exec make test
