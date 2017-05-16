LIBRARY_N := feeca
LIBRARY_D := FEECa
BINDIR    := dist/build
PROF      := prof
TEST      := test-feec
BENCH     := bench
PROFCAB   := $(BINDIR)/$(PROF)
BENCHCAB  := $(BINDIR)/$(BENCH)
PROFBIN   := $(PROFCAB)/$(PROF)
BENCHBIN  := $(BENCHCAB)/$(BENCH)
PLOTDIR   := $(BENCH)/$(LIBRARY_D)

PRFLAGS   := --enable-profiling
BMFLAGS   := --enable-benchmarks
TFFLAGS   := --enable-tests
ConfFLAGS =

BuildSPEC =


default:
	cabal configure $(ConfFLAGS)
	cabal build $(BuildSPEC)

redoc:
	cabal run doc rebuild
	cabal run doc check
	open http://0.0.0.0:8000
	cabal run doc server

preview:
	stack build feeca
	stack exec doc rebuild
	echo Open localhost:8000
	stack exec doc preview

feeca: BuildSPEC += $(LIBRARY_N)
feeca: default

%.prof: ConfFLAGS += $(PRFLAGS)

prof:   ConfFLAGS += $(PRFLAGS)
prof:   BuildSPEC += $(PROF)

bench:  ConfFLAGS += $(BMFLAGS)
bench:  BuildSPEC += $(BENCH)

tests:  ConfFLAGS += $(TFFLAGS)
tests:  BuildSPEC += $(TEST)

prof:  default
bench: default
tests: default

profile: prof
	./$(PROFBIN)

benchmark.%: bench
	./$(BENCHBIN) &> xxxxBENCHxxxx

test-feec.%: tests
	cabal test

clean-prof:
	rm -f *.tix *.hp *.aux $(PROFBIN)

clean: clean-prof
	cabal clean

clean-data: clean
	rm *.prof *.dat
