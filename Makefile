LIBRARY_N := feeca
LIBRARY_D := FEECa
#BINDIR    := dist-newstyle/build
PROF      := prof
TEST      := test-feec
BENCH     := bench
#PROFCAB   := $(BINDIR)/$(PROF)
#BENCHCAB  := $(BINDIR)/$(BENCH)
#PROFBIN   := $(PROFCAB)/$(PROF)
#BENCHBIN  := $(BENCHCAB)/$(BENCH)
PLOTDIR   := $(BENCH)/$(LIBRARY_D)

PRFLAGS   := --enable-profiling
BMFLAGS   := --enable-benchmarks
TFFLAGS   := --enable-tests
ConfFLAGS =

BuildSPEC =

TSTAMP    := $(shell date +'%y%m%d_%H%M%S')

default:
	cabal new-configure $(ConfFLAGS)
	cabal new-build $(BuildSPEC)

redoc:
	cabal new-run doc rebuild
	cabal new-run doc check
	open http://0.0.0.0:8000
	cabal new-run doc server

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
	cabal new-run prof

benchmark.%: bench
	cabal new-bench &> $(addsuffix _$(TSTAMP),$(BENCH))

test-feec.%: tests
	cabal new-test

clean-prof:
	rm -f *.tix *.hp *.aux

clean: clean-prof
	cabal new-clean

clean-data: clean
	rm -f *.prof *.dat
