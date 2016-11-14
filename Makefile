preview:
	stack build feeca
	stack exec doc rebuild
	echo Open localhost:8000
	stack exec doc preview

redoc-cabal:
	cabal run doc rebuild
	cabal run doc check
	open http://0.0.0.0:8000
	cabal run doc server
