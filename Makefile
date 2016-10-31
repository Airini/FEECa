preview:
	stack build feeca
	stack exec doc rebuild
	echo Open localhost:8000
	stack exec doc preview
