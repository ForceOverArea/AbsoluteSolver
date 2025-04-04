
AbsoluteSolverObj: cabal_project haskell_bindings

cabal_project:
	cd ./Cabal && \
		cabal build && \
		cabal install --lib --force-reinstalls

haskell_bindings:
	cd ./Haskell && \
		ghc -c C.hs

rust_project:
	cd ./