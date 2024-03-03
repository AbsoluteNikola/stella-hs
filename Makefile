.PHONY: generate
generate:
	bnfc --haskell --text-token --functor --name-space="Stella.Ast" --generic --outputdir=src ./Syntax.cf
	rm -f src/Stella/Ast/*.bak
	rm -f src/Stella/Ast/DocSyntax.txt
# rm -f src/Stella/Ast/LexSyntax.x
# rm -f src/Stella/Ast/ParSyntax.y
# 	rm -f src/Stella/Ast/TestSyntax.hs
	rm -f src/Stella/Ast/ErrM.hs
	rm -f src/Stella/Ast/SkelSyntax.hs
