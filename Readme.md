## Nonce word generator

Generates nonce words for a colleague's experiment (will link paper when it's published) with specific parameters:

- syllable structure
- distance from one another
- number of neighbours

Words are generated for two tasks.

## Task 1

Nonce words need to be matched to target words of shape cvcv or cvcvc. Should have as many lexical neighbours as target word (where lexical neigbour is an existing word w/ an edit distance of 1). Where this is not possible, should has about as many (+/- 1-2) neighbours as target word.

## Task 2

Nonce words need to be drawn in groups of four (cvcv shapes) or six (cvcvc shapes). Words in groups should have "b" in specific points in the structure, should be at an edit distance of 3 from one another, should have 0 lexical neighbours, should not repeat across groups.

## TOC

 - alszo_new_code.R: generates tables in out from tables in src
 - out
   - alszavak_negyesek_cvcvc.tsv: groups of words in task 2, cvcvc template
   - cvcvc_alszavak.tsv: matched words in task 1, cvcv
   - alszavak_negyesek_cvcv.tsv: groups of words in task 2, cvcvc template
   - alszavak_szomszedokkal.tsv: nonce words in big bag, with neighbour counts
   - cvcv_alszavak.tsv: matched words in task 2, cvcv
 - src
   - celszavak.txt: target words for nonce words
   - cel_proc.tsv: target words for nocne words, processed
   - hu_list.txt: spelling dict of Hungarian used in [Hunspell](http://hunspell.github.io/), used here to count lexical neighbours.
   - hun_proc.tsv: Hungarian word list, processed