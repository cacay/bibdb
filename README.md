# bibdb

bibdb is a simple citation resolver and bibliography manager for BibTeX. Instead of manually copy pasting bibliographies, you specify a reference to the work you want to cite (by providing a DOI or a DBLP key, for example) in a simple domain specific language. bibdb is then responsible for fetching the citations, renaming them, removing duplicates, and sorting them properly (to handle cross-references).

# Usage

A bibdb file might look like the following:
```
-- Papers
DBLP:books/aw/Knuth68
DBLP:conf/fossacs/PfenningG15 as PfenningG15
DOI:10.1145/800157.805047 as Cook71

-- Conferences
DBLP:conf/popl/2016 as Popl16
```

Note how we can use comments (with `--` or `{- ... -}`) and rename references using `as`. References without an explicit name inherit the source address. For example, the first reference will have the name `DBLP:books/aw/Knuth68` in the generated bibliography file. After we create the file, we invoke bibdb:
```
bibdb references.bibdb -o references.bib
```
(here, we assume the file is named `references.bibdb`). bibdb can generate compressed citations or full citations with cross-references where available. For more information on the allowed commands, see `bibdb --help`.


# Comparison to Similar Tools

* [dblp](https://github.com/grundprinzip/dblp) is a command line tool that parses a TeX file and resolves all references to DBLP. This makes it unnecessary to maintain a separate file. However, dblp cannot resolve other types of references, and it gives no way to rename them.

* [citation-resolve](https://github.com/nushio3/citation-resolve) is a package for resolving citations. It can handle more types of references but it is not a tool on its own. We might use it internally in the future.


# Language Specification

bibdb files have a very simple grammar. `--` is used for single line comments and `{- ... -}` for multi-line nested comments. Each entry is of the form `<source-type> : <source-key> [as <name>]`. A file is simply a list of entries.

Currently, `<source-type>` can only be `DBLP` or `DOI`. We may add more in the future.

