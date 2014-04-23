.convert.version <- function(version) {
  version <- if (length(version) < 1L || any(is.na(version)))
    "LUCENE_CURRENT" else paste0("LUCENE_", gsub(".", "", sprintf("%.1f", as.numeric(version)), fixed=TRUE))
  version <- tryCatch(.jfield("org.apache.lucene.util.Version",,version),
                      error=function(e) NULL)
  if (is.jnull(version)) stop("Lucene version %.1f format is not supported", as.numeric(version))
  version
}

lucene.writer <- function(directory, analyzer="standard", version=NA) {
  version <- .convert.version(version)
  analyzer <- .jnew("org.apache.lucene.analysis.standard.StandardAnalyzer", version)
  dir <-.jcall("org/apache/lucene/store/FSDirectory", "Lorg/apache/lucene/store/FSDirectory;", "open", .jnew("java/io/File", as.character(directory)[1L]))
  iwc <- .jnew("org.apache.lucene.index.IndexWriterConfig", version, .jcast(analyzer, "org/apache/lucene/analysis/Analyzer"))
  iw <- .jnew("org.apache.lucene.index.IndexWriter", .jcast(dir, "org/apache/lucene/store/Directory"), iwc)
  structure(list(writer=iw, directory=dir, dir.name=directory), class="LuceneIndexWriter")
}

close.LuceneIndexWriter <- function(con, ...) {
  .jcall(con$writer, "V", "close")
  .jcall(con$directory, "V", "close")
}

# methods to manipulate the index
add.document <- function(where, doc, ...) UseMethod("add.document")
update.document <- function(where, ...) UseMethod("update.document")
delete.documents <- function(where, ...) UseMethod("delete.documents")

# flag value as stored
stored <- function(x) { attr(x, "lucene.store") <- TRUE; x }
not.stored <- function(x) { attr(x, "lucene.store") <- FALSE; x }

.construct.doc <- function(doc, store.default=TRUE) {
  if (!is.list(doc)) doc <- list(text=paste(as.character(doc), collapse='\n'))
  if (is.null(names(doc))) names(doc) <- rep("text", length(doc))
  d <- .jnew("org.apache.lucene.document.Document")
  NO <- .jfield("org/apache/lucene/document/Field$Store",,"NO")
  YES <- .jfield("org/apache/lucene/document/Field$Store",,"YES")
  for (i in seq.int(length(doc))) {
    name <- names(doc)[i]
    value <- doc[[i]]
    store <- attr(value, "lucene.store")
    if (is.null(store)) store <- store.default
    type <- attr(value, "lucene.type")
    if (is.null(type)) type <- if (inherits(value, "AsIs")) "StringField" else "TextField"
    f <- if (type == "TextField") .jnew("org.apache.lucene.document.TextField", name, value, if(isTRUE(store)) YES else NO) else
    if (type == "StringField") .jnew("org.apache.lucene.document.StringField", name, value, if(isTRUE(store)) YES else NO) else
    stop("unsupported filed type `", type, "'")
    .jcall(d, "V", "add", .jcast(f, "org/apache/lucene/index/IndexableField"))
  }
  d
}

add.document.LuceneIndexWriter <- function(where, doc, ..., store.default=TRUE) {
  .jcall(where$writer, "V", "addDocument", .jcast(.construct.doc(doc, store.default), "java/lang/Iterable"))
}

update.document.LuceneIndexWriter <- function(where, field, value, doc, ..., store.default=TRUE) {
  .jcall(where$writer, "V", "updateDocument", .jnew("org.apache.lucene.index.Term", as.character(field), paste(as.character(value), collapse="\n")),
         .jcast(.construct.doc(doc, store.default), "java/lang/Iterable"))
}

delete.documents.LuceneIndexWriter <- function(where, field, value, ...)
  if (missing(field) && missing(value))
    .jcall(where$writer, "V", "deleteAll") else
    .jcall(where$writer, "V", "deleteDocuments", .jnew("org.apache.lucene.index.Term", as.character(field), paste(as.character(value), collapse="\n")))

## convenience methods that create a writer and close it essentially preforming an atomic operation

add.document.character <- function(where, doc, ..., analyzer="standard", version=NA, store.default=TRUE) {
  w <- lucene.writer(where, analyzer=analyzer, version=version)
  on.exit(close(w))
  add.document(w, doc, ..., store.default=store.default)
}

update.document.character <- function(where, field, value, doc, ..., analyzer="standard", version=NA, store.default=TRUE) {
  w <- lucene.writer(where, analyzer=analyzer, version=version)
  on.exit(close(w))
  update.document(w, field, value, doc, ..., store.default=store.default)
}

delete.documents.character <- function(where, field, value, ..., analyzer="standard", version=NA, store.default=TRUE) {
  w <- lucene.writer(where, analyzer=analyzer, version=version)
  on.exit(close(w))
  if (missing(field) && missing(value)) delete.documents(w, ..., store.default=store.default) else
  delete.documents(w, field, value, ..., store.default=store.default)
}

## convert a document from Java format to a list of fields
.convert.doc <- function(doc) {
  fs <- .jevalArray(doc$getFields()$toArray(), "[Lorg/apache/lucene/index/IndexableField;")
  names <- sapply(fs, .jcall, "Ljava/lang/String;", "name")
  values <- sapply(fs, .jcall, "Ljava/lang/String;", "stringValue")
  types <- lapply(fs, .jcall, "Lorg/apache/lucene/index/IndexableFieldType;", "fieldType")
  stored <- sapply(types, .jfield, "Z", "stored")
  tokenized <- sapply(types, .jfield, "Z", "tokenized")
  v <- lapply(seq.int(length(values)), function(i) { v <- values[i]; if (!tokenized[i]) v <- I(v); v })
  names(v) <- names
  v
}

lucene.query <- function(directory, field, query, version=NA) {
  lo <- LuceneObjects()
  a2 <- .jcast(lo$SA, "org/apache/lucene/analysis/Analyzer")
  on.exit(.jcall(lo$index, "V", "close"))
  on.exit(.jcall(lo$reader, "V", "close"), TRUE)
  q <- .jcall(lo$multiparser, "Lorg/apache/lucene/search/Query;", "parse", query)
  rset <- .jcall(lo$searcher, "Lorg/apache/lucene/search/TopDocs;", "search", q, .jnull("org/apache/lucene/search/Filter"), 1000L)
  res <- .jfield(rset,,"scoreDocs")
  qs <- .jnew("org.apache.lucene.search.highlight.QueryScorer", q, as.character("content"))
  scorer <- .jcast(qs, "org/apache/lucene/search/highlight/Scorer")
  hl <- .jnew("org.apache.lucene.search.highlight.Highlighter", .jcast(lo$shtml, "org/apache/lucene/search/highlight/Formatter"), scorer)
  .jcall(hl,"V", "setMaxDocCharsToAnalyze", as.integer(900000000))
  s_f <- .jnew("org.apache.lucene.search.highlight.SimpleSpanFragmenter",qs)
  t_f <- .jcall(hl, "V", "setTextFragmenter", .jcast(s_f,"org/apache/lucene/search/highlight/Fragmenter"))
  reader <- .jcall(lo$searcher, "Lorg/apache/lucene/index/IndexReader;", "getIndexReader")
  output <- list()
  cont.string <- list()
  if(length(res) != 0) {
	for(i in 1:length(res)) {
		document <- .jcall(lo$searcher, "Lorg/apache/lucene/document/Document;", "doc", rset$scoreDocs[[i]]$doc)
		content <- document$get("content")
		stream <- .jcall(lo$ts, "Lorg/apache/lucene/analysis/TokenStream;", "getAnyTokenStream", reader, rset$scoreDocs[[i]]$doc, "content", document, a2)
		tokenstream <- .jcast(stream , "org/apache/lucene/analysis/TokenStream")
		frag <- .jcall(hl, "[Lorg/apache/lucene/search/highlight/TextFragment;", "getBestTextFragments", tokenstream, content, TRUE , as.integer(10000000),evalArray=TRUE)
		if(length(frag) > 0) {
		for(j in 1:length(frag)) {
			if (!is.null(frag[[j]]) && frag[[j]]$getScore() >0) {
				cont.string[[j]] <- frag[[j]]$toString()
			}
		}
	  }	
		output[[i]]<-cont.string
    }
  } else output
  m <- sapply(res, function(o) c(.jfield(o,,"doc"), .jfield(o,,"score")))
  if (length(m)) {
    df <- data.frame(id=as.integer(m[1,]), score=m[2,])
	df$output <- output
    df$docs <- lapply(df$id, function(i) .convert.doc(lo$searcher$doc(i)))
    df
  } else data.frame(id=integer(0), score=numeric(0), docs=list())
}
