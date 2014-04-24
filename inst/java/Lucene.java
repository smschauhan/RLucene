import java.io.File;
import java.io.IOException;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.highlight.Formatter;
import org.apache.lucene.search.highlight.Fragmenter;
import org.apache.lucene.search.highlight.Highlighter;
import org.apache.lucene.search.highlight.QueryScorer;
import org.apache.lucene.search.highlight.SimpleHTMLFormatter;
import org.apache.lucene.search.highlight.SimpleSpanFragmenter;
import org.apache.lucene.search.highlight.TextFragment;
import org.apache.lucene.search.highlight.TokenSources;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.MMapDirectory;


import org.apache.lucene.util.Version;
import org.json.JSONObject;

public class Lucene {
	public Analyzer SA;
	public Version version;
	public MMapDirectory index;
	public IndexReader reader;
	public IndexSearcher searcher;
	public Formatter shtml;
	String[] fields = {"notebook_id","description","created_at","updated_at","content","starcount","avatar_url","user_url","commited_at","user"};
	boolean unmapHack = true;
  	private int maxChunk ;
	public MultiFieldQueryParser multiparser;
	public static void main (String[] args) throws Exception {
	}
	
	public Lucene(String path) throws IOException{ 
		
		version = Version.LUCENE_47;
		SA = new StandardAnalyzer(version);
		MMapDirectory index = new MMapDirectory(new File(path));
		try {
      			index.setUseUnmap(unmapHack);
		 } catch (Exception e) {
      			System.out.println("Unmap not supported on this JVM, continuing on without setting unmap\n" + e);
    		}
    		//index.setMaxChunkSize(maxChunk);
	
		reader = DirectoryReader.open(index);
		searcher = new IndexSearcher(reader);
		multiparser = new MultiFieldQueryParser(version, fields, SA);
		shtml = new SimpleHTMLFormatter("<b style='background:yellow'>", "</b>");
	}
	public String getResults(String query) throws Exception{
		//ArrayList<String> results = new ArrayList<String>();
		Query q = multiparser.parse(query);
        //Long start = System.currentTimeMillis();
        TopDocs hits = searcher.search(q,1000);
        //Long end = System.currentTimeMillis();
        //System.out.println(end-start);
        //System.out.println(hits.totalHits);
        QueryScorer scorer = new QueryScorer(q, "content");
        Highlighter highlighter = new Highlighter(shtml, scorer);
        highlighter.setMaxDocCharsToAnalyze(900000000);
        Fragmenter fragmenter = new SimpleSpanFragmenter(scorer);
        highlighter.setTextFragmenter(fragmenter);
        JSONObject json = new JSONObject();
        for (ScoreDoc sd : hits.scoreDocs) {
        	Document doc = searcher.doc(sd.doc);
        	String content = doc.get("content");
            TokenStream stream = TokenSources.getAnyTokenStream(searcher.getIndexReader(), sd.doc, "content", doc, SA);
            TextFragment[] frag = highlighter.getBestTextFragments(stream, content, true, 10000000);
            //System.out.println(frag);
            String x=null;
            if ((frag[0] != null) && (frag[0].getScore() > 0)) {
                     x = frag[0].toString();
            }
            json.put(doc.get("notebook_id"),x);
        }
		return json.toString();
	} 
}
