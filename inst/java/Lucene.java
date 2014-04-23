import java.io.File;
import java.io.IOException;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.highlight.Formatter;
import org.apache.lucene.search.highlight.SimpleHTMLFormatter;
import org.apache.lucene.search.highlight.TokenSources;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;
public class Lucene {
	public Analyzer SA;
	public Version version;
	public Directory index;
	public IndexReader reader;
	public IndexSearcher searcher;
	public SimpleHTMLFormatter shtml;
	String[] fields = {"notebook_id","description","created_at","updated_at","content","starcount","avatar_url","user_url","commited_at","user"};
	public MultiFieldQueryParser multiparser;
	public TokenSources ts;
	public static void main (String[] args) throws Exception {
	}
	
	public Lucene() throws IOException{ 
		version = Version.LUCENE_47;
		SA = new StandardAnalyzer(version);
		index = FSDirectory.open(new File("/vagrant/work/solr-4.5.1/example/solr/rcloudnotebooks/data/index/"));
		reader = DirectoryReader.open(index);
		searcher = new IndexSearcher(reader);
		multiparser = new MultiFieldQueryParser(version, fields, SA);
		shtml = new SimpleHTMLFormatter("<b style='background:yellow'>", "</b>");
		ts = new TokenSources();
	}
}
