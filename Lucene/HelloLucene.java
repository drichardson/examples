import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.*;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.*;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.RAMDirectory;
import org.apache.lucene.util.Version;

import java.io.IOException;

public class HelloLucene {
	
  	public static void main(String[] args) throws IOException, ParseException {
		// 0. Specify the analyzer for tokenizing text.
		//    The same analyzer should be used for indexing and searching
		StandardAnalyzer analyzer = new StandardAnalyzer(Version.LUCENE_33);

		// 1. create the index
		Directory index = new RAMDirectory();

		// the boolean arg in the IndexWriter ctor means to
		// create a new index, overwriting any existing index
		IndexWriterConfig config = new IndexWriterConfig(Version.LUCENE_33, analyzer);
		IndexWriter w = new IndexWriter(index, config);
		addDoc(w, "Lucene in Action");
		addDoc(w, "Lucene for Dummies");
		addDoc(w, "Managing Gigabytes");
		addDoc(w, "The Art of Computer Science");
		addDoc(w, "Doug's Twitter Page");
		addDoc(w, "Rebecca's Twitter Page");
		addDoc(w, "Google Plus");
		addDoc(w, "Going Places");
		w.close();

		// 2. query
		String querystr = args.length > 0 ? args[0] : "lucene";

		// the "title" arg specifies the default field to use
		// when no field is explicitly specified in the query.
		Query q = new QueryParser(
		Version.LUCENE_33, "title", analyzer).parse(querystr);

		// 3. search
		int hitsPerPage = 10;
		IndexSearcher searcher = new IndexSearcher(index, true);
		TopScoreDocCollector collector = 
		TopScoreDocCollector.create(hitsPerPage, true);
		searcher.search(q, collector);
		ScoreDoc[] hits = collector.topDocs().scoreDocs;

		// 4. display results
		System.out.println("Found " + hits.length + " hits.");
		for(int i=0;i<hits.length;++i) {
			int docId = hits[i].doc;
			Document d = searcher.doc(docId);
			System.out.println((i + 1) + ". " + d.get("title"));
		}

		// searcher can only be closed when there
		// is no need to access the documents any more. 
		searcher.close();
	}

	private static void addDoc(IndexWriter w, String value) throws IOException {
		Document doc = new Document();
		doc.add(new Field("title", value, Field.Store.YES, Field.Index.ANALYZED));
		w.addDocument(doc);
	}
}