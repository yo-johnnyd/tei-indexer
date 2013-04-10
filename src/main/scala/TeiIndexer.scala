import scala.xml._
import scala.util.matching.Regex
import org.codehaus.jackson.map.ObjectMapper
import com.fasterxml.jackson.module.scala._
import org.slf4j.{LoggerFactory}
import java.io.File
import org.apache.commons.io.FileUtils
import javax.xml.transform._
import javax.xml.transform.stream._
import java.io.StringWriter
import java.io.InputStreamReader
import java.io.FileInputStream
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.entity.ContentType


object TeiIndexer {
 private val objectMapper: ObjectMapper = new ObjectMapper()
 objectMapper.registerModule(DefaultScalaModule)
 private lazy val logger = LoggerFactory.getLogger(getClass)

 def main(args: Array[String]): Unit = {

	if (args.length != 2) {

		// check to see that the directories or files passed in exist
		val teiFilesToIndex = new File(args(0))
		val teiFilesAlreadyIndexed = new File(args(1))
		val teiFilesWithProblems = new File(args(2))
		val teiStylesheet = new File(args(3))

		if (teiFilesToIndex.exists 
			&& teiFilesAlreadyIndexed.exists 
			&& teiStylesheet.exists
			&& teiFilesWithProblems.exists){
			for (file <- teiFilesToIndex.listFiles) {

				logger.info("Begin processing: " + file.getName())

				// read the file as XML
				val xmlSrc = XML.loadFile(file)

				// convert the content to html
				// Create a transform factory instance.
        		val tfactory: TransformerFactory = TransformerFactory.newInstance("net.sf.saxon.TransformerFactoryImpl",null);
        		// Create a transformer for the stylesheet.
				val transformer: Transformer = tfactory.newTransformer(new StreamSource(teiStylesheet))
				// create result stream string writer
				val stringResultWriter: StringWriter = new StringWriter()

				// Transform the source XML to System.out.
 				transformer.transform(new StreamSource(file), new StreamResult(stringResultWriter))
 				val htmlResultString = stringResultWriter.toString

 				// try to process the TEI file. if any exception is thrown, move the file to teiFilesWithProbelms directory
 				try{

					// create a JSON represntation of the document 
					// add brackets to json to make it an array of one object
					// solr throws bad request if object is not part of a json array

					val teiDocument = new TeiDocument(xmlSrc, htmlResultString)
					val json = "[" + objectMapper.writeValueAsString(teiDocument) + "]"

					// send the json to solr for indexing
					// create an HttpPost object
	  				val post = new HttpPost("http://localhost:8080/solr/leeser/update/json?commit=true")
	  				// for local testing with node.js
	  				//val post = new HttpPost("http://localhost:3000")

	  				// set the Content-type
	  				post.setHeader("Content-type", "application/json; charset=utf-8")

	  				// add the JSON as a StringEntity
	  				post.setEntity(new StringEntity(json, ContentType.APPLICATION_JSON))

	  				// send the post request
	  				val response = (new DefaultHttpClient).execute(post)

	  				// get the response status
	  				val responseStatus = response.getStatusLine().getStatusCode()

	  				// check  if status was successful (200)
	  				// if Solr instance could not be found (404)
	  				// or some other error code
					if (responseStatus == 200) {
						FileUtils.copyFileToDirectory(file, teiFilesAlreadyIndexed)
						file.delete()
						logger.info(teiDocument.getId() + ": processed and moved to " + teiFilesAlreadyIndexed.getName)
					}else if(responseStatus == 404){
						logger.error(teiDocument.getId() + ": Solr instance not found (404)")
					}else{
						logger.error(teiDocument.getId() + ": Solr http error code: " + responseStatus)
					}
				} catch {
  					case e: Exception => {
  						FileUtils.copyFileToDirectory(file, teiFilesWithProblems)
  						file.delete()
  						logger.error(file.getName + ": FAILED and moved to " + teiFilesWithProblems.getName
  							+ " with exception " + e);
  					};
				}
			}
		}else {
			logger.error("Cannot find one or more of the directories or files passed in.")
		}
	}else {
	   logger.error("Welcome to the TEI file indexer.")
	   logger.error("Please pass in these three arguments: ")
	   logger.error("1) Directory of TEI files to index ")
	   logger.error("2) Directory to move successfully indexed TEI files to ")
	   logger.error("3) Directory to move failed TEI files to ")
	   logger.error("4) Tei conversion stylesheet ")
	   logger.error("Example: run teiFilesToIndex teiFilesAlreadyIndexed teiFilesWithProblems teiStylesheets/profiles/default/html/to.xsl")
   }
 }
}
