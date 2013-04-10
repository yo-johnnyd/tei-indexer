import scala.xml._
import scala.util.matching.Regex
import org.codehaus.jackson.annotate.JsonIgnore
import org.codehaus.jackson.map.annotate.JsonSerialize
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion

@JsonSerialize(include=Inclusion.NON_NULL)
class TeiDocument(xmlSrc: NodeSeq, convertedContent: String) {

	// the content run through the Saxon XML to XHTML parser
	val contentInHTML = convertedContent

	// sections that we want to pull multiple elements out of
	@JsonIgnore
	val titleElement = xmlSrc \ "teiHeader" \ "fileDesc" \ "titleStmt"
	@JsonIgnore
	val body = xmlSrc \ "text" \ "body"
	@JsonIgnore
	val sourceDesc = xmlSrc \ "teiHeader" \ "fileDesc" \ "sourceDesc"
	@JsonIgnore
	val msIdentifier = sourceDesc \ "msDesc" \ "msIdentifier"
	@JsonIgnore
	val supportDesc = sourceDesc \ "msDesc" \ "physDesc" \ "objectDesc" \ "supportDesc"
	@JsonIgnore
	val letHeading = sourceDesc \ "msDesc" \ "letHeading"

	// id field
	val id = (msIdentifier \ "idno").text

	// addressee
	// (0) means get the first one you find (if there are more or not)
	// .text means get the contents of the element
	// So "Leeser" in <addressee>Leeser</addressee>
	val addressee: String = if((letHeading \ "addressee")(0).text != "") (letHeading \ "addressee")(0).text else null

	// author
	val author: String = if((titleElement \ "author")(0).text != "") (titleElement \ "author")(0).text else null

	// called "Source" or "Collection"
	val collection: String = if((msIdentifier \ "collection")(0).text != "") (msIdentifier \ "collection")(0).text else null

	// get all dateLet as written
	val dateLet = (letHeading \ "dateLet").map(_.text)

	// envelope
	val envelope: Boolean = ((sourceDesc \ "msDesc" \ "physDesc" \ "envDesc")(0) \ "@occ").text match {
		case "true" => true
		case "false" => false
	}

	// fragment
	val fragment: Boolean = ((supportDesc \ "condition")(0) \ "@fragment").text match {
		case "true" => true
		case "false" => false
	}

	// get all image file names
	val imgUrl = (sourceDesc \ "msDesc" \ "msContents" \ "msItem" \ "figure" \ "graphic")
		.map(_ \ "@url").map(_.text)

	// language
	val language: String = if((xmlSrc \\ "language")(0).text != "") (xmlSrc \\ "language")(0).text else null

	// material
	val material: String = if((supportDesc \ "support" \ "material")(0).text != "") (supportDesc \ "support" \ "material")(0).text else null

	// format of extent:
	// <extent># pages on # sheets
	//    <dimensions></dimensions>
	// </extent>
	@JsonIgnore
	val extent = (supportDesc \ "extent")(0).text
	// split the text "# pages on # sheets" into 5 element array
	// (#,"pages","on",#,"sheets")
	// or ("") if there was no text there
	@JsonIgnore
	val extentArray = extent.split(" ")

	// watch out for new line characters!
	val numberOfPages: java.lang.Integer = 
		if((extentArray(0) != "") && (extentArray(0) != "\n")) extentArray(0).toInt else null

	val numberOfSheets: java.lang.Integer =
		if(extentArray.length == 5) extentArray(3).toInt else null

	// first get placeLet, from which we will extract country, state, and city
	@JsonIgnore
	val placeLet = (letHeading \ "placeLet")(0).text

	// now decide values for country, state, and city based on these rules:
	// example 1: Greensboro, AL
	// example 2: Paris, France
	// The first word is the city
	// If the text after the comma is only two letters, that's the state and the country is USA
	// If the text after the comma is more than two letters, that's the country and there is no state
	@JsonIgnore
	val placeLetArray = placeLet.split(",")

	// placeLet city
	val placeLetCity = if(placeLet != ""){ placeLetArray(0).trim }else null

	// placeLet state
	val placeLetState = if((placeLet != "") && (placeLetArray(1).trim.length == 2)){ placeLetArray(1).trim }else null

	// placeLet country
	val placeLetCountry = if((placeLet != "") && (placeLetArray(1).trim.length == 2)){ "USA" } 
		else if(placeLet != "") { placeLetArray(1).trim }
		else null

	// first get placeRec, from which we will extract country, state, and city
	@JsonIgnore
	val placeRec = (letHeading \ "placeRec")(0).text

	// now decide values for country, state, and city based on these rules:
	// example 1: Greensboro, AL
	// example 2: Paris, France
	// The first word is the city
	// If the text after the comma is only two letters, that's the state and the country is USA
	// If the text after the comma is more than two letters, that's the country and there is no state
	@JsonIgnore
	val placeRecArray = placeRec.split(",")

	// placeRec city
	val placeRecCity = if(placeRec != ""){ placeRecArray(0).trim }else null

	// placeRec state
	val placeRecState = if((placeRec != "") && (placeRecArray(1).trim.length == 2)){ placeRecArray(1).trim }else null

	// placeRec country
	val placeRecCountry = if((placeRec != "") && (placeRecArray(1).trim.length == 2)){ "USA" } 
		else if(placeRec != "") { placeRecArray(1).trim }
		else null

	// repository
	val repository = (msIdentifier \ "repository")(0).text

	// script
	val script = ((sourceDesc \ "msDesc" \ "physDesc" \ "handDesc")(0) \ "@script").text

	// signature
	val signature: String = ((titleElement \ "author")(0) \ "@signature").text

	// title
	// many of the titles say "[no title given]" but we don't actually want to store that so we just store blank instead
	@JsonIgnore
	val titleNoTitle = (titleElement \ "title").text
	val title: String = if((titleNoTitle == "[no title given]") || (titleNoTitle == "")) null else titleNoTitle

	// wax seal
	// true if it contains "Wax Seal", false otherwise
	val waxSeal: Boolean = 
		if((sourceDesc \ "msDesc" \ "physDesc" \ "sealDesc")(0).text.toLowerCase == "wax seal") true else false

	// year parsed from dateLet as first 4 digits
	// the first dateLet is from the gregorian calendar
	// the second dateLet is from the Hebrew calendar
	@JsonIgnore
	val yearRegex = new Regex("""\d\d\d\d""")
	@JsonIgnore
	val dateGregorian = ((letHeading \ "dateLet")(0) \ "@when").text
	// have to check first b/c if there isn't too dateLets
	// and array out of bounds exception will be thrown
	@JsonIgnore
	val dateHebrew = 
		if((letHeading \ "dateLet").length > 1) ((letHeading \ "dateLet")(1) \ "@when").text else ""
	// extract gregorian year
	val yearGregorian: java.lang.Integer = 
		(yearRegex findFirstIn dateGregorian) match {
			case Some(s) => s.toInt
			case None => null
		}
	// extract Hebrew year
	val yearHebrew: java.lang.Integer = 
		(yearRegex findFirstIn dateHebrew) match {
			case Some(s) => s.toInt
			case None => null
		}


	// put a space afer each </p> so when all the elements are stripped out, the text is still readable
	// get rid of the multiple tei xmlns attributes Scala XML adds to <body> on the toString method
	@JsonIgnore
	val cleanBodyString = body.toString
		.replaceAll("</p>", "</p> ")
			.replace(
				"<body xmlns=\"http://www.tei-c.org/ns/1.0\" xmlns=\"http://www.tei-c.org/ns/1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://www.tei-c.org/ns/1.0\"",
				"<body xmlns=\"http://www.tei-c.org/ns/1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"")
	val content = XML.loadString(cleanBodyString).text

	// need getters so Jackson JSON mapper can call them

	def getId(): String = { id }

	def getAddressee(): String = { addressee }

	def getAuthor(): String = { author }

	def getCollection(): String = { collection }

	def getContentInHTML(): String = { contentInHTML }

	def getDateLet(): Seq[String] = { dateLet }

	def getEnvelope(): Boolean = { envelope }

	def getFragment(): Boolean = { fragment }

	def getImgUrl(): Seq[String] = { imgUrl }

	def getLanguage(): String = { language }

	def getMaterial(): String = { material }

	def getNumberOfPages(): java.lang.Integer = { numberOfPages }

	def getNumberOfSheets(): java.lang.Integer = { numberOfSheets }

	def getPlaceLetCity(): String = { placeLetCity }

	def getPlaceLetState(): String = { placeLetState }

	def getPlaceLetCountry(): String = { placeLetCountry }

	def getPlaceRecCity(): String = { placeRecCity }

	def getPlaceRecState(): String = { placeRecState }

	def getPlaceRecCountry(): String = { placeRecCountry }

	def getRepository(): String = { repository }

	def getScript(): String = { script }

	def getSignature(): String = { signature }

	def getTitle(): String = { title }

	def getWaxSeal(): Boolean = { waxSeal }

	def getYearGregorian(): java.lang.Integer = { yearGregorian }

	def getYearHebrew(): java.lang.Integer = { yearHebrew }

	def getContent(): String = { content }

}