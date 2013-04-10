tei-indexer
===========

Scala program for extracting information from [TEI](http://www.tei-c.org/index.xml) xml files and sending it to Solr in JSON format

Wrote this as part of a project where the client had several thousand xml files in [TEI](http://www.tei-c.org/index.xml) format that they wanted to be able to do full text searches on.
I used [Apache Solr](http://lucene.apache.org/solr/) for full-text indexing, search, and storage.

