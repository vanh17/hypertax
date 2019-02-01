import java.io.File

import org.clulab.odin.{ExtractorEngine, Mention}
import org.clulab.processors.{Document, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Serializer
import ai.lum.common.FileUtils._


import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object makeDocument {

  val rules =
    """
      |- name: property-lexiconner
      |  label: Entity
      |  priority: 1
      |  type: token
      |  pattern: |
      |    [entity="B-Entity"] [entity="I-Entity"]*
      |
      |- name: hearst_pattern_such_as
      |  label: IsA
      |  priority: 2
      |  type: token
      |  pattern: |
      |    @hypernym: Entity such as @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
    """.stripMargin

  def main(args: Array[String]): Unit = {
    val lexiconfile = "Entity.txt"
    val lexicon_ner = LexiconNER(Seq(lexiconfile), caseInsensitiveMatching = true)
    val dir = new File(args(0))

    for (filename <- dir.listFilesByWildcard(".possf2")) {
      val docs = parseDoc(filename, lexicon_ner)

      val extractor_engine = ExtractorEngine(rules)
      for (doc <- docs) {
        val mentions = extractor_engine.extractFrom(doc)
        for {
          m <- mentions
          if m matches "IsA"
          rule = m.foundBy
          hypernym <- m.arguments("hypernym")
          hyponym <- m.arguments("hyponym")
        } {
          println(s"${hypernym.words.mkString(" ")}\t${hyponym.words.mkString(" ")}\t$rule\t${m.words.mkString(" ")}\n")
        }
      }
    }
//        for (docAndIndex <- docs.zipWithIndex) {
//          val doc = docAndIndex._1
//          val index = docAndIndex._2
//          val serName = s"${filename}_${index}.serialized"
//          Serializer.save(doc, serName)
//        }

  }
  def parseDoc(file: File, lex: LexiconNER): Array[Document] = {
    //  val a = new Document()
    val documents = new ArrayBuffer[Document]()
    var docSentences = new ArrayBuffer[Sentence]()
    var lineCounter = 0
    for (line <- Source.fromFile(file).getLines) {
      val sentence = line.trim.split(" ")
      lineCounter += 1
      if (sentence(0).length>0) { // skip the empty line
        val content = for {
          token <- sentence
          splitToken = token.split("_")
        } yield (splitToken(0), splitToken(1))
        val (words, tags) = content.unzip
        val numWords = words.length
        val procSentence = new Sentence(words, Array.fill[Int](numWords)(0), Array.fill[Int](numWords)(0), words)
        procSentence.tags = Some(tags)
        val ner = lex.find(procSentence)
        procSentence.entities = Some(ner)
        docSentences.append(procSentence)
        if (lineCounter >= 100) {
          val doc = new Document(docSentences.toArray)
          documents.append(doc)
          docSentences = new ArrayBuffer[Sentence]()
          lineCounter = 0
        }
      }
    }
    val doc = new Document(docSentences.toArray)
    documents.append(doc)

    documents.toArray
  }

}
