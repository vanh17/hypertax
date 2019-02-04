import java.io._

import org.clulab.odin.{ExtractorEngine, Mention}
import org.clulab.processors.{Document, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Serializer
import ai.lum.common.FileUtils._
import scala.collection.parallel.ForkJoinTaskSupport


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
      |- name: hearst_pattern_other_than
      |  label: IsA
      |  priority: 3
      |  type: token
      |  pattern: |
      |    @hypernym: Entity other than @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_not_all
      |  label: IsA
      |  priority: 4
      |  type: token
      |  pattern: |
      |    @hypernym: not all Entity are @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_including
      |  label: IsA
      |  priority: 5
      |  type: token
      |  pattern: |
      |    @hypernym: Entity (which)? (including|include|includes) @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_especially
      |  label: IsA
      |  priority: 6
      |  type: token
      |  pattern: |
      |    @hypernym: Entity especially @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_like
      |  label: IsA
      |  priority: 7
      |  type: token
      |  pattern: |
      |    @hypernym: Entity like @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_for_example
      |  label: IsA
      |  priority: 8
      |  type: token
      |  pattern: |
      |    @hypernym: Entity for example @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_are_also
      |  label: IsA
      |  priority: 9
      |  type: token
      |  pattern: |
      |    @hypernym: Entity (are also|also are) @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_are_all
      |  label: IsA
      |  priority: 10
      |  type: token
      |  pattern: |
      |    @hypernym: Entity (are all|all are) @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_not_Y_so_much_as_X
      |  label: IsA
      |  priority: 11
      |  type: token
      |  pattern: |
      |    @hypernym: not Entity so much as @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
      |- name: hearst_pattern_Y_not_so_much_as_X
      |  label: IsA
      |  priority: 12
      |  type: token
      |  pattern: |
      |    @hypernym: Entity not so much as @hyponym: Entity (',' (and|or)? @hyponym: Entity)*
    """.stripMargin

  def main(args: Array[String]): Unit = {
    if (args.size != 3) {
      println("usage: path_to_corpora path_to_output_file numbers_of_cores")
    }
    val lexiconfile = "Entity.txt"
    val lexicon_ner = LexiconNER(Seq(lexiconfile), caseInsensitiveMatching = true)
    val dir = new File(args(0))
    val fileList = dir.listFilesByWildcard("*.possf2").toArray.par
    val tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(args(2).toInt))
    fileList.tasksupport = tasksupport 
    for (filename <- fileList) {
      println(s"${filename}")
      var docs = parseDoc(filename, lexicon_ner)
      var pw = new PrintWriter(new File(args(1)+"/"+filename.toString.split("/").last+".txt"))
      var extractor_engine = ExtractorEngine(rules)
      // println("start hypertaxing...")
      for (doc <- docs) { 
        var mentions = extractor_engine.extractFrom(doc)
        for {
          m <- mentions
          if m matches "IsA"
          rule = m.foundBy
          hypernym <- m.arguments("hypernym")
          hyponym <- m.arguments("hyponym")
        } {
          // val sentenceText = m.text
          val out = s"${hypernym.words.mkString(" ")}\t${hyponym.words.mkString(" ")}\t$rule\t${m.text}\n"
          println(out)
          pw.write(out)
        }
      }
      pw.close()
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
      try {
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
      } catch {
        case _: Throwable => () // println("exception ignored")
      }
    }
    val doc = new Document(docSentences.toArray)
    documents.append(doc)

    documents.toArray
  }

}
