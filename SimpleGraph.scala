package plugins

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import core.common.BiDiDictionary
import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}
import core.common.Util._
import org.semanticweb.yars.nx.parser.NxParser
import core.common.Pimps.pimpString

class SimpleGraph(triples: Iterable[(String, String, String)] = Nil) extends Serializable {
	val outEdges = mutable.Map[Int, mutable.Map[Int, ListBuffer[Int]]]()
	val vertices = new BiDiDictionary()
	val predicates = new BiDiDictionary()

	triples foreach (addEdge(_))

	def addEdge(triple: (String, String, String)) {
		addEdge(triple._1, triple._2, triple._3)
	}

	def addEdge(src: String, prd: String, tgt: String) {
		outEdges
		.getOrElseUpdate(vertices getOrElseUpdate src, mutable.Map.empty)
		.getOrElseUpdate(predicates getOrElseUpdate prd, ListBuffer.empty)
		.append(vertices getOrElseUpdate tgt)
	}

	def save(filename: String) {
		using(new FileOutputStream(filename))(fileOut => using(new ObjectOutputStream(fileOut))(_ writeObject this))
	}

	def contains(node: String, predicate: String) =
		vertices.stringToId.contains(node) &&
		predicates.stringToId.contains(predicate) &&
		outEdges.contains(vertices(node)) &&
		outEdges(vertices(node)).contains(predicates(predicate))


	def apply(node: String, predicates: String*): List[String] = query(List(node), predicates)

	def query(node: String, predicates: Seq[String]): List[String] = query(List(node), predicates)

	def query(nodes: List[String], predicates: Seq[String]): List[String] = {
		if (predicates.isEmpty)
			nodes
		else
			query(nodes flatMap outEdgesOf(_, predicates.head), predicates.tail)
	}

	def outEdgesOf(node: String, predicate: String): List[String] =
		if (contains(node, predicate))
			outEdges(vertices(node))(predicates(predicate)).map(vertices(_)).toList
		else Nil

	def outEdgesOf(node: String) = {
		val maybeNodeId = vertices.stringToId.get(node)
		maybeNodeId.toList.flatMap(outEdges(_) map {
			case (predId, targets) => (predicates(predId) -> targets.map(vertices(_)))
		})
	}
}

object SimpleGraph {
	def fromSerializedFile(filename: String) =
		using(new FileInputStream(filename))(in => using(new ObjectInputStream(in))(_.readObject().asInstanceOf[SimpleGraph]))

	private def normalize(string: String)(implicit substitutions: List[(String, String)] = Nil) =
		(substitutions foldLeft string) {
			case (res, (from, to)) => res.replaceAllLiterally(from, to)
		}

	def fromNT(filename: String)(implicit substitutions: List[(String, String)] = Nil, blacklistPattern: List[String] = Nil) = {
		val nxp = new NxParser(new FileInputStream(filename), false)
		val graph = new SimpleGraph()
		def isValid(s: String) = !s.hasSubstringFrom(blacklistPattern)
		while (nxp.hasNext)
			nxp.next() match {
				case Array(source, pred, target) if isValid(source.toString + pred.toString + target.toString) =>
					graph.addEdge(normalize(source.toString), normalize(pred.toString), normalize(target.toString))
				case _ => ()
			}
		graph
	}
}
