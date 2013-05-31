package plugins

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import core.common.BiDiDictionary
import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}
import core.common.Util._
import org.semanticweb.yars.nx.parser.NxParser

class SimpleGraph(triples: Iterable[(String, String, String)] = Nil) extends Serializable {
  val outEdges = mutable.Map[Int, mutable.Map[Int, ListBuffer[Int]]]()
	val vertices = new BiDiDictionary()
	val predicates = new BiDiDictionary()

	triples foreach (addEdge(_))

	def addEdge(triple: (String, String, String)) {
		addEdge(triple._1, triple._2, triple._3)
	}

	def addEdge(src: String, prd: String, tgt: String) {
		val (srcId, prdId, tgtId) = (vertices getOrElseUpdate src, predicates getOrElseUpdate prd, vertices getOrElseUpdate tgt)

		outEdges
		.getOrElseUpdate(srcId, mutable.Map.empty)
		.getOrElseUpdate(prdId, ListBuffer.empty)
		.append(tgtId)
	}

	def save(filename: String) {
		using(new FileOutputStream(filename))(fileOut => using(new ObjectOutputStream(fileOut))(_ writeObject this))
	}

	def outEdgesOf(node: String, predicate: String) = outEdges(vertices(node))(predicates(predicate)).map(vertices(_))

	def outEdgesOf(node: String) = outEdges(vertices(node)).map {
		case (predId, targets) => (predicates(predId) -> targets.map(vertices(_)))
	}
}

object SimpleGraph {
	def fromSerializedFile(filename: String) =
		using(new FileInputStream(filename))(fileIn => using(new ObjectInputStream(fileIn))(_.readObject().asInstanceOf[SimpleGraph]))

	private def normalize(string: String)(implicit substitutions: List[(String, String)] = Nil) =
		(substitutions foldLeft string) {
			case (res, (from, to)) => res.replaceAllLiterally(from, to)
		}

	def fromNT(filename: String)(implicit substitutions: List[(String, String)] = Nil) = {
		val nxp = new NxParser(new FileInputStream(filename), false)
		val graph = new SimpleGraph()
		while (nxp.hasNext)
			nxp.next() match {
				case Array(source, pred, target) => graph.addEdge(normalize(source.toString), normalize(pred.toString), normalize(target.toString))
			}
		graph
	}
}
