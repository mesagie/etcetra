class FuzzySearch(document: String) {
  private def tokenize(s: String) = s.toLowerCase.split("[^\\w]+")

  private val tokenized = tokenize(document).zipWithIndex
  private val index = tokenized.groupBy(_._1).mapValues(_.map(_._2).sorted.toList)

  private def containsExact(query: String) = search(query) == 1

  private val threshold = 0.4

  private val maxQueryLength = 500

  def containsFuzzy(query: String) = search(query).exists(_ >= threshold)

  def search(query: String) = {
    if (query.size > maxQueryLength) Nil
    else {
      val queryTokens = tokenize(query)
      if (queryTokens.size == 1)
        List(if (index.contains(queryTokens.head)) 1.0 else 0.0)
      else {
        def combinationScore(positions: Seq[Int]) = positions.sliding(2).map(x => 1.0 / (x.last - x.head)).sum / (queryTokens.size - 1)

        val indices = queryTokens.flatMap(index.get).flatten.distinct.sorted
        indices.view.map {
          ind =>
            val localIndices = tokenized.slice(ind - query.size - 1, ind + query.size + 1).collect {
              case (token, pos) if queryTokens contains token => pos
            }
            if (localIndices.size == 1) 1.0 / queryTokens.size
            else combinationScore(localIndices)
        }
      }
    }
  }
}
