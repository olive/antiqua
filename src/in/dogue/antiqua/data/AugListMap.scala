package in.dogue.antiqua.data

class AugListMap[K,V](map:Map[K, List[V]]) {
  def *+*(other:Map[K, List[V]]) = {
    (for (k <- map.keys ++ other.keys) yield {
      val v1 = map.getOrElse(k, List())
      val v2 = other.getOrElse(k, List())
      k -> (v1 ++ v2)
    }).toMap.withDefaultValue(List())
  }
}
