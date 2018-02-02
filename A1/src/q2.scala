object q2 {
  def show(list: List[Any]): String = {
    def doShow(list: List[Any], result: String, size: Int) : String = {
      if(list.isEmpty) return "Nil"
      if(list.size == 1) closeList(result.concat(list.head.toString).concat("::Nil"), size)
      else doShow(list.tail, result.concat(list.head.toString).concat("::("), size)
    }
    def closeList(list: String, size: Int) : String = {
      if(size == 1) list
      else closeList(list.concat(")"), size - 1)
    }
    doShow(list, "", list.size)
  }
}