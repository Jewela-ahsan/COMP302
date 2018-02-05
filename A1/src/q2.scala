object q2 {
  /**
    * Transforms a list into a string representation
    * @author Kevin Laframboise - 260687529
    * @param list list to be converted to a string
    * @return string representation of a list
    */
  def show(list: List[Any]): String = {

    /**
      * Helper method that builds the string recursively
      * @param list remainder of the list to be converted to a string
      * @param result result string
      * @param size size of the original list
      * @return string representation of the list
      */
    def doShow(list: List[Any], result: String, size: Int) : String = {
      if(list.isEmpty) return "Nil" // list = Nil
      if(list.size == 1) closeList(result.concat(list.head.toString).concat("::Nil"), size) // add the last element of the list + Nil and close the list
      else doShow(list.tail, result.concat(list.head.toString).concat("::("), size) // add the current element to the string and move to next item
    }

    /**
      * Closes the list by adding the closing parenthesis
      * @param list string representation of the list the close (without closing parenthesis)
      * @param size number of element in the list
      * @return the closed representation of the list
      */
    def closeList(list: String, size: Int) : String = {
      if(size == 1) list  // done closing, return list
      else closeList(list.concat(")"), size - 1)  //recursively add closing parenthesis to string
    }

    doShow(list, "", list.size)
  }
}