object q6 {

  /**
    * Returns a function that encapsulates all the operations given in param transform
    * @author Kevin Laframboise - 260687529
    * @param transform a String containing only the characters 'U', 'l', 'T', 'r', 's' or '*'
    * @return a function that encapsulates all the operations given in param transform
    */
  def stringPipeline(transform: String) = (s: String) => {

    /**
      * Parses the transform string in a List of functions to be executed in order
      * @param s transform string
      * @param functions partial list of functions to be applied in order
      * @return list of functions to be applied in order
      */
    def doStringPipeline(s: String, functions: List[String => String]): List[String => String] = {
      if (s isEmpty) functions  //return list once whole transform string has been processed
      else {
        s(0) match {  //add function corresponding to first char in the transform string to the list
          case 'U' => doStringPipeline(s.substring(1), functions :+(toUpperCase(_)))
          case 'l' => doStringPipeline(s.substring(1), functions :+(toLowerCase(_)))
          case 'T' => doStringPipeline(s.substring(1), functions :+(toTitleCase(_)))
          case 'r' => doStringPipeline(s.substring(1), functions :+(reverseString(_)))
          case 's' => doStringPipeline(s.substring(1), functions :+(sort(_)))
          case '*' => doStringPipeline(s.substring(1), functions :+(stripSpaces(_)))
          case _ => functions
        }
      }
    }

    /**
      * Applies toUpperCase to the passed string.
      * @param s
      * @return
      */
    def toUpperCase(s: String) : String = {
      s.toUpperCase
    }

    /**
      * Applies toLowerCase to the passed string.
      * @param s
      * @return
      */
    def toLowerCase(s: String) : String = {
      s.toLowerCase()
    }

    /**
      * Converts the first letter of each word in the string to upper case.
      * @param s
      * @return
      */
    def toTitleCase(s: String) : String = {
      val words = s.split(' ')
      val titleWords = for(word <- words) yield word.capitalize
      titleWords.mkString(" ")
    }

    /**
      * Applies reverse to the passed string
      * @param s
      * @return
      */
    def reverseString(s: String) : String = {
      s.reverse
    }

    /**
      * Gets the sorted string
      * @param s
      * @return
      */
    def sort(s: String) : String = {
      s.sorted
    }

    /**
      * Removes the white spaces from the string
      * @param s
      * @return
      */
    def stripSpaces(s: String) : String = {
      s.replace(" ", "")
    }

    val functions = doStringPipeline(transform, List()) //get the list of functions to be applied

    /**
      * Applies the list of function to the string to be transformed
      * Is called recursively.
      * @param s string to be transformed
      * @param count counts the number of transformations applied
      * @param list list containing the transformation functions
      * @return transformed string
      */
    def applyList(s: String, count: Int, list: List[String => String]) : String = {
      if (count == list.size) s //once all transformation have been applied, return the transformed string
      else applyList(list(count)(s), count + 1, list) //apply the transformation and continue transforming
    }
    applyList(s, 0, functions)  //applies the list of transformation
  }

  def main(args: Array[String]): Unit = {
    val x = stringPipeline("Tsl")
    println(x(" abc def ghi x !  1"))
  }
}