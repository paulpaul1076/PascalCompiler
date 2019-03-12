package frontend

import frontend.pascal.{PascalParserTD, PascalScanner}

/**
  * A factory class that creates parsers for specific source languages.
  */
object FrontendFactory {
  /**
    * Create a parser
    *
    * @param language   source language name (e.g. Pascal).
    * @param parserType the type of the parser (e.g. top-down).
    * @param source     the source object.
    * @return the parser.
    */
  def createParser(language: String, parserType: String, source: Source): Parser = {
    if (language.equalsIgnoreCase("Pascal") &&
      parserType.equalsIgnoreCase("top-down")) {
      new PascalParserTD(new PascalScanner(source))
    } else if (!language.equalsIgnoreCase("Pascal")) {
      throw new Exception("Parser factory: Invalid language '" + language + "'")
    } else {
      throw new Exception("Parser factory: Invalid type '" + "'")
    }
  }
}
