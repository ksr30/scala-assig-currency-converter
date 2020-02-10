package com.knoldus

class CurrencyConverter {


  def convert(currentValue: Double, currentState: String, futureState: String): Double = {
    if (currentValue >= 0) {
      val tempInrValue = currencyHandler(currentValue, currentState, "from")
      val finalStateValue = currencyHandler(tempInrValue, futureState, "to")
      finalStateValue
    }
    else {
      throw new Exception("invalid value")
    }

  }

  private def currencyHandler(value: Double, state: String, conversionCase: String): Double = {
    state match {
      case "dollar" | "euro" | "canadianDollar" | "swissFranc" | "cubanPeso" => westernCurrency(value, state, conversionCase)
      case "inr" | "dirham" | "afghani" | "yuan" | "yen" => easternCurrency(value, state, conversionCase)
      case _ => throw new Exception("invalid currency")
    }
  }

  private def westernCurrency(value: Double, state: String, conversionCase: String): Double = {
    state match {
      case "dollar" => dollar(value, conversionCase)
      case "euro" => euro(value, conversionCase)
      case "canadianDollar" => canadianDollar(value, conversionCase)
      case "swissFranc" => swissFranc(value, conversionCase)
      case "cubanPeso" => cubanPeso(value, conversionCase)
    }
  }

  private def easternCurrency(value: Double, state: String, conversionCase: String): Double = {
    state match {
      case "inr" => inr(value, conversionCase)
      case "dirham" => dirham(value, conversionCase)
      case "afghani" => afghani(value, conversionCase)
      case "yuan" => yuan(value, conversionCase)
      case "yen" => yen(value, conversionCase)
    }
  }

  private def dollar(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 70 else value / 70

  private def inr(value: Double, conversionCase: String): Double = if (conversionCase == "from") value else value

  private def dirham(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 20 else value / 20

  private def afghani(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 0.93 else value / 0.93

  private def euro(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 78.44 else value / 78.44

  private def yuan(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 10.26 else value / 10.26

  private def canadianDollar(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 54.07 else value / 54.07

  private def swissFranc(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 73.16 else value / 73.16

  private def cubanPeso(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 71.20 else value / 71.20

  private def yen(value: Double, conversionCase: String): Double = if (conversionCase == "from") value * 0.65 else value / 0.65
}















