package utils

import java.util.Scanner

/**
 * @param str              The base string
 * @param desiredNumDigits The number of digits in the resulting string
 * @return A binary string of length desiredNumDigits
 */
def padBinaryString(str: String, desiredNumDigits: Int) = ("0" * (desiredNumDigits - str.length)) + str


/**
 * Asks the user through the command line for an integer
 * @return the int
 */
def getIntegerInput: Int = {
  val inputScanner = new Scanner(System.in)

  def getInput(scanner: Scanner, validResult: Boolean, num: Int): Int = {
    if (validResult) {
      num
    }
    else {
      try {
        val input = inputScanner.nextInt()
        getInput(scanner, true, input)
      } catch {
        case e => {
          inputScanner.nextLine()
          println("Invalid Integer")
          getInput(scanner, false, num)
        }
      }
    }
  }

  val outputNum = getInput(inputScanner, false, 0)
  outputNum
}