abstract class GameStatus(state:String) 
case class GameStatusWin() extends GameStatus("win") 
case class GameStatusLose() extends GameStatus("lose")
case class GameStatusPlaying() extends GameStatus("playing")

case class Hangman(
    answerWord: String,
    incorrectLetters: Set[Char],
    correctLetters: Set[Char],
    lives: Int,
    gameStatus: GameStatus
)

object Hangman {
  def apply(hangman: Hangman, letter :Char) : Hangman = ???
}


object HangmanGame {

  def main(args: Array[String]):Unit = {
    val hangman = gameSetUp()
    val hangman1 = play(hangman,readChar)
    
    showInitializeScreen(hangman)
  }

  def gameSetUp(): Hangman = {
    val answerWord = "hello"
    val maxLives = 6
    val incorrectLetters = Set[Char]()
    val correctLetters = Set[Char]()
    Hangman(answerWord, incorrectLetters, correctLetters, maxLives)
  }

   def play(hangman: Hangman, letter: Char): Hangman = {
    if (answerWord.contains(letter)) {
      val addedCorrectLetters = correctLetters + letter
      if (hangman.answerWord.forall(letter => hangmanStatus.correctLetters.contains(letter)))
        Hangman(answerWord, incorrectLetters, addedCorrectLetters , lives, GameStatusWin)
      else
        Hangman(answerWord, incorrectLetters, addedCorrectLetters , lives, GameStatusPlaying)

    } else {
      val livesRemain = lives - 1
      val addedIncorrectLetters = incorrectLetters + letter
      if (livesRemain == 0) 
        Hangman(answerWord, addedIncorrectLetters, correctLetters, livesRemain, GameStatusLose)
      else
        Hangman(answerWord, addedIncorrectLetters,correctLetters, livesRemain,GameStatusPlaying )
      }
  }

  def showInitializeScreen(hangman: Hangman): Unit = {
    println("-----------Hangman--------------\n")
    println(s"word: ${hangman.answerWord.map(_ => "_").mkString("")}")
    println(s"lives: ${hangman.lives}")
    println("-------------------------------")
  }

  def showUpdatedState(hangman: Hangman) = {
    println("-------------------------------\n")
    println(s"word: ${getAnswerWord(hangman)}")
    println(s"lives: ${hangman.lives}")

    if (!hangman.incorrectLetters.isEmpty)
      println(s"wrong characters: ${hangman.incorrectLetters.mkString(" ")}")
      
    println("-------------------------------")
  }

  def getAnswerWord(hangman: Hangman):String = {
  hangman.answerWord.map(w =>
      mapAnswerLettersWithMask(w, hangman.correctLetters) match {
        case Right(value) => value
        case Left(value)  => value
      }
    )
  }

  def mapAnswerLettersWithMask(
      letter: Char,
      correctLetters: Set[Char]
  ): Either[Char, Char] = {
    if (correctLetters.contains(letter)) Right(letter)
    else Left('_')
  }
}