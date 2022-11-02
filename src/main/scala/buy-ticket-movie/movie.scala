case class Movie(id: Int, title: String)
case class ShowTime(id: Int, startTime: String, endTime: String)
case class Theathre(id: Int)
case class BuyTicket(movie: Movie, theathre: Theathre, showTime: ShowTime)