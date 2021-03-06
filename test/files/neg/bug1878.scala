object Test extends Application {
  // illegal 
  val err1 = "" match { case Seq(f @ _*, ',') => f }
  
  // no error
  val List(List(arg1, _*), _) = List(List(1,2,3), List(4,5,6))
  
  // illegal
  val List(List(_*, arg2), _) = List(List(1,2,3), List(4,5,6))
  
  // illegal - bug #1764
  null match {
    case <p> { _* } </p> =>
  }
}
