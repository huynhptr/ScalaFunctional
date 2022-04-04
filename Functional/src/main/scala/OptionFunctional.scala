import java.util.regex.{Pattern, PatternSyntaxException}

object OptionFunctional {
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(_a => b.map(_b => f(_a,_b)))
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((m1, m2) => m1(s) && m2(s))
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val list = for {
      op <- a
      if op != None
    } yield(op.get)
    if (list.length == a.length) {
      Option(list)
    }
    else {
      None
    }
  }
}
