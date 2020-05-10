import cats.data.EitherT
import cats.effect.{IO, Resource}
import cats.implicits._

import scala.io.Source

object Ops {
  def readDockerSecret(key: String): EitherT[IO, String, String] = {
    val fileResource =
      Resource.fromAutoCloseable(IO(Source.fromFile(s"/run/secrets/$key")))
    fileResource.use(s => IO(s.mkString)).attemptT.leftMap(_.getMessage)
  }

  def readEnv(key: String): EitherT[IO, String, String] =
    EitherT(IO {
      System.getenv(key) match {
        case null  => Left(s"No key $key found")
        case value => Right(value)
      }
    })

  def tryFindKey(
    key: String,
    default: Option[String] = None
  ): EitherT[IO, String, String] = {
    val run = readDockerSecret(key).recoverWith(_ => readEnv(key))

    default match {
      case Some(d) => run.recover(_ => d)
      case None    => run
    }
  }
}
