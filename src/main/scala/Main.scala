import cats.effect.{IO, IOApp}
import doobie.Transactor
import doobie.implicits._

object Main extends IOApp.Simple:
   val run =
      val appConf = AppConf.load
      val xa = Transactor.fromDriverManager[IO](
         appConf.db.driver,
         appConf.db.url,
         appConf.db.user,
         appConf.db.password,
      )
      VanillaDoobie.program.transact(xa)
