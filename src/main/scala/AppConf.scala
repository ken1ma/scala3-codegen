/*
   pureconfig 0.16.0 doesn't seem to be ready for Scala 3
   https://github.com/pureconfig/pureconfig/issues/970

   I can't figure out how to compile those in
   https://github.com/pureconfig/pureconfig/tree/master/tests/src/test/scala-3/pureconfig
   nor which maven artifact has pureconfig.generic.derivation.default.derived
   https://github.com/pureconfig/pureconfig/blob/v0.16.0/core/src/main/scala-3/pureconfig/generic/derivation/ConfigReaderDerivation.scala#L24
*/

import AppConf.*
case class AppConf(
   db: Db
)

object AppConf:
   case class Db(driver: String, url: String, user: String, password: String)

   def load: AppConf =
      //ConfigSource.default.load[AppConf]
      AppConf(Db("org.postgresql.Driver", "jdbc:postgresql://localhost/test_db", "test_user", "test_pass"))
