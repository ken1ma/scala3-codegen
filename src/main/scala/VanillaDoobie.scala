import java.time.Instant

import doobie._
import doobie.syntax.all._
import doobie.postgres.implicits._ // https://github.com/tpolecat/doobie/releases/tag/v0.12.0
import cats._
import cats.syntax.all._
import cats.effect.IO

object VanillaDoobie:
   case class Dog(id: Int, name: String, parentId: Option[Int], lastModified: Instant)

   // TODO I want to derive a case class as we did in Scala 2.13 with macro annotation
   // maybe study https://github.com/liufengyun/scala3-plugin-example
   //@deriveFrom(Dog) case class DogReq(name, parentId)
   case class DogReq(name: String, parentId: Option[Int])

   def program: ConnectionIO[Unit] = for {
      _ <- sql"""
         DROP TABLE IF EXISTS Dog
      """.update.run

      _ <- sql"""
         CREATE TABLE Dog(
            id           SERIAL    NOT NULL,
            name         TEXT NOT  NULL,
            parentId     INTEGER   , /* might be unknown */
            lastModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY(id)
         )
      """.update.run

      dog1 <- insert(DogReq("name1", None))
      _ = println(s"INSERT: $dog1")

      dog2 <- insert(DogReq("name2", Some(dog1.id)))
      _ = println(s"INSERT: $dog2")

      dogs <- sql"""
         SELECT id, name, parentId, lastModified FROM Dog
      """.query[Dog].to[List]
      _ = println(s"SELECT: count = ${dogs.size}\n\t${dogs.mkString("\n\t")}")

   } yield ()

   def insert(req: DogReq): ConnectionIO[Dog] = for {
      /*
         doobie 1.0.0-RC1: the next block results in
            [E007] Type Mismatch Error:
               """.update.withUniqueGeneratedKeys[(Int, Instant)]("id", "lastModified").run(req)
                                                                                            ^^^
                   Found:    (req : VanillaDoobie.DogReq)
                   Required: cats.Comonad[doobie.free.connection.ConnectionOp]

      gen <- sql"""
         INSERT INTO Dog(name, parentId) VALUES(?, ?)
      """.update.withUniqueGeneratedKeys[(Int, Instant)]("id", "lastModified").run(req)
      */

      gen <- Update[DogReq]("""
         INSERT INTO Dog(name, parentId) VALUES(?, ?)
      """).withUniqueGeneratedKeys[(Int, Instant)]("id", "lastModified".toLowerCase)(req) // doobie 1.0.0-RC1: column namess will be quoted, so toLowerCase must be done as the PostgreSQL server does

   } yield Dog(gen._1, req.name, req.parentId, gen._2)
