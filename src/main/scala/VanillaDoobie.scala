import java.time.Instant

import java.sql.SQLException

import cats.*
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.cps.* // async, await
import doobie.*
import doobie.syntax.all.*
import doobie.postgres.implicits.* // support Instant https://github.com/tpolecat/doobie/releases/tag/v0.12.0

import CpsHelper.given

object VanillaDoobie:
   case class Dog(id: Int, name: String, parentId: Option[Int], lastModified: Instant)

   // TODO I want to derive a case class as we did in Scala 2.13 with macro annotation
   // maybe study https://github.com/liufengyun/scala3-plugin-example
   //@deriveFrom(Dog) case class DogReq(name, parentId)
   case class DogReq(name: String, parentId: Option[Int])

   val program: ConnectionIO[Unit] =
      async[ConnectionIO] { // TODO without the braces, compilation error occurs: Found: Unit, Required: doobie.free.connection.ConnectionIO[Unit]
         sql"""
            DROP TABLE IF EXISTS Dog
         """.update.run.await

         sql"""
            CREATE TABLE Dog(
               id           SERIAL    NOT NULL,
               name         TEXT      NOT NULL,
               parentId     INTEGER   REFERENCES Dog, /* might be unknown */
               lastModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
               PRIMARY KEY(id)
            )
         """.update.run.await

         // INSERT
         val dog1 = insert(DogReq("name1", None)).await
         println(s"INSERT: $dog1")

         // use finally
         try
            val dog2 = insert(DogReq("name2", Some(dog1.id))).await
            println(s"INSERT: $dog2")

         finally
            println("async: finally block executed")

         // use try, catch
         async[ConnectionIO] {
            try
               val dog3 = insert(DogReq("name3", Some(-1))).await // foreign key error

            catch
               case ex: SQLException =>
                  println(s"async: expected exception caught: $ex")
                  throw new ExpectedException(ex)

         }.handleError {
            case ex: ExpectedException => () // TODO the entire ConnectionIO still fails
         }.await

         // SELECT
         val dogs = sql"""
            SELECT id, name, parentId, lastModified FROM Dog
         """.query[Dog].to[List].await
         println(s"SELECT: count = ${dogs.size}\n\t${dogs.mkString("\n\t")}")
      }

   def insert(req: DogReq): ConnectionIO[Dog] = for
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

   yield Dog(gen._1, req.name, req.parentId, gen._2)

   class ExpectedException(cause: Throwable) extends Exception(cause)
