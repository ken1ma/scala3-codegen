import java.time.Instant

import java.sql.SQLException

import cats.*
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.cps.* // async, await
import doobie.*
import doobie.syntax.all.*
import doobie.postgres.implicits.* // support Instant https://github.com/tpolecat/doobie/releases/tag/v0.12.0

import jp.ken1ma.SqlClass.SqlTable

import CpsHelper.given

object SqlClassDoobie:
  @SqlTable("""
    CREATE TABLE Dog(
      id           INTEGER   NOT NULL,
      name         TEXT      NOT NULL,
      parentId     INTEGER   REFERENCES Dog, /* might be unknown */
      lastModified TIMESTAMP NOT NULL DEFAULT current_timestamp,
      PRIMARY KEY(id)
    )
  """)
  case class Dog()

  val dog1 = Dog(1, "dog1")
  val dog2 = Dog(2, "dog2")
  val dog3 = Dog(3, "dog3")

/*
  @jp.ken1ma.SqlClass.SqlTable("""
    CREATE TABLE Cat(
    )
  """)
  case class Cat(id: Int)
*/
