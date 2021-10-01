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
      id           SERIAL    NOT NULL,
      name         TEXT      NOT NULL,
      parentId     INTEGER   REFERENCES Dog, /* might be unknown */
      lastModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY(id)
    )
  """)
  case class Dog()

  val dog1 = Dog(1)
  val dog2 = Dog(2)
  val dog3 = Dog(3)


  @jp.ken1ma.SqlClass.SqlTable("""
    CREATE TABLE Cat(
    )
  """)
  case class Cat(id: Int)
